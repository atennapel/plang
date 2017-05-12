var __extends = (this && this.__extends) || (function () {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
function union(a, b) {
    var r = a.slice();
    for (var i = 0, l = b.length; i < l; i++)
        if (r.indexOf(b[i]) < 0)
            r.push(b[i]);
    return r;
}
function remove(a, b) {
    var i = a.indexOf(b);
    if (i < 0)
        return a;
    var r = a.slice();
    r.splice(i, 1);
    return r;
}
function findVar(env, name) {
    if (env[name])
        return env[name];
    throw new TypeError('undefined var: ' + name);
}
function extend(env, name, expr) {
    var n = {};
    for (var k in env)
        n[k] = env[k];
    n[name] = expr;
    return n;
}
var Expr = (function () {
    function Expr() {
    }
    Expr.prototype.alphaEq = function (other) {
        return false;
    };
    Expr.prototype.substVar = function (vr, expr) {
        return this.subst(vr, new Var(expr));
    };
    Expr.prototype.betaEq = function (other) {
        return this.nf([]).alphaEq(other.nf([]));
    };
    Expr.prototype.reduce = function () {
        return this.nf([]);
    };
    Expr.prototype.checkReduce = function (env) {
        return this.check(env).whnf([]);
    };
    return Expr;
}());
var Var = (function (_super) {
    __extends(Var, _super);
    function Var(id) {
        var _this = _super.call(this) || this;
        _this.id = id;
        return _this;
    }
    Var.prototype.toString = function () {
        return "" + this.id;
    };
    Var.prototype.freeVars = function () {
        return [this.id];
    };
    Var.prototype.subst = function (vr, expr) {
        return this.id === vr ? expr : this;
    };
    Var.prototype.alphaEq = function (other) {
        return other instanceof Var && this.id === other.id;
    };
    Var.prototype.whnf = function (acc) {
        return acc.reduce(function (x, y) { return new App(x, y); }, this);
    };
    Var.prototype.nf = function (acc) {
        return acc.map(function (x) { return x.nf([]); }).reduce(function (x, y) { return new App(x, y); }, this);
    };
    Var.prototype.check = function (env) {
        return findVar(env, this.id);
    };
    return Var;
}(Expr));
var App = (function (_super) {
    __extends(App, _super);
    function App(left, right) {
        var _this = _super.call(this) || this;
        _this.left = left;
        _this.right = right;
        return _this;
    }
    App.prototype.toString = function () {
        return "(" + this.left + " " + this.right + ")";
    };
    App.prototype.freeVars = function () {
        return union(this.left.freeVars(), this.right.freeVars());
    };
    App.prototype.subst = function (vr, expr) {
        return new App(this.left.subst(vr, expr), this.right.subst(vr, expr));
    };
    App.prototype.alphaEq = function (other) {
        return other instanceof App &&
            this.left.alphaEq(other.left) && this.right.alphaEq(other.right);
    };
    App.prototype.whnf = function (acc) {
        return this.left.whnf([this.right].concat(acc));
    };
    App.prototype.nf = function (acc) {
        return this.left.nf([this.right].concat(acc));
    };
    App.prototype.check = function (env) {
        var tf = this.left.checkReduce(env);
        if (!(tf instanceof Pi))
            throw new TypeError("non-function in application: " + tf);
        var ta = this.right.check(env);
        if (!ta.betaEq(tf.type))
            throw new TypeError("bad function argument type: " + ta);
        return tf.body.subst(tf.arg, this.right);
    };
    return App;
}(Expr));
var Lam = (function (_super) {
    __extends(Lam, _super);
    function Lam(arg, type, body) {
        var _this = _super.call(this) || this;
        _this.arg = arg;
        _this.type = type;
        _this.body = body;
        return _this;
    }
    Lam.prototype.toString = function () {
        return "(\\" + this.arg + ":" + this.type + "." + this.body + ")";
    };
    Lam.prototype.freeVars = function () {
        return union(this.type.freeVars(), remove(this.body.freeVars(), this.arg));
    };
    Lam.prototype.subst = function (vr, expr) {
        if (vr === this.arg)
            return new Lam(this.arg, this.type.subst(vr, expr), this.body);
        var exprFree = expr.freeVars();
        if (exprFree.indexOf(this.arg) < 0)
            return new Lam(this.arg, this.type.subst(this.arg, this.body), this.body.subst(this.arg, this.body));
        var free = union(exprFree, this.body.freeVars());
        var v = this.arg;
        while (free.indexOf(v) >= 0)
            v = v + "'";
        return new Lam(v, this.type.subst(vr, expr), this.body.substVar(this.arg, v).subst(vr, expr));
    };
    Lam.prototype.alphaEq = function (other) {
        return other instanceof Lam &&
            this.type.alphaEq(other.type) &&
            this.body.alphaEq(other.body.substVar(other.arg, this.arg));
    };
    Lam.prototype.whnf = function (acc) {
        return acc.length === 0 ?
            acc.reduce(function (x, y) { return new App(x, y); }, this) :
            this.body.subst(this.arg, acc[0]).whnf(acc.slice(1));
    };
    Lam.prototype.nf = function (acc) {
        return acc.length === 0 ?
            new Lam(this.arg, this.type.nf([]), this.body.nf([])) :
            this.body.subst(this.arg, acc[0]).nf(acc.slice(1));
    };
    Lam.prototype.check = function (env) {
        this.type.check(env);
        var nenv = extend(env, this.arg, this.type);
        var te = this.body.check(nenv);
        var lt = new Pi(this.arg, this.type, te);
        lt.check(env);
        return lt;
    };
    return Lam;
}(Expr));
var Pi = (function (_super) {
    __extends(Pi, _super);
    function Pi(arg, type, body) {
        var _this = _super.call(this) || this;
        _this.arg = arg;
        _this.type = type;
        _this.body = body;
        return _this;
    }
    Pi.prototype.toString = function () {
        return "(" + this.arg + ":" + this.type + ")->" + this.body;
    };
    Pi.prototype.freeVars = function () {
        return union(this.type.freeVars(), remove(this.body.freeVars(), this.arg));
    };
    Pi.prototype.subst = function (vr, expr) {
        if (vr === this.arg)
            return new Pi(this.arg, this.type.subst(vr, expr), this.body);
        var exprFree = expr.freeVars();
        if (exprFree.indexOf(this.arg) < 0)
            return new Pi(this.arg, this.type.subst(this.arg, this.body), this.body.subst(this.arg, this.body));
        var free = union(exprFree, this.body.freeVars());
        var v = this.arg;
        while (free.indexOf(v) >= 0)
            v = v + "'";
        return new Pi(v, this.type.subst(vr, expr), this.body.substVar(this.arg, v).subst(vr, expr));
    };
    Pi.prototype.whnf = function (acc) {
        return acc.length === 0 ?
            acc.reduce(function (x, y) { return new App(x, y); }, this) :
            this.body.subst(this.arg, acc[0]).whnf(acc.slice(1));
    };
    Pi.prototype.nf = function (acc) {
        return acc.map(function (x) { return x.nf([]); }).reduce(function (x, y) { return new App(x, y); }, new Pi(this.arg, this.type.nf([]), this.body.nf([])));
    };
    Pi.prototype.alphaEq = function (other) {
        return other instanceof Lam &&
            this.type.alphaEq(other.type) &&
            this.body.alphaEq(other.body.substVar(other.arg, this.arg));
    };
    Pi.prototype.check = function (env) {
        var s = this.type.checkReduce(env);
        var nenv = extend(env, this.arg, this.type);
        var t = this.body.checkReduce(nenv);
        return t;
    };
    return Pi;
}(Expr));
var Kinds;
(function (Kinds) {
    Kinds[Kinds["Star"] = 0] = "Star";
    Kinds[Kinds["Box"] = 1] = "Box";
})(Kinds || (Kinds = {}));
var Kind = (function (_super) {
    __extends(Kind, _super);
    function Kind(kind) {
        var _this = _super.call(this) || this;
        _this.kind = kind;
        return _this;
    }
    Kind.prototype.toString = function () {
        return this.kind === Kinds.Star ? 'Star' : 'Box';
    };
    Kind.prototype.freeVars = function () {
        return [];
    };
    Kind.prototype.subst = function (vr, expr) {
        return this;
    };
    Kind.prototype.whnf = function (acc) {
        return acc.reduce(function (x, y) { return new App(x, y); }, this);
    };
    Kind.prototype.nf = function (acc) {
        return acc.map(function (x) { return x.nf([]); }).reduce(function (x, y) { return new App(x, y); }, this);
    };
    Kind.prototype.alphaEq = function (other) {
        return other instanceof Kind && this.kind === other.kind;
    };
    Kind.prototype.check = function (env) {
        if (this.kind === Kinds.Star)
            return new Kind(Kinds.Box);
        throw new TypeError('Found Box');
    };
    return Kind;
}(Expr));
Kind.Star = new Kind(Kinds.Star);
Kind.Box = new Kind(Kinds.Box);
var id = new Lam('a', Kind.Star, new Lam('x', new Var('a'), new Var('x')));
console.log('' + id);
console.log('' + id.check({}));
var app = new App(new App(id, new Pi('a', Kind.Star, new Pi('x', new Var('a'), new Var('a')))), id);
console.log('' + app);
console.log('' + app.check({}));

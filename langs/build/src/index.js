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
function subst(tm, x, t) {
    return tm.close(x).open(t);
}
function betaEquals(tm, t) {
    return tm.normalize().equals(t.normalize());
}
var Term = (function () {
    function Term() {
    }
    return Term;
}());
var Bound = (function (_super) {
    __extends(Bound, _super);
    function Bound(index) {
        var _this = _super.call(this) || this;
        _this.index = index;
        return _this;
    }
    Bound.prototype.toString = function () {
        return "'" + this.index;
    };
    Bound.prototype.open = function (u, k) {
        if (k === void 0) { k = 0; }
        return this.index === k ? u : this;
    };
    Bound.prototype.close = function (x, k) {
        if (k === void 0) { k = 0; }
        return this;
    };
    Bound.prototype.equals = function (t) {
        return t instanceof Bound && this.index === t.index;
    };
    Bound.prototype.normalize = function () {
        return this;
    };
    return Bound;
}(Term));
function bvar(index) { return new Bound(index); }
var Free = (function (_super) {
    __extends(Free, _super);
    function Free(name) {
        var _this = _super.call(this) || this;
        _this.name = name;
        return _this;
    }
    Free.prototype.toString = function () {
        return this.name;
    };
    Free.prototype.open = function (u, k) {
        if (k === void 0) { k = 0; }
        return this;
    };
    Free.prototype.close = function (x, k) {
        if (k === void 0) { k = 0; }
        return this.name === x ? new Bound(k) : this;
    };
    Free.prototype.equals = function (t) {
        return t instanceof Free && this.name === t.name;
    };
    Free.prototype.normalize = function () {
        return this;
    };
    return Free;
}(Term));
function fvar(name) { return new Free(name); }
var varI = 0;
function fresh() { return "$" + varI++; }
var Abs = (function (_super) {
    __extends(Abs, _super);
    function Abs(term) {
        var _this = _super.call(this) || this;
        _this.term = term;
        return _this;
    }
    Abs.prototype.toString = function () {
        return "\\" + this.term;
    };
    Abs.prototype.open = function (u, k) {
        if (k === void 0) { k = 0; }
        return new Abs(this.term.open(u, k + 1));
    };
    Abs.prototype.close = function (x, k) {
        if (k === void 0) { k = 0; }
        return new Abs(this.term.close(x, k + 1));
    };
    Abs.prototype.equals = function (t) {
        return t instanceof Abs && this.term.equals(t.term);
    };
    Abs.prototype.normalize = function () {
        var f = fresh();
        return new Abs(this.term.open(new Free(f)).normalize().close(f));
    };
    return Abs;
}(Term));
function abs(term) { return new Abs(term); }
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
    App.prototype.open = function (u, k) {
        if (k === void 0) { k = 0; }
        return new App(this.left.open(u, k), this.right.open(u, k));
    };
    App.prototype.close = function (x, k) {
        if (k === void 0) { k = 0; }
        return new App(this.left.close(x, k), this.right.close(x, k));
    };
    App.prototype.equals = function (t) {
        return t instanceof App &&
            this.left.equals(t.left) &&
            this.right.equals(t.right);
    };
    App.prototype.normalize = function () {
        var l = this.left.normalize();
        var r = this.right.normalize();
        if (l instanceof Abs)
            return l.term.open(r).normalize();
        return new App(l, r);
    };
    return App;
}(Term));
function app() {
    var ts = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        ts[_i] = arguments[_i];
    }
    return ts.reduce(function (x, y) { return new App(x, y); });
}
var B = bvar;
var F = fvar;
var L = abs;
var A = app;
var id = L(B(0));
var z = L(L(B(0)));
var s = L(L(L(A(B(1), A(B(2), B(1), B(0))))));
var plus = L(L(L(L(A(B(3), B(1), A(B(2), B(1), B(0)))))));
var mult = L(L(L(A(B(2), A(B(1), B(0))))));
var exp = L(L(A(B(0), B(1))));
var pred = L(L(L(A(B(2), L(L(A(B(0), A(B(1), B(3))))), L(B(1)), L(B(0))))));
var sub = L(L(A(A(B(0), pred), B(1))));
var _0 = z;
var _1 = A(s, _0);
var _2 = A(s, _1);
var _3 = A(s, _2);
var _4 = A(s, _3);
var _5 = A(s, _4);
var e = A(exp, _2, _2);
console.log('' + e);
console.log('' + e.normalize());

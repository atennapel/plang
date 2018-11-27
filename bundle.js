(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Either {
    static err(err) {
        return new Left(err);
    }
    static of(val) {
        return new Right(val);
    }
}
exports.default = Either;
class Left extends Either {
    constructor(error) {
        super();
        this.error = error;
    }
    toString() {
        return `Left(${this.error})`;
    }
    isError() {
        return true;
    }
    map(fn) {
        return this;
    }
    chain(fn) {
        return this;
    }
    throw() {
        throw this.error;
    }
}
exports.Left = Left;
class Right extends Either {
    constructor(val) {
        super();
        this.val = val;
    }
    toString() {
        return `Right(${this.val})`;
    }
    isError() {
        return false;
    }
    map(fn) {
        return new Right(fn(this.val));
    }
    chain(fn) {
        return fn(this.val);
    }
    throw() {
        return this.val;
    }
}
exports.Right = Right;

},{}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class NameRep {
}
exports.default = NameRep;
class Name extends NameRep {
    constructor(name) {
        super();
        this.name = name;
    }
    next() {
        return new Gen(this.name, 0);
    }
    withIndex(index) {
        return new Gen(this.name, index);
    }
    equals(other) {
        return other instanceof Name && other.name === this.name;
    }
    toString() {
        return this.name;
    }
}
exports.Name = Name;
exports.name = (name) => new Name(name);
class Gen extends NameRep {
    constructor(name, index) {
        super();
        this.name = name;
        this.index = index;
    }
    next() {
        return new Gen(this.name, this.index + 1);
    }
    withIndex(index) {
        return new Gen(this.name, index);
    }
    equals(other) {
        return other instanceof Gen && other.name === this.name && other.index === this.index;
    }
    toString() {
        return `${this.name}\$${this.index}`;
    }
}
exports.Gen = Gen;
exports.gen = (name, index) => new Gen(name, index);

},{}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const NameRep_1 = require("./NameRep");
class NameRepSupply {
    constructor(index) {
        this.index = index;
    }
    fresh(name) {
        return { name: new NameRep_1.Gen(name.name, this.index), supply: new NameRepSupply(this.index + 1) };
    }
}
exports.default = NameRepSupply;

},{"./NameRep":2}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.impossible = () => { throw new Error('impossible'); };
exports.assocGet = (arr, val) => {
    for (let i = arr.length - 1; i >= 0; i--) {
        if (arr[i][0].equals(val))
            return arr[i][1];
    }
    return null;
};
exports.containsDuplicate = (arr) => {
    const acc = [];
    for (let i = 0; i < arr.length; i++) {
        const c = arr[i];
        for (let j = 0; j < acc.length; j++) {
            if (acc[j].equals(c))
                return true;
        }
        acc.push(c);
    }
    return false;
};

},{}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const Either_1 = require("../Either");
const context_1 = require("./context");
const NameRep_1 = require("../NameRep");
const elems_1 = require("./elems");
const types_1 = require("./types");
const utils_1 = require("../utils");
class TC {
    constructor(run) {
        this.run = run;
    }
    static of(val) {
        return new TC((ctx, supply) => ({ ctx, supply, val: Either_1.default.of(val) }));
    }
    static void() {
        return TC.of(undefined);
    }
    static error(err) {
        return new TC((ctx, supply) => ({ ctx, supply, val: Either_1.default.err(err) }));
    }
    map(fn) {
        return new TC((ctx, supply) => {
            const ret = this.run(ctx, supply);
            return { ctx: ret.ctx, supply: ret.supply, val: ret.val.map(fn) };
        });
    }
    chain(fn) {
        return new TC((ctx, supply) => {
            const ret = this.run(ctx, supply);
            if (ret.val.isError())
                return ret;
            return fn(ret.val.val).run(ret.ctx, ret.supply);
        });
    }
    chain2(fn, that) {
        return this.chain(a => that.chain(b => fn(a, b)));
    }
    chain3(fn, that, that2) {
        return this.chain(a => that.chain(b => that2.chain(c => fn(a, b, c))));
    }
    ap(fn) {
        return this.chain(val => fn.map(fn => fn(val)));
    }
    then(that) {
        return this.chain(() => that);
    }
    void() {
        return this.map(() => undefined);
    }
    catch(fn) {
        return new TC((ctx, supply) => {
            const ret = this.run(ctx, supply);
            if (ret.val.isError())
                return fn(ret.val.error).run(ctx, supply);
            return ret;
        });
    }
    static if(c, a, b) {
        return c.chain(cb => cb ? a : b);
    }
    static check(c, msg) {
        return c ? TC.void() : TC.error(msg);
    }
    checkIs(fn, msg) {
        return this.chain(x => TC.check(fn(x), msg(x)).map(() => x));
    }
    fail(msg) {
        return this.chain(() => TC.error(msg));
    }
    // context
    static getContext() {
        return new TC((ctx, supply) => ({ ctx, supply, val: Either_1.default.of(ctx) }));
    }
    static getNameSupply() {
        return new TC((ctx, supply) => ({ ctx, supply, val: Either_1.default.of(supply) }));
    }
    static putContext(ctx) {
        return new TC((_, supply) => ({ ctx, supply, val: Either_1.default.of(undefined) }));
    }
    static putNameSupply(supply) {
        return new TC((ctx, _) => ({ ctx, supply, val: Either_1.default.of(undefined) }));
    }
    static updateContext(fn) {
        return TC.getContext().chain(ctx => TC.putContext(fn(ctx)));
    }
    static updateNameSupply(fn) {
        return TC.getNameSupply().chain(supply => TC.putNameSupply(fn(supply)));
    }
    // fresh
    static freshName(val) {
        return TC.getNameSupply().chain(sup => {
            const { name, supply } = sup.fresh(val);
            return TC.putNameSupply(supply).map(() => name);
        });
    }
    static freshNames(ns) {
        return ns.reduce((c, n) => c.chain(a => TC.freshName(n).map(x => a.concat([x]))), TC.of([]));
    }
    // context
    static pop(fn) {
        return TC.getContext().chain(ctx => {
            const [left, right] = ctx.split(fn);
            return TC.putContext(left).map(() => right);
        });
    }
    static replace(fn, es) {
        return TC.updateContext(ctx => ctx.replace(fn, es));
    }
}
exports.default = TC;
exports.pure = (val) => TC.of(val);
exports.error = (err) => TC.error(err);
exports.ok = exports.pure(undefined);
exports.log = (msg) => exports.getCtx.map(ctx => { console.log(`${msg}`); return undefined; });
exports.iff = (c, a, b) => TC.if(c, a, b);
exports.check = (c, msg) => TC.check(c, msg);
exports.getCtx = TC.getContext();
exports.updateCtx = TC.updateContext;
exports.freshName = (val) => TC.freshName(val);
exports.freshNames = (ns) => TC.freshNames(ns);
exports.pop = (fn) => TC.pop(fn);
exports.replace = (fn, es) => TC.replace(fn, es);
exports.withElems = (es, action) => exports.freshName(NameRep_1.name('wm'))
    .chain(name => exports.updateCtx(context_1.default.addAll([elems_1.cmarker(name)].concat(es)))
    .then(action)
    .chain(val => exports.pop(elems_1.isCMarker(name))
    .map(() => val)));
exports.freshWithElems = (val, es, action) => exports.freshNames(val).chain(ns => exports.withElems(es(ns), action(ns)));
exports.ordered = (a, b) => exports.getCtx.map(ctx => ctx.ordered(elems_1.isCTMeta(a), elems_1.isCTMeta(b)));
exports.find = (fn, then, other) => exports.getCtx.chain(ctx => ctx.find(fn, then, other));
exports.findKVar = (name) => exports.find(elems_1.isCKVar(name), e => exports.pure(e), () => exports.error(`kvar ${name} not found`));
exports.findTVar = (name) => exports.find(elems_1.isCTVar(name), e => exports.pure(e), () => exports.error(`tvar ${name} not found`));
exports.findTMeta = (name) => exports.find(elems_1.isCTMeta(name), e => exports.pure(e), () => exports.error(`tmeta ${name} not found`));
exports.findMarker = (name) => exports.find(elems_1.isCMarker(name), e => exports.pure(e), () => exports.error(`marker ${name} not found`));
exports.findVar = (name) => exports.find(elems_1.isCVar(name), e => exports.pure(e), () => exports.error(`var ${name} not found`));
exports.apply = (type) => {
    if (types_1.isTVar(type))
        return exports.pure(type);
    if (types_1.isTMeta(type))
        return exports.getCtx.chain(ctx => ctx.find(elems_1.isCTMeta(type.name), e => e.type ? exports.apply(e.type) : exports.pure(type), () => exports.pure(type)));
    if (types_1.isTApp(type))
        return exports.apply(type.left)
            .chain(left => exports.apply(type.right)
            .map(right => types_1.tapp(left, right)));
    if (types_1.isTFun(type))
        return exports.apply(type.left)
            .chain(left => exports.apply(type.right)
            .map(right => types_1.tfun(left, right)));
    if (types_1.isTComp(type))
        return exports.apply(type.type)
            .chain(left => exports.apply(type.eff)
            .map(right => types_1.tcomp(left, right)));
    if (types_1.isTForall(type))
        return exports.apply(type.type).map(body => types_1.tforall(type.name, type.kind, body));
    if (types_1.isTEffsExtend(type))
        return exports.apply(type.type)
            .chain(left => exports.apply(type.rest)
            .map(right => types_1.teffsextend(left, right)));
    if (types_1.isTEffsEmpty(type))
        return exports.pure(type);
    return utils_1.impossible();
};

},{"../Either":1,"../NameRep":2,"../utils":25,"./context":7,"./elems":8,"./types":15}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
class Comp extends exprs_1.default {
    constructor() {
        super(...arguments);
        this._type = 'Comp';
    }
}
exports.default = Comp;
class Return extends Comp {
    constructor(val) {
        super();
        this.val = val;
    }
    toString() {
        return `(return ${this.val})`;
    }
    subst(name, val) {
        return new Return(this.val.subst(name, val));
    }
    substTVar(name, type) {
        return new Return(this.val.substTVar(name, type));
    }
}
exports.Return = Return;
exports.ret = (val) => new Return(val);
exports.isReturn = (expr) => expr instanceof Return;
class App extends Comp {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} ${this.right})`;
    }
    subst(name, val) {
        return new App(this.left.subst(name, val), this.right.subst(name, val));
    }
    substTVar(name, type) {
        return new App(this.left.substTVar(name, type), this.right.substTVar(name, type));
    }
}
exports.App = App;
exports.app = (left, right) => new App(left, right);
// export const appFrom = (es: Val[]) => es.reduce(app);
// export function apps(...es: Val[]) { return appFrom(es) }
exports.isApp = (expr) => expr instanceof App;
class AppT extends Comp {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} @${this.right})`;
    }
    subst(name, val) {
        return new AppT(this.left.subst(name, val), this.right);
    }
    substTVar(name, type) {
        return new AppT(this.left.substTVar(name, type), this.right.substTVar(name, type));
    }
}
exports.AppT = AppT;
exports.appT = (left, right) => new AppT(left, right);
// export const appTs = (left: Expr, ts: Type[]) => ts.reduce(appT, left);
exports.isAppT = (expr) => expr instanceof AppT;
class Let extends Comp {
    constructor(name, expr, body) {
        super();
        this.name = name;
        this.expr = expr;
        this.body = body;
    }
    toString() {
        return `(let ${this.name} = ${this.expr} in ${this.body})`;
    }
    subst(name, val) {
        return this.name.equals(name) ?
            new Let(this.name, this.expr.subst(name, val), this.body) :
            new Let(this.name, this.expr.subst(name, val), this.body.subst(name, val));
    }
    open(val) {
        return this.body.subst(this.name, val);
    }
    substTVar(name, type) {
        return new Let(this.name, this.expr.substTVar(name, type), this.body.substTVar(name, type));
    }
}
exports.Let = Let;
exports.lt = (name, expr, body) => new Let(name, expr, body);
exports.lts = (ns, body) => ns.reduceRight((b, [n, e]) => exports.lt(n, e, b), body);
exports.isLet = (expr) => expr instanceof Let;

},{"./exprs":9}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Context {
    constructor(elems) {
        this.elems = elems;
    }
    static empty() {
        return new Context([]);
    }
    static of(...es) {
        return new Context(es);
    }
    static from(es) {
        return new Context(es);
    }
    toString() {
        return `[${this.elems.join(', ')}]`;
    }
    find(fn, then, other) {
        const a = this.elems;
        for (let i = a.length - 1; i >= 0; i--) {
            if (fn(a[i]))
                return then(a[i]);
        }
        return other();
    }
    findAll(fn) {
        const ret = [];
        const a = this.elems;
        for (let i = a.length - 1; i >= 0; i--) {
            const c = fn(a[i]);
            if (c !== null)
                ret.push(c);
        }
        return ret;
    }
    add(...es) {
        return new Context(this.elems.concat(es));
    }
    addAll(es) {
        return new Context(this.elems.concat(es));
    }
    static addAll(es) {
        return (ctx) => ctx.addAll(es);
    }
    static add(...es) {
        return (ctx) => ctx.addAll(es);
    }
    append(that) {
        return new Context(this.elems.concat(that.elems));
    }
    static append(that) {
        return ctx => ctx.append(that);
    }
    split(fn) {
        const a = this.elems;
        for (let i = a.length - 1; i >= 0; i--) {
            const c = a[i];
            if (fn(c)) {
                return [new Context(this.elems.slice(0, i)), new Context(this.elems.slice(i + 1))];
            }
        }
        return [this, Context.empty()];
    }
    replace(fn, es) {
        const ret = this.split(fn);
        return ret[0].append(Context.from(es)).append(ret[1]);
    }
    ordered(a, b) {
        const as = this.elems;
        for (let i = as.length - 1; i >= 0; i--) {
            if (a(as[i]))
                return false;
            if (b(as[i]))
                return true;
        }
        return true;
    }
}
exports.default = Context;

},{}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Elem {
}
exports.default = Elem;
class CKVar extends Elem {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `kind ${this.name}`;
    }
}
exports.CKVar = CKVar;
exports.ckvar = (name) => new CKVar(name);
exports.isCKVar = (name) => (e) => e instanceof CKVar && e.name.equals(name);
class CTVar extends Elem {
    constructor(name, kind) {
        super();
        this.name = name;
        this.kind = kind;
    }
    toString() {
        return `${this.name} :k ${this.kind}`;
    }
}
exports.CTVar = CTVar;
exports.ctvar = (name, kind) => new CTVar(name, kind);
exports.isCTVar = (name) => (e) => e instanceof CTVar && e.name.equals(name);
class CTMeta extends Elem {
    constructor(name, kind, type) {
        super();
        this.name = name;
        this.kind = kind;
        this.type = type;
    }
    toString() {
        return this.type ? `^${this.name} :k ${this.kind} = ${this.type}` : `^${this.name} :k ${this.kind}`;
    }
    solve(type) {
        return new CTMeta(this.name, this.kind, type);
    }
}
exports.CTMeta = CTMeta;
exports.ctmeta = (name, kind) => new CTMeta(name, kind, null);
exports.csolved = (name, kind, type) => new CTMeta(name, kind, type);
exports.isCTMeta = (name) => (e) => e instanceof CTMeta && e.name.equals(name);
class CMarker extends Elem {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `|>${this.name}`;
    }
}
exports.CMarker = CMarker;
exports.cmarker = (name) => new CMarker(name);
exports.isCMarker = (name) => (e) => e instanceof CMarker && e.name.equals(name);
class CVar extends Elem {
    constructor(name, type) {
        super();
        this.name = name;
        this.type = type;
    }
    toString() {
        return `${this.name} : ${this.type}`;
    }
}
exports.CVar = CVar;
exports.cvar = (name, type) => new CVar(name, type);
exports.isCVar = (name) => (e) => e instanceof CVar && e.name.equals(name);

},{}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Expr {
}
exports.default = Expr;

},{}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const TC_1 = require("./TC");
const types_1 = require("./types");
const values_1 = require("./values");
const computations_1 = require("./computations");
const context_1 = require("./context");
const NameSupply_1 = require("../NameSupply");
const subsumption_1 = require("./subsumption");
const wf_1 = require("./wf");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const NameRep_1 = require("../NameRep");
const utils_1 = require("../utils");
const unification_1 = require("./unification");
const orderedUnsolved = (ctx, type) => {
    const u = ctx.findAll(e => e instanceof elems_1.CTMeta && !e.type ? [e.name, e.kind] : null);
    const r = [];
    const es = type.freeTMeta();
    for (let i = 0; i < es.length; i++) {
        const n = es[i];
        const k = utils_1.assocGet(u, n);
        if (k && !utils_1.assocGet(r, n))
            r.push([n, k]);
    }
    return r;
};
const generalize = (action) => TC_1.freshName(NameRep_1.name('m'))
    .chain(m => TC_1.updateCtx(context_1.default.add(elems_1.cmarker(m)))
    .then(action
    .chain(TC_1.apply)
    .chain(ty => TC_1.log(`gen: ${ty}`).map(() => ty)
    .chain(ty => TC_1.pop(elems_1.isCMarker(m))
    .map(right => {
    const u = orderedUnsolved(right, ty);
    return types_1.tforalls(u, u.reduce((t, [n, _]) => t.substTMeta(n, types_1.tvar(n)), types_1.isTComp(ty) ? ty : types_1.tcomp(ty, types_1.teffsempty())));
}))))
    .chain(TC_1.apply)
    //.map(closeTFun)
    .chain(ty => TC_1.log(`gen done: ${ty}`).map(() => ty)));
const synthVal = (expr) => TC_1.log(`synthVal ${expr}`).chain(() => {
    if (values_1.isVar(expr))
        return TC_1.findVar(expr.name).map(e => e.type);
    if (values_1.isAbs(expr)) {
        const type = expr.type;
        if (type)
            return wf_1.wfType(type)
                .chain(k => wf_1.checkKind(kinds_1.kType, k, `abstraction argument ${expr}`))
                .then(generalize(TC_1.freshNames([expr.name, NameRep_1.name('t'), NameRep_1.name('e')])
                .chain(([x, b, e]) => TC_1.updateCtx(context_1.default.add(elems_1.ctmeta(b, kinds_1.kType), elems_1.ctmeta(e, kinds_1.kEffs), elems_1.cvar(x, type)))
                .then(checkComp(expr.open(values_1.vr(x)), types_1.tcomp(types_1.tmeta(b), types_1.tmeta(e))))
                .map(() => types_1.tfun(type, types_1.tcomp(types_1.tmeta(b), types_1.tmeta(e)))))));
        else
            return generalize(TC_1.freshNames([expr.name, expr.name, NameRep_1.name('t'), NameRep_1.name('e')])
                .chain(([x, a, b, e]) => TC_1.updateCtx(context_1.default.add(elems_1.ctmeta(a, kinds_1.kType), elems_1.ctmeta(b, kinds_1.kType), elems_1.ctmeta(e, kinds_1.kEffs), elems_1.cvar(x, types_1.tmeta(a))))
                .then(checkComp(expr.open(values_1.vr(x)), types_1.tcomp(types_1.tmeta(b), types_1.tmeta(e))))
                .map(() => types_1.tfun(types_1.tmeta(a), types_1.tcomp(types_1.tmeta(b), types_1.tmeta(e))))));
    }
    if (values_1.isAbsT(expr))
        return wf_1.wfKind(expr.kind)
            .then(TC_1.freshName(expr.name)
            .chain(x => TC_1.withElems([elems_1.ctvar(x, expr.kind)], synthComp(expr.openTVar(types_1.tvar(x))))
            .map(type => types_1.tforall(x, expr.kind, type))));
    if (values_1.isAnno(expr))
        return wf_1.wfType(expr.type)
            .chain(k => wf_1.checkKind(kinds_1.kType, k, `annotation ${expr}`))
            .then(checkVal(expr.expr, expr.type)).map(() => expr.type);
    return TC_1.error(`cannot synthVal ${expr}`);
})
    .chain(TC_1.apply)
    .chain(ty => wf_1.wfType(ty)
    .chain(k => wf_1.checkKind(kinds_1.kType, k, `synthVal end ${expr} : ${ty}`)
    .chain(() => TC_1.log(`synthVal done ${expr} : ${ty}`).map(() => ty))));
const synthComp = (expr) => TC_1.log(`synthComp ${expr}`).chain(() => {
    if (computations_1.isReturn(expr))
        return synthVal(expr.val).map(type => types_1.tcomp(type, types_1.teffsempty()));
    if (computations_1.isApp(expr))
        return synthVal(expr.left)
            .chain(ty => TC_1.apply(ty))
            .chain(ty => synthapp(ty, expr.right));
    if (computations_1.isAppT(expr))
        return wf_1.wfType(expr.right)
            .chain(ka => synthVal(expr.left)
            .checkIs(types_1.isTForall, ty => `not a forall in left side of ${expr}: got ${ty}`)
            .chain(ty => wf_1.checkKind(ty.kind, ka, `${expr}`)
            .map(() => ty.open(expr.right))));
    if (computations_1.isLet(expr))
        return synthComp(expr.expr)
            .checkIs(types_1.isTComp, ty => `expected computation type but got ${ty} in left side of ${expr}`)
            .chain(ty => TC_1.freshName(expr.name)
            .chain(x => TC_1.withElems([elems_1.cvar(x, ty.type)], synthComp(expr.open(values_1.vr(x)))
            .checkIs(types_1.isTComp, ty2 => `expected computation type but got ${ty2} in right side of ${expr}`)
            .chain(ty2 => unification_1.openEffs(ty2.eff)
            .chain(ef2open => unification_1.openEffs(ty.eff).chain2(unification_1.unify, TC_1.default.of(ef2open))
            .then(TC_1.apply(ef2open).chain(unification_1.closeEffs)
            .map(ef2open => types_1.tcomp(ty2.type, ef2open))))))));
    return TC_1.error(`cannot synthComp ${expr}`);
})
    .chain(TC_1.apply)
    .chain(ty => wf_1.wfType(ty)
    .chain(k => wf_1.checkKind(kinds_1.kComp, k, `synthComp end ${expr} : ${ty}`)
    .chain(() => TC_1.log(`synthComp done ${expr} : ${ty}`)
    .map(() => ty))));
const checkVal = (expr, type) => TC_1.log(`checkVal ${expr} : ${type}`).chain(() => {
    if (types_1.isTForall(type))
        return TC_1.freshName(type.name).chain(x => TC_1.withElems([elems_1.ctvar(x, type.kind)], checkVal(expr, type.open(types_1.tvar(x)))));
    if (types_1.isTFun(type) && values_1.isAbs(expr) && !expr.type)
        return TC_1.freshName(expr.name).chain(x => TC_1.withElems([elems_1.cvar(x, type.left)], checkComp(expr.open(values_1.vr(x)), type.right)));
    return synthVal(expr)
        .chain(te => TC_1.apply(te))
        .chain(te => TC_1.apply(type)
        .chain(ta => subsumption_1.subsume(te, ta)));
});
const checkComp = (expr, type) => TC_1.log(`checkComp ${expr} : ${type}`).chain(() => {
    if (types_1.isTForall(type))
        return TC_1.freshName(type.name).chain(x => TC_1.withElems([elems_1.ctvar(x, type.kind)], checkComp(expr, type.open(types_1.tvar(x)))));
    if (computations_1.isReturn(expr))
        return TC_1.default.of(type).checkIs(types_1.isTComp, ty => `expected computation type but got ${ty} in checking ${expr}`)
            .chain(ty => checkVal(expr.val, ty.type));
    if (computations_1.isLet(expr))
        return synthComp(expr.expr)
            .checkIs(types_1.isTComp, ty => `expected computation type but got ${ty} in left side of checking ${expr}`)
            .chain(ty => TC_1.freshName(expr.name)
            .chain(x => TC_1.withElems([elems_1.cvar(x, ty.type)], checkComp(expr.open(values_1.vr(x)), type))));
    return synthComp(expr)
        .chain(ty => TC_1.apply(ty)
        .chain(ty => TC_1.apply(type)
        .chain(type => subsumption_1.subsume(ty, type))));
});
const synthapp = (type, expr) => TC_1.log(`synthapp ${type} @ ${expr}`).chain(() => {
    if (types_1.isTForall(type))
        return TC_1.freshName(type.name)
            .chain(x => TC_1.updateCtx(context_1.default.add(elems_1.ctmeta(x, type.kind)))
            .then(synthapp(type.open(types_1.tmeta(x)), expr)));
    if (types_1.isTMeta(type))
        return TC_1.findTMeta(type.name)
            .chain(e => TC_1.freshNames([type.name, type.name])
            .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(type.name), [
            elems_1.ctmeta(a2, kinds_1.kComp), elems_1.ctmeta(a1, kinds_1.kType), e.solve(types_1.tfun(types_1.tmeta(a1), types_1.tmeta(a2)))
        ])
            .then(checkVal(expr, types_1.tmeta(a1))
            .map(() => types_1.tmeta(a2)))));
    if (types_1.isTFun(type))
        return checkVal(expr, type.left).map(() => type.right);
    return TC_1.error(`cannot synthapp ${type} @ ${expr}`);
})
    .chain(TC_1.apply)
    .chain(ty => wf_1.wfType(ty)
    .chain(k => wf_1.checkKind(kinds_1.kComp, k, `synthapp end ${type} @ ${expr}`)
    .chain(() => TC_1.log(`synthapp done ${type} @ ${expr}`)
    .map(() => ty))));
exports.synthgenVal = (expr) => generalize(synthVal(expr))
    .chain(TC_1.apply)
    .chain(ty => wf_1.wfType(ty)
    .chain(k => wf_1.checkKind(kinds_1.kType, k, `synthgenVal of ${expr} : ${ty}`)
    .map(() => ty)));
exports.synthgenComp = (expr) => generalize(synthComp(expr))
    .map(ty => types_1.isTComp(ty) ? ty : types_1.tpure(ty))
    .chain(TC_1.apply)
    .chain(ty => wf_1.wfType(ty)
    .chain(k => wf_1.checkKind(kinds_1.kComp, k, `synthgenComp of ${expr} : ${ty}`)
    .map(() => ty)));
exports.inferVal = (ctx, expr) => exports.synthgenVal(expr).run(ctx, new NameSupply_1.default(0)).val;

},{"../NameRep":2,"../NameSupply":3,"../utils":25,"./TC":5,"./computations":6,"./context":7,"./elems":8,"./kinds":13,"./subsumption":14,"./types":15,"./unification":16,"./values":17,"./wf":18}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const context_1 = require("./context");
const kinds_1 = require("./kinds");
const elems_1 = require("./elems");
const initialContext = context_1.default.of(elems_1.ckvar(kinds_1.nType), elems_1.ckvar(kinds_1.nComp), elems_1.ckvar(kinds_1.nEffs), elems_1.ckvar(kinds_1.nEff));
exports.default = initialContext;

},{"./context":7,"./elems":8,"./kinds":13}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const values_1 = require("./values");
const utils_1 = require("../utils");
const computations_1 = require("./computations");
const compileToJS = (e) => {
    if (values_1.isVar(e))
        return `${e.name}`;
    if (values_1.isAbs(e))
        return `(${e.name} => ${compileToJS(e.body)})`;
    if (values_1.isAbsT(e))
        return compileToJS(e.body);
    if (values_1.isAnno(e))
        return compileToJS(e.expr);
    if (computations_1.isReturn(e))
        return `${compileToJS(e.val)}`;
    if (computations_1.isApp(e))
        return `${compileToJS(e.left)}(${compileToJS(e.right)})`;
    if (computations_1.isAppT(e))
        return compileToJS(e.left);
    if (computations_1.isLet(e))
        return `_do(${compileToJS(e.expr)}, ${e.name} => ${compileToJS(e.body)})`;
    return utils_1.impossible();
};
exports.default = compileToJS;

},{"../utils":25,"./computations":6,"./values":17}],13:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const NameRep_1 = require("../NameRep");
class Kind {
}
exports.default = Kind;
class KVar extends Kind {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return this.name.toString();
    }
    equals(that) {
        return that instanceof KVar && this.name.equals(that.name);
    }
}
exports.KVar = KVar;
exports.kvar = (name) => new KVar(name);
exports.isKVar = (kind) => kind instanceof KVar;
class KFun extends Kind {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} -> ${this.right})`;
    }
    equals(that) {
        return that instanceof KFun && this.left.equals(that.left) && this.right.equals(that.right);
    }
}
exports.KFun = KFun;
exports.kfun = (left, right) => new KFun(left, right);
exports.kfunFrom = (ks) => ks.reduceRight((x, y) => exports.kfun(y, x));
function kfuns(...ks) { return exports.kfunFrom(ks); }
exports.kfuns = kfuns;
exports.isKFun = (kind) => kind instanceof KFun;
exports.nType = NameRep_1.name('Type');
exports.kType = exports.kvar(exports.nType);
exports.nComp = NameRep_1.name('Comp');
exports.kComp = exports.kvar(exports.nComp);
exports.nEffs = NameRep_1.name('Effs');
exports.kEffs = exports.kvar(exports.nEffs);
exports.nEff = NameRep_1.name('Eff');
exports.kEff = exports.kvar(exports.nEff);

},{"../NameRep":2}],14:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const context_1 = require("./context");
const wf_1 = require("./wf");
const TC_1 = require("./TC");
const elems_1 = require("./elems");
const unification_1 = require("./unification");
const kinds_1 = require("./kinds");
const solve = (a, b) => TC_1.log(`solve ${a} = ${b}`).then(!b.isMono() ? TC_1.error(`polymorphic type in solve ${a} = ${b}`) :
    TC_1.findTMeta(a).chain(e => TC_1.pop(elems_1.isCTMeta(a))
        .chain(right => wf_1.wfType(b)
        .then(TC_1.updateCtx(context_1.default.append(context_1.default.of(e.solve(b)).append(right)))))));
const instL = (a, b) => TC_1.log(`instL ${a} := ${b}`).then(types_1.isTMeta(b) ? TC_1.iff(TC_1.ordered(a, b.name), solve(b.name, types_1.tmeta(a)), solve(a, b)) :
    solve(a, b).catch(err => TC_1.log(`solve failed: ${err}`).chain(() => {
        if (types_1.isTFun(b))
            return TC_1.freshNames([a, a, a])
                .chain(([a1, a2, a3]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kinds_1.kType), elems_1.ctmeta(a3, kinds_1.kEffs), elems_1.ctmeta(a1, kinds_1.kType), elems_1.csolved(a, kinds_1.kType, types_1.tfun(types_1.tmeta(a1), types_1.tcomp(types_1.tmeta(a2), types_1.tmeta(a3))))])
                .then(instR(b.left, a1))
                .then(TC_1.apply(b.right))
                .chain(type => exports.subsume(types_1.tcomp(types_1.tmeta(a2), types_1.tmeta(a3)), type)));
        if (types_1.isTComp(b))
            return TC_1.freshNames([a, a])
                .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kinds_1.kEffs), elems_1.ctmeta(a1, kinds_1.kType), elems_1.csolved(a, kinds_1.kComp, types_1.tcomp(types_1.tmeta(a1), types_1.tmeta(a2)))])
                .then(instL(a1, b.type))
                .then(TC_1.apply(b.eff))
                .chain(eff => instL(a2, eff)));
        if (types_1.isTForall(b))
            return TC_1.freshName(b.name).chain(x => TC_1.withElems([elems_1.ctvar(x, b.kind)], instL(a, b.open(types_1.tvar(x)))));
        if (types_1.isTEffsExtend(b))
            return TC_1.freshNames([a, a])
                .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kinds_1.kEffs), elems_1.ctmeta(a1, kinds_1.kEff), elems_1.csolved(a, kinds_1.kEffs, types_1.teffsextend(types_1.tmeta(a1), types_1.tmeta(a2)))])
                .then(instL(a1, b.type))
                .then(TC_1.apply(b.rest))
                .chain(type => instL(a2, type)));
        if (types_1.isTApp(b))
            return wf_1.wfType(b)
                .chain(kAll => wf_1.wfType(b.left)
                .chain(kLeft => wf_1.wfType(b.right)
                .chain(kRight => TC_1.freshNames([a, a])
                .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kRight), elems_1.ctmeta(a1, kLeft), elems_1.csolved(a, kAll, types_1.tapp(types_1.tmeta(a1), types_1.tmeta(a2)))])
                .then(unification_1.instUnify(a1, b.left)
                .then(TC_1.apply(b.right)
                .chain(right => unification_1.instUnify(a2, right))))))));
        return TC_1.error(`instL failed: ${a} = ${b}`);
    })));
const instR = (a, b) => TC_1.log(`instR ${b} := ${a}`).then(types_1.isTMeta(a) ? TC_1.iff(TC_1.ordered(b, a.name), solve(a.name, types_1.tmeta(b)), solve(b, a)) :
    solve(b, a).catch(err => TC_1.log(`solve failed: ${err}`).chain(() => {
        if (types_1.isTFun(a))
            return TC_1.freshNames([b, b, b])
                .chain(([b1, b2, b3]) => TC_1.replace(elems_1.isCTMeta(b), [elems_1.ctmeta(b2, kinds_1.kType), elems_1.ctmeta(b3, kinds_1.kEffs), elems_1.ctmeta(b1, kinds_1.kType), elems_1.csolved(b, kinds_1.kType, types_1.tfun(types_1.tmeta(b1), types_1.tcomp(types_1.tmeta(b2), types_1.tmeta(b3))))])
                .then(instL(b1, a.left)
                .then(TC_1.apply(a.right))
                .chain(type => exports.subsume(type, types_1.tcomp(types_1.tmeta(b2), types_1.tmeta(b3))))));
        if (types_1.isTComp(a))
            return TC_1.freshNames([b, b])
                .chain(([b1, b2]) => TC_1.replace(elems_1.isCTMeta(b), [elems_1.ctmeta(b2, kinds_1.kEffs), elems_1.ctmeta(b1, kinds_1.kType), elems_1.csolved(b, kinds_1.kComp, types_1.tcomp(types_1.tmeta(b1), types_1.tmeta(b2)))])
                .then(instR(a.type, b1)
                .then(TC_1.apply(a.eff))
                .chain(eff => instR(eff, b2))));
        if (types_1.isTForall(a))
            return TC_1.freshName(a.name).chain(x => TC_1.withElems([elems_1.ctmeta(x, a.kind)], instR(a.open(types_1.tmeta(x)), b)));
        if (types_1.isTEffsExtend(a))
            return TC_1.freshNames([b, b])
                .chain(([b1, b2]) => TC_1.replace(elems_1.isCTMeta(b), [elems_1.ctmeta(b2, kinds_1.kEffs), elems_1.ctmeta(b1, kinds_1.kEff), elems_1.csolved(b, kinds_1.kEffs, types_1.teffsextend(types_1.tmeta(b1), types_1.tmeta(b2)))])
                .then(instR(a.type, b1))
                .then(TC_1.apply(a.rest))
                .chain(type => instR(type, b2)));
        if (types_1.isTApp(a))
            return wf_1.wfType(a)
                .chain(kAll => wf_1.wfType(a.left)
                .chain(kLeft => wf_1.wfType(a.right)
                .chain(kRight => TC_1.freshNames([b, b])
                .chain(([b1, b2]) => TC_1.replace(elems_1.isCTMeta(b), [elems_1.ctmeta(b2, kRight), elems_1.ctmeta(b1, kLeft), elems_1.csolved(b, kAll, types_1.tapp(types_1.tmeta(b1), types_1.tmeta(b2)))])
                .then(unification_1.instUnify(b1, a.left)
                .then(TC_1.apply(a.right)
                .chain(right => unification_1.instUnify(b2, right))))))));
        return TC_1.error(`instR failed: ${b} = ${a}`);
    })));
exports.subsume = (a, b) => TC_1.log(`subsume ${a} <: ${b}`).then(wf_1.wfType(a).chain(k1 => wf_1.wfType(b).chain(k2 => wf_1.checkKind(k1, k2, `subsume ${a} <: ${b}`)
    .chain(() => {
    if (types_1.isTVar(a) && types_1.isTVar(b) && a.name.equals(b.name))
        return TC_1.ok;
    if (types_1.isTMeta(a) && types_1.isTMeta(b) && a.name.equals(b.name))
        return TC_1.ok;
    if (types_1.isTEffsEmpty(a) && types_1.isTEffsEmpty(b))
        return TC_1.ok;
    if (types_1.isTFun(a) && types_1.isTFun(b))
        return exports.subsume(b.left, a.left)
            .then(TC_1.apply(a.right)
            .chain(ta => TC_1.apply(b.right)
            .chain(tb => exports.subsume(ta, tb))));
    if (types_1.isTComp(a) && types_1.isTComp(b))
        return exports.subsume(a.type, b.type)
            .then(TC_1.apply(a.eff)
            .chain(ta => TC_1.apply(b.eff)
            .chain(tb => exports.subsume(ta, tb))));
    if (types_1.isTEffsExtend(a) && types_1.isTEffsExtend(b))
        return unification_1.rewriteEffs(a.type, b, `${a} <: ${b}`)
            .chain(es => exports.subsume(a.type, es.type)
            .then(TC_1.apply(a.rest)
            .chain(restA => TC_1.apply(es.rest)
            .chain(restB => exports.subsume(restA, restB)))));
    if (types_1.isTApp(a) && types_1.isTApp(b))
        return unification_1.unify(a, b);
    if (types_1.isTForall(a))
        return TC_1.freshName(a.name).chain(x => TC_1.withElems([elems_1.ctmeta(x, a.kind)], exports.subsume(a.open(types_1.tmeta(x)), b)));
    if (types_1.isTForall(b))
        return TC_1.freshName(b.name).chain(x => TC_1.withElems([elems_1.ctvar(x, b.kind)], exports.subsume(a, b.open(types_1.tvar(x)))));
    if (types_1.isTMeta(a))
        return TC_1.check(!b.containsTMeta(a.name), `occurs check failed L: ${a} in ${b}`)
            .then(instL(a.name, b));
    if (types_1.isTMeta(b))
        return TC_1.check(!a.containsTMeta(b.name), `occurs check failed R: ${b} in ${a}`)
            .then(instR(a, b.name));
    return TC_1.error(`subsume failed: ${a} <: ${b}`);
}))));

},{"./TC":5,"./context":7,"./elems":8,"./kinds":13,"./types":15,"./unification":16,"./wf":18}],15:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const utils_1 = require("../utils");
class Type {
    pretty() {
        return this.toString();
    }
}
exports.default = Type;
const parenIf = (cs, x) => utils_1.any(cs, c => x instanceof c) ? `(${x.pretty()})` : x.pretty();
class TVar extends Type {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return this.name.toString();
    }
    pretty() {
        return this.name.toString();
    }
    isMono() {
        return true;
    }
    substTVar(name, type) {
        return this.name.equals(name) ? type : this;
    }
    substTMeta(name, type) {
        return this;
    }
    containsTMeta(name) {
        return false;
    }
    containsTVar(name) {
        return this.name.equals(name);
    }
    freeTMeta() {
        return [];
    }
    equals(that) {
        return that instanceof TVar && this.name.equals(that.name);
    }
}
exports.TVar = TVar;
exports.tvar = (name) => new TVar(name);
exports.isTVar = (type) => type instanceof TVar;
class TMeta extends Type {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `^${this.name}`;
    }
    pretty() {
        return `^${this.name}`;
    }
    isMono() {
        return true;
    }
    substTVar(name, type) {
        return this;
    }
    substTMeta(name, type) {
        return this.name.equals(name) ? type : this;
    }
    containsTMeta(name) {
        return this.name.equals(name);
    }
    containsTVar(name) {
        return false;
    }
    freeTMeta() {
        return [this.name];
    }
    equals(that) {
        return that instanceof TMeta && this.name.equals(that.name);
    }
}
exports.TMeta = TMeta;
exports.tmeta = (name) => new TMeta(name);
exports.isTMeta = (type) => type instanceof TMeta;
class TApp extends Type {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        const left = this.left;
        if (exports.isTApp(left) && exports.isTVar(left.left) && /[^a-z]/i.test(left.left.toString()[0]))
            return `(${left.right} ${left.left} ${this.right})`;
        return `(${left} ${this.right})`;
    }
    pretty() {
        const f = exports.flattenTApp(this);
        return `${[f.head].concat(f.tail).map(x => parenIf([TApp, TFun, TForall], x)).join(' ')}`;
    }
    isMono() {
        return this.left.isMono() && this.right.isMono();
    }
    substTVar(name, type) {
        return new TApp(this.left.substTVar(name, type), this.right.substTVar(name, type));
    }
    substTMeta(name, type) {
        return new TApp(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
    }
    containsTMeta(name) {
        return this.left.containsTMeta(name) || this.right.containsTMeta(name);
    }
    containsTVar(name) {
        return this.left.containsTVar(name) || this.right.containsTVar(name);
    }
    freeTMeta() {
        return this.left.freeTMeta().concat(this.right.freeTMeta());
    }
    equals(that) {
        return that instanceof TApp && this.left.equals(that.left) && this.right.equals(that.right);
    }
}
exports.TApp = TApp;
exports.tapp = (left, right) => new TApp(left, right);
exports.tappsFrom = (ts) => ts.reduce(exports.tapp);
exports.tapps = (...ts) => exports.tappsFrom(ts);
exports.isTApp = (type) => type instanceof TApp;
exports.flattenTApp = (type) => {
    if (exports.isTApp(type)) {
        const rec = exports.flattenTApp(type.left);
        return { head: rec.head, tail: rec.tail.concat([type.right]) };
    }
    return { head: type, tail: [] };
};
exports.headTApp = (type) => exports.flattenTApp(type).head;
const ARROW = '->';
class TFun extends Type {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} -> ${this.right})`;
    }
    pretty() {
        return `(${this.left.pretty()} ${ARROW} ${this.right.pretty()})`;
    }
    isMono() {
        return this.left.isMono() && this.right.isMono();
    }
    substTVar(name, type) {
        return new TFun(this.left.substTVar(name, type), this.right.substTVar(name, type));
    }
    substTMeta(name, type) {
        return new TFun(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
    }
    containsTMeta(name) {
        return this.left.containsTMeta(name) || this.right.containsTMeta(name);
    }
    containsTVar(name) {
        return this.left.containsTVar(name) || this.right.containsTVar(name);
    }
    freeTMeta() {
        return this.left.freeTMeta().concat(this.right.freeTMeta());
    }
    equals(that) {
        return that instanceof TFun && this.left.equals(that.left) && this.right.equals(that.right);
    }
}
exports.TFun = TFun;
exports.tfun = (left, right) => new TFun(left, right);
exports.tfunFrom = (ts) => ts.reduceRight((x, y) => exports.tfun(y, x));
function tfuns(...ts) { return exports.tfunFrom(ts); }
exports.tfuns = tfuns;
exports.tfunp = (left, right) => new TFun(left, exports.tpure(right));
exports.tfunpFrom = (ts) => ts.reduceRight((x, y) => exports.tfunp(y, x));
function tfunps(...ts) { return exports.tfunpFrom(ts); }
exports.tfunps = tfunps;
exports.isTFun = (type) => type instanceof TFun;
const FORALL = 'forall';
class TForall extends Type {
    constructor(name, kind, type) {
        super();
        this.name = name;
        this.kind = kind;
        this.type = type;
    }
    toString() {
        return `(forall(${this.name} : ${this.kind}). ${this.type})`;
    }
    pretty() {
        const f = exports.flattenTForall(this);
        return `${FORALL}${f.ns.map(([n, k]) => `(${n} : ${k})`).join('')}. ${f.type.pretty()}`;
    }
    isMono() {
        return false;
    }
    substTVar(name, type) {
        return this.name.equals(name) ? this : new TForall(this.name, this.kind, this.type.substTVar(name, type));
    }
    open(type) {
        return this.type.substTVar(this.name, type);
    }
    substTMeta(name, type) {
        return new TForall(this.name, this.kind, this.type.substTMeta(name, type));
    }
    containsTMeta(name) {
        return this.type.containsTMeta(name);
    }
    containsTVar(name) {
        return this.name.equals(name) ? false : this.type.containsTVar(name);
    }
    freeTMeta() {
        return this.type.freeTMeta();
    }
    equals(that) {
        return that instanceof TForall && this.name.equals(that.name) && this.kind.equals(that.kind) && this.type.equals(that.type);
    }
}
exports.TForall = TForall;
exports.tforall = (name, kind, type) => new TForall(name, kind, type);
exports.tforalls = (ns, type) => {
    if (ns.length === 0)
        return type;
    const ret = ns.reduceRight((t, [n, k]) => exports.tpure(exports.tforall(n, k, t)), type);
    return exports.isTComp(ret) ? ret.type : ret;
};
exports.tforallp = (name, kind, type) => new TForall(name, kind, exports.tpure(type));
exports.tforallps = (ns, type) => {
    if (ns.length === 0)
        return type;
    const ret = ns.reduceRight((t, [n, k]) => exports.tpure(exports.tforallp(n, k, t)), type);
    return exports.isTComp(ret) ? ret.type : ret;
};
exports.isTForall = (type) => type instanceof TForall;
exports.flattenTForall = (type) => {
    if (exports.isTForall(type)) {
        const rec = exports.flattenTForall(type.type);
        return { ns: [[type.name, type.kind]].concat(rec.ns), type: rec.type };
    }
    return { ns: [], type };
};
class TComp extends Type {
    constructor(type, eff) {
        super();
        this.type = type;
        this.eff = eff;
    }
    toString() {
        return `${this.type}!${this.eff}`;
    }
    pretty() {
        return exports.isTEffsEmpty(this.eff) ? `${this.type.pretty()}` :
            `${this.type.pretty()}!${this.eff.pretty()}`;
    }
    isMono() {
        return this.type.isMono() && this.eff.isMono();
    }
    substTVar(name, type) {
        return new TComp(this.type.substTVar(name, type), this.eff.substTVar(name, type));
    }
    substTMeta(name, type) {
        return new TComp(this.type.substTMeta(name, type), this.eff.substTMeta(name, type));
    }
    containsTMeta(name) {
        return this.type.containsTMeta(name) || this.eff.containsTMeta(name);
    }
    containsTVar(name) {
        return this.type.containsTVar(name) || this.eff.containsTVar(name);
    }
    freeTMeta() {
        return this.type.freeTMeta().concat(this.eff.freeTMeta());
    }
    equals(that) {
        return that instanceof TComp && this.type.equals(that.type) && this.eff.equals(that.eff);
    }
}
exports.TComp = TComp;
exports.tcomp = (type, eff) => new TComp(type, eff);
exports.tpure = (type) => exports.tcomp(type, exports.teffsempty());
exports.isTComp = (type) => type instanceof TComp;
class TEffsEmpty extends Type {
    constructor() { super(); }
    toString() {
        return `{}`;
    }
    isMono() {
        return true;
    }
    substTVar(name, type) {
        return this;
    }
    substTMeta(name, type) {
        return this;
    }
    containsTMeta(name) {
        return false;
    }
    containsTVar(name) {
        return false;
    }
    freeTMeta() {
        return [];
    }
    equals(that) {
        return that instanceof TEffsEmpty;
    }
}
exports.TEffsEmpty = TEffsEmpty;
exports.teffsempty = () => new TEffsEmpty();
exports.isTEffsEmpty = (type) => type instanceof TEffsEmpty;
class TEffsExtend extends Type {
    constructor(type, rest) {
        super();
        this.type = type;
        this.rest = rest;
    }
    toString() {
        return `{ ${this.type} | ${this.rest} }`;
    }
    pretty() {
        return exports.isTEffsEmpty(this.rest) ? `{ ${this.type.pretty()} }` :
            `{ ${this.type.pretty()} | ${this.rest.pretty()} }`;
    }
    isMono() {
        return this.type.isMono() && this.rest.isMono();
    }
    substTVar(name, type) {
        return new TEffsExtend(this.type.substTVar(name, type), this.rest.substTVar(name, type));
    }
    substTMeta(name, type) {
        return new TEffsExtend(this.type.substTMeta(name, type), this.rest.substTMeta(name, type));
    }
    containsTMeta(name) {
        return this.type.containsTMeta(name) || this.rest.containsTMeta(name);
    }
    containsTVar(name) {
        return this.type.containsTVar(name) || this.rest.containsTVar(name);
    }
    freeTMeta() {
        return this.type.freeTMeta().concat(this.rest.freeTMeta());
    }
    equals(that) {
        return that instanceof TEffsExtend && this.type.equals(that.type) && this.rest.equals(that.rest);
    }
}
exports.TEffsExtend = TEffsExtend;
exports.teffsextend = (type, rest) => new TEffsExtend(type, rest);
exports.teffsFrom = (ts, rest) => ts.reduceRight((a, b) => exports.teffsextend(b, a), rest || exports.teffsempty());
exports.teffs = (...ts) => exports.teffsFrom(ts);
exports.isTEffsExtend = (type) => type instanceof TEffsExtend;
exports.flattenEffs = (row) => {
    if (exports.isTEffsExtend(row)) {
        const rec = exports.flattenEffs(row.rest);
        return { types: [row.type].concat(rec.types), rest: rec.rest };
    }
    return { types: [], rest: row };
};

},{"../utils":25}],16:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const TC_1 = require("./TC");
const wf_1 = require("./wf");
const types_1 = require("./types");
const elems_1 = require("./elems");
const NameRep_1 = require("../NameRep");
const context_1 = require("./context");
const kinds_1 = require("./kinds");
/*
export const closeTFun = (type: Type): Type => {
  if (!isTForall(type)) return type;
  const f = flattenTForall(type);
  const body = f.type;
  if (!isTFun(body)) return type;
  const eff = body.eff;
  const feff = flattenEffs(eff);
  const tv = feff.rest;
  if (!isTVar(tv)) return type;
  const name = tv.name;
  if (body.left.containsTVar(name)) return type;
  if (body.right.containsTVar(name)) return type;
  if (any(feff.types, t => t.containsTVar(name))) return type;
  const neff = teffsFrom(feff.types);
  const args = remove(f.ns, ([n, k]) => name.equals(n));
  return tforalls(args, tfun(body.left, neff, body.right));
};
*/
exports.closeEffs = (type) => TC_1.log(`closeEffs ${type}`).chain(() => {
    if (types_1.isTEffsExtend(type))
        return exports.closeEffs(type.rest).map(rest => types_1.teffsextend(type.type, rest));
    if (types_1.isTMeta(type))
        return TC_1.default.of(types_1.teffsempty());
    if (types_1.isTEffsEmpty(type))
        return TC_1.default.of(type);
    return TC_1.error(`unexpected type in openEffs ${type}`);
});
exports.openEffs = (type) => TC_1.log(`openEffs ${type}`).chain(() => {
    if (types_1.isTEffsExtend(type))
        return exports.openEffs(type.rest).map(rest => types_1.teffsextend(type.type, rest));
    if (types_1.isTEffsEmpty(type))
        return TC_1.freshName(NameRep_1.name('e')).chain(e => TC_1.updateCtx(context_1.default.add(elems_1.ctmeta(e, kinds_1.kEffs))).map(() => types_1.tmeta(e)));
    if (types_1.isTMeta(type))
        return TC_1.default.of(type);
    return TC_1.error(`unexpected type in openEffs ${type}`);
});
exports.rewriteEffs = (head, type, msg) => TC_1.log(`rewriteEffs ${head} in ${type}`).chain(() => {
    if (types_1.isTEffsExtend(type)) {
        if (types_1.headTApp(type.type).equals(types_1.headTApp(head)))
            return TC_1.pure(type);
        return exports.rewriteEffs(head, type.rest, msg)
            .map(rest => types_1.teffsextend(rest.type, types_1.teffsextend(type.type, rest.rest)));
    }
    if (types_1.isTMeta(type))
        return TC_1.freshName(NameRep_1.name('e'))
            .chain(r => TC_1.replace(elems_1.isCTMeta(type.name), [
            elems_1.ctmeta(r, kinds_1.kEffs),
            elems_1.csolved(type.name, kinds_1.kEffs, types_1.teffsextend(head, types_1.tmeta(r)))
        ])
            .map(() => types_1.teffsextend(head, types_1.tmeta(r))));
    return TC_1.error(`cannot rewrite effs ${head} in ${type}: ${msg}`);
});
const solveUnify = (a, b) => TC_1.log(`solve unify ${a} = ${b}`).then(!b.isMono() ? TC_1.error(`polymorphic type in solve unify ${a} = ${b}`) :
    TC_1.findTMeta(a).chain(e => TC_1.pop(elems_1.isCTMeta(a))
        .chain(right => wf_1.wfType(b)
        .then(TC_1.updateCtx(context_1.default.append(context_1.default.of(e.solve(b)).append(right)))))));
exports.instUnify = (a, b) => TC_1.log(`instUnify ${a} := ${b}`).then(types_1.isTMeta(b) ? TC_1.iff(TC_1.ordered(a, b.name), solveUnify(b.name, types_1.tmeta(a)), solveUnify(a, b)) :
    solveUnify(a, b).catch(err => TC_1.log(`solveUnify failed: ${err}`).chain(() => {
        if (types_1.isTFun(b))
            return TC_1.freshNames([a, a, a])
                .chain(([a1, a2, a3]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kinds_1.kType), elems_1.ctmeta(a3, kinds_1.kEffs), elems_1.ctmeta(a1, kinds_1.kType), elems_1.csolved(a, kinds_1.kType, types_1.tfun(types_1.tmeta(a1), types_1.tcomp(types_1.tmeta(a2), types_1.tmeta(a3))))])
                .then(exports.instUnify(a1, b.left))
                .then(TC_1.apply(b.right))
                .chain(type => exports.unify(types_1.tcomp(types_1.tmeta(a2), types_1.tmeta(a3)), type)));
        if (types_1.isTComp(b))
            return TC_1.freshNames([a, a])
                .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kinds_1.kEffs), elems_1.ctmeta(a1, kinds_1.kType), elems_1.csolved(a, kinds_1.kComp, types_1.tcomp(types_1.tmeta(a1), types_1.tmeta(a2)))])
                .then(exports.instUnify(a1, b.type))
                .then(TC_1.apply(b.eff))
                .chain(eff => exports.instUnify(a2, eff)));
        if (types_1.isTForall(b))
            return TC_1.freshName(b.name).chain(x => TC_1.withElems([elems_1.ctvar(x, b.kind)], exports.instUnify(a, b.open(types_1.tvar(x)))));
        if (types_1.isTEffsExtend(b))
            return TC_1.freshNames([a, a])
                .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kinds_1.kEffs), elems_1.ctmeta(a1, kinds_1.kEff), elems_1.csolved(a, kinds_1.kEffs, types_1.teffsextend(types_1.tmeta(a1), types_1.tmeta(a2)))])
                .then(exports.instUnify(a1, b.type))
                .then(TC_1.apply(b.rest))
                .chain(type => exports.instUnify(a2, type)));
        if (types_1.isTApp(b))
            return wf_1.wfType(b)
                .chain(kAll => wf_1.wfType(b.left)
                .chain(kLeft => wf_1.wfType(b.right)
                .chain(kRight => TC_1.freshNames([a, a])
                .chain(([a1, a2]) => TC_1.replace(elems_1.isCTMeta(a), [elems_1.ctmeta(a2, kRight), elems_1.ctmeta(a1, kLeft), elems_1.csolved(a, kAll, types_1.tapp(types_1.tmeta(a1), types_1.tmeta(a2)))])
                .then(exports.instUnify(a1, b.left)
                .then(TC_1.apply(b.right)
                .chain(right => exports.instUnify(a2, right))))))));
        return TC_1.error(`instUnify failed: ${a} = ${b}`);
    })));
exports.unify = (a, b) => TC_1.log(`unify ${a} ~ ${b}`).then(wf_1.wfType(a).chain(k1 => wf_1.wfType(b).chain(k2 => wf_1.checkKind(k1, k2, `unify ${a} ~ ${b}`)
    .chain(() => {
    if (types_1.isTVar(a) && types_1.isTVar(b) && a.name.equals(b.name))
        return TC_1.ok;
    if (types_1.isTMeta(a) && types_1.isTMeta(b) && a.name.equals(b.name))
        return TC_1.ok;
    if (types_1.isTEffsEmpty(a) && types_1.isTEffsEmpty(b))
        return TC_1.ok;
    if (types_1.isTFun(a) && types_1.isTFun(b))
        return exports.unify(a.left, b.left)
            .then(TC_1.apply(a.right)
            .chain(ta => TC_1.apply(b.right)
            .chain(tb => exports.unify(ta, tb))));
    if (types_1.isTComp(a) && types_1.isTComp(b))
        return exports.unify(a.type, b.type)
            .then(TC_1.apply(a.eff)
            .chain(ta => TC_1.apply(b.eff)
            .chain(tb => exports.unify(ta, tb))));
    if (types_1.isTApp(a) && types_1.isTApp(b))
        return exports.unify(a.left, b.left)
            .then(TC_1.apply(a.right)
            .chain(ta => TC_1.apply(b.right)
            .chain(tb => exports.unify(ta, tb))));
    if (types_1.isTEffsExtend(a) && types_1.isTEffsExtend(b))
        return exports.rewriteEffs(a.type, b, `${a} <: ${b}`)
            .chain(es => exports.unify(a.type, es.type)
            .then(TC_1.apply(a.rest)
            .chain(restA => TC_1.apply(es.rest)
            .chain(restB => exports.unify(restA, restB)))));
    if (types_1.isTForall(a) && types_1.isTForall(b))
        return wf_1.checkKind(a.kind, b.kind, `unification of ${a} ~ ${b}`)
            .then(TC_1.freshName(a.name)
            .chain(x => TC_1.withElems([elems_1.ctvar(x, a.kind)], exports.unify(a.open(types_1.tvar(x)), b.open(types_1.tvar(x))))));
    if (types_1.isTMeta(a))
        return TC_1.check(!b.containsTMeta(a.name), `occurs check failed L unify: ${a} in ${b}`)
            .then(exports.instUnify(a.name, b));
    if (types_1.isTMeta(b))
        return TC_1.check(!a.containsTMeta(b.name), `occurs check failed R unify: ${b} in ${a}`)
            .then(exports.instUnify(b.name, a));
    return TC_1.error(`unification failed: ${a} <: ${b}`);
}))));

},{"../NameRep":2,"./TC":5,"./context":7,"./elems":8,"./kinds":13,"./types":15,"./wf":18}],17:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const computations_1 = require("./computations");
class Val extends exprs_1.default {
    constructor() {
        super(...arguments);
        this._type = 'Val';
    }
}
exports.default = Val;
class Var extends Val {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return this.name.toString();
    }
    subst(name, val) {
        return this.name.equals(name) ? val : this;
    }
    substTVar(name, type) {
        return this;
    }
}
exports.Var = Var;
exports.vr = (name) => new Var(name);
exports.isVar = (expr) => expr instanceof Var;
class Abs extends Val {
    constructor(name, type, body) {
        super();
        this.name = name;
        this.type = type;
        this.body = body;
    }
    toString() {
        return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.body})` : `(\\${this.name} -> ${this.body})`;
    }
    subst(name, val) {
        return this.name.equals(name) ? this : new Abs(this.name, this.type, this.body.subst(name, val));
    }
    open(val) {
        return this.body.subst(this.name, val);
    }
    substTVar(name, type) {
        return new Abs(this.name, this.type && this.type.substTVar(name, type), this.body.substTVar(name, type));
    }
}
exports.Abs = Abs;
exports.abs = (name, body) => new Abs(name, null, body);
exports.absty = (name, type, body) => new Abs(name, type, body);
exports.abss = (ns, body) => ns.reduceRight((b, n) => computations_1.ret(exports.abs(n, b)), body);
exports.abstys = (ns, body) => ns.reduceRight((b, [n, t]) => computations_1.ret(exports.absty(n, t, b)), body);
exports.isAbs = (expr) => expr instanceof Abs;
class AbsT extends Val {
    constructor(name, kind, body) {
        super();
        this.name = name;
        this.kind = kind;
        this.body = body;
    }
    toString() {
        return `(/\\(${this.name} : ${this.kind}) -> ${this.body})`;
    }
    subst(name, val) {
        return new AbsT(this.name, this.kind, this.body.subst(name, val));
    }
    substTVar(name, type) {
        return this.name.equals(name) ? this : new AbsT(this.name, this.kind, this.body.substTVar(name, type));
    }
    openTVar(val) {
        return this.body.substTVar(this.name, val);
    }
}
exports.AbsT = AbsT;
exports.absT = (name, kind, body) => new AbsT(name, kind, body);
exports.absTs = (ns, body) => ns.reduceRight((b, [n, k]) => computations_1.ret(exports.absT(n, k, b)), body);
exports.isAbsT = (expr) => expr instanceof AbsT;
class Anno extends Val {
    constructor(expr, type) {
        super();
        this.expr = expr;
        this.type = type;
    }
    toString() {
        return `(${this.expr} : ${this.type})`;
    }
    subst(name, val) {
        return new Anno(this.expr.subst(name, val), this.type);
    }
    substTVar(name, type) {
        return new Anno(this.expr.substTVar(name, type), this.type.substTVar(name, type));
    }
}
exports.Anno = Anno;
exports.anno = (expr, type) => new Anno(expr, type);
exports.isAnno = (expr) => expr instanceof Anno;

},{"./computations":6,"./exprs":9}],18:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const TC_1 = require("./TC");
const kinds_1 = require("./kinds");
const utils_1 = require("../utils");
const types_1 = require("./types");
const elems_1 = require("./elems");
exports.checkKind = (exp, actual, msg) => {
    if (exp.equals(actual))
        return TC_1.ok;
    return TC_1.error(`kind mismatch, expected ${exp} but got ${actual}${msg ? `: ${msg}` : ''}`);
};
exports.wfKind = (kind) => {
    if (kinds_1.isKVar(kind))
        return TC_1.findKVar(kind.name).void();
    if (kinds_1.isKFun(kind))
        return exports.wfKind(kind.left).then(exports.wfKind(kind.right));
    return utils_1.impossible();
};
exports.wfType = (type) => {
    if (types_1.isTVar(type))
        return TC_1.findTVar(type.name).map(e => e.kind);
    if (types_1.isTMeta(type))
        return TC_1.findTMeta(type.name).map(e => e.kind);
    if (types_1.isTApp(type))
        return exports.wfType(type.left)
            .checkIs(kinds_1.isKFun, k => `left side of ${type} is not a higher-kinded type: ${k}`)
            .chain(k => exports.wfType(type.right)
            .chain(kr => exports.checkKind(k.left, kr, `type application ${type}`))
            .map(() => k.right));
    if (types_1.isTFun(type))
        return exports.wfType(type.left)
            .chain(kl => exports.checkKind(kinds_1.kType, kl, `left side of function: ${type}`))
            .then(exports.wfType(type.right)
            .chain(kr => exports.checkKind(kinds_1.kComp, kr, `right side of function: ${type}`))
            .map(() => kinds_1.kType));
    if (types_1.isTComp(type))
        return exports.wfType(type.type)
            .chain(kl => exports.checkKind(kinds_1.kType, kl, `left side of comp type: ${type}`))
            .then(exports.wfType(type.eff)
            .chain(kr => exports.checkKind(kinds_1.kEffs, kr, `right side of comp type: ${type}`))
            .map(() => kinds_1.kComp));
    if (types_1.isTEffsExtend(type))
        return exports.wfType(type.type)
            .chain(k => exports.checkKind(kinds_1.kEff, k, `effs type ${type}`))
            .then(exports.wfType(type.rest))
            .chain(k => exports.checkKind(kinds_1.kEffs, k, `effs rest ${type}`))
            .map(() => kinds_1.kEffs);
    if (types_1.isTForall(type))
        return exports.wfKind(type.kind)
            .then(TC_1.freshName(type.name)
            .chain(x => TC_1.withElems([elems_1.ctvar(x, type.kind)], exports.wfType(type.open(types_1.tvar(x)))
            .chain(k => exports.checkKind(kinds_1.kComp, k, `tforall: ${type}`)))))
            .map(() => kinds_1.kType);
    if (types_1.isTEffsEmpty(type))
        return TC_1.pure(kinds_1.kEffs);
    return utils_1.impossible();
};

},{"../utils":25,"./TC":5,"./elems":8,"./kinds":13,"./types":15}],19:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const initial_1 = require("./il/initial");
const elems_1 = require("./il/elems");
const types_1 = require("./il/types");
const NameRep_1 = require("./NameRep");
const kinds_1 = require("./il/kinds");
const parser_1 = require("./surface/parser");
const surface_1 = require("./surface/surface");
const TC_1 = require("./il/TC");
const inference_1 = require("./il/inference");
const NameSupply_1 = require("./NameSupply");
const javascriptBackend_1 = require("./il/javascriptBackend");
exports._context = initial_1.default.add(elems_1.ctvar(NameRep_1.name('Str'), kinds_1.kType), elems_1.cvar(NameRep_1.name('show'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('Str'))))), elems_1.ctvar(NameRep_1.name('Void'), kinds_1.kType), elems_1.cvar(NameRep_1.name('caseVoid'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('Void')), types_1.tvar(NameRep_1.name('t'))))), elems_1.ctvar(NameRep_1.name('Unit'), kinds_1.kType), elems_1.cvar(NameRep_1.name('Unit'), types_1.tvar(NameRep_1.name('Unit'))), elems_1.ctvar(NameRep_1.name('Bool'), kinds_1.kType), elems_1.cvar(NameRep_1.name('True'), types_1.tvar(NameRep_1.name('Bool'))), elems_1.cvar(NameRep_1.name('False'), types_1.tvar(NameRep_1.name('Bool'))), elems_1.cvar(NameRep_1.name('caseBool'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('Bool')), types_1.tvar(NameRep_1.name('t'))))), elems_1.ctvar(NameRep_1.name('Nat'), kinds_1.kType), elems_1.cvar(NameRep_1.name('Z'), types_1.tvar(NameRep_1.name('Nat'))), elems_1.cvar(NameRep_1.name('S'), types_1.tfunps(types_1.tvar(NameRep_1.name('Nat')), types_1.tvar(NameRep_1.name('Nat')))), elems_1.cvar(NameRep_1.name('caseNat'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tfunps(types_1.tvar(NameRep_1.name('Nat')), types_1.tvar(NameRep_1.name('t'))), types_1.tvar(NameRep_1.name('Nat')), types_1.tvar(NameRep_1.name('t'))))), elems_1.ctvar(NameRep_1.name('Maybe'), kinds_1.kfuns(kinds_1.kType, kinds_1.kType)), elems_1.cvar(NameRep_1.name('Nothing'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tapps(types_1.tvar(NameRep_1.name('Maybe')), types_1.tvar(NameRep_1.name('t'))))), elems_1.cvar(NameRep_1.name('Just'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tapps(types_1.tvar(NameRep_1.name('Maybe')), types_1.tvar(NameRep_1.name('t')))))), elems_1.cvar(NameRep_1.name('caseMaybe'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType], [NameRep_1.name('r'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('r')), types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('r'))), types_1.tapps(types_1.tvar(NameRep_1.name('Maybe')), types_1.tvar(NameRep_1.name('t'))), types_1.tvar(NameRep_1.name('r'))))), elems_1.ctvar(NameRep_1.name('List'), kinds_1.kfuns(kinds_1.kType, kinds_1.kType)), elems_1.cvar(NameRep_1.name('Nil'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tapps(types_1.tvar(NameRep_1.name('List')), types_1.tvar(NameRep_1.name('t'))))), elems_1.cvar(NameRep_1.name('Cons'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tapps(types_1.tvar(NameRep_1.name('List')), types_1.tvar(NameRep_1.name('t'))), types_1.tapps(types_1.tvar(NameRep_1.name('List')), types_1.tvar(NameRep_1.name('t')))))), elems_1.cvar(NameRep_1.name('caseList'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType], [NameRep_1.name('r'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('r')), types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tapps(types_1.tvar(NameRep_1.name('List')), types_1.tvar(NameRep_1.name('t'))), types_1.tvar(NameRep_1.name('r'))), types_1.tapps(types_1.tvar(NameRep_1.name('List')), types_1.tvar(NameRep_1.name('t'))), types_1.tvar(NameRep_1.name('r'))))), elems_1.cvar(NameRep_1.name('fix'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tfunps(types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('t'))), types_1.tvar(NameRep_1.name('t'))))), elems_1.ctvar(NameRep_1.name('Flip'), kinds_1.kEff), elems_1.cvar(NameRep_1.name('flip'), types_1.tfun(types_1.tvar(NameRep_1.name('Unit')), types_1.tcomp(types_1.tvar(NameRep_1.name('Bool')), types_1.teffs(types_1.tvar(NameRep_1.name('Flip')))))), elems_1.cvar(NameRep_1.name('runFlip'), types_1.tforallps([[NameRep_1.name('e'), kinds_1.kEffs], [NameRep_1.name('t'), kinds_1.kType]], types_1.tfun(types_1.tfun(types_1.tvar(NameRep_1.name('Unit')), types_1.tcomp(types_1.tvar(NameRep_1.name('t')), types_1.teffsFrom([types_1.tvar(NameRep_1.name('Flip'))], types_1.tvar(NameRep_1.name('e'))))), types_1.tcomp(types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('e')))))), elems_1.ctvar(NameRep_1.name('Fail'), kinds_1.kEff), elems_1.cvar(NameRep_1.name('fail'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfun(types_1.tvar(NameRep_1.name('Unit')), types_1.tcomp(types_1.tvar(NameRep_1.name('t')), types_1.teffs(types_1.tvar(NameRep_1.name('Fail'))))))), elems_1.cvar(NameRep_1.name('runFail'), types_1.tforallps([[NameRep_1.name('e'), kinds_1.kEffs], [NameRep_1.name('t'), kinds_1.kType]], types_1.tfun(types_1.tfun(types_1.tvar(NameRep_1.name('Unit')), types_1.tcomp(types_1.tvar(NameRep_1.name('t')), types_1.teffsFrom([types_1.tvar(NameRep_1.name('Fail'))], types_1.tvar(NameRep_1.name('e'))))), types_1.tcomp(types_1.tapps(types_1.tvar(NameRep_1.name('Maybe')), types_1.tvar(NameRep_1.name('t'))), types_1.tvar(NameRep_1.name('e')))))), elems_1.ctvar(NameRep_1.name('State'), kinds_1.kEff), elems_1.cvar(NameRep_1.name('get'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfun(types_1.tvar(NameRep_1.name('Unit')), types_1.tcomp(types_1.tvar(NameRep_1.name('Nat')), types_1.teffs(types_1.tvar(NameRep_1.name('State'))))))), elems_1.cvar(NameRep_1.name('put'), types_1.tforallps([[NameRep_1.name('t'), kinds_1.kType]], types_1.tfun(types_1.tvar(NameRep_1.name('Nat')), types_1.tcomp(types_1.tvar(NameRep_1.name('Unit')), types_1.teffs(types_1.tvar(NameRep_1.name('State'))))))), elems_1.cvar(NameRep_1.name('runState'), types_1.tforallps([[NameRep_1.name('e'), kinds_1.kEffs], [NameRep_1.name('t'), kinds_1.kType]], types_1.tfunps(types_1.tvar(NameRep_1.name('Nat')), types_1.tfun(types_1.tfun(types_1.tvar(NameRep_1.name('Unit')), types_1.tcomp(types_1.tvar(NameRep_1.name('t')), types_1.teffsFrom([types_1.tvar(NameRep_1.name('State'))], types_1.tvar(NameRep_1.name('e'))))), types_1.tcomp(types_1.tvar(NameRep_1.name('t')), types_1.tvar(NameRep_1.name('e'))))))));
function _show(x) {
    if (typeof x === 'string')
        return JSON.stringify(x);
    if (typeof x === 'function')
        return '[Function]';
    if (typeof x._tag === 'string')
        return typeof x.val === 'undefined' ? x._tag :
            Array.isArray(x.val) ? `(${x._tag} ${x.val.map(_show).join(' ')})` :
                `(${x._tag} ${_show(x.val)})`;
    if (x._cont)
        return `(${x.op}(${_show(x.val)}))`;
    return '' + x;
}
let _ctx = exports._context;
function _run(i, cb) {
    try {
        console.log(i);
        const p = parser_1.default(i);
        console.log('' + p);
        let cex = null;
        const prog = surface_1.exprToCompIL(p)
            .chain(ex => {
            cex = ex;
            return TC_1.log(`${ex}`).then(inference_1.synthgenComp(ex));
        });
        const result = prog.run(_ctx, new NameSupply_1.default(0)).val.throw();
        console.log(`${result}`);
        if (cex) {
            const c = javascriptBackend_1.default(cex);
            console.log(c);
            const res = eval(c);
            cb(`${_show(res)} : ${result.pretty()}`);
        }
    }
    catch (e) {
        console.log(e);
        cb('' + e, true);
    }
}
exports.default = _run;

},{"./NameRep":2,"./NameSupply":3,"./il/TC":5,"./il/elems":8,"./il/inference":10,"./il/initial":11,"./il/javascriptBackend":12,"./il/kinds":13,"./il/types":15,"./surface/parser":22,"./surface/surface":23}],20:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Expr {
}
exports.default = Expr;
class Val extends Expr {
    constructor() {
        super(...arguments);
        this._type = 'Val';
    }
    isValue() {
        return true;
    }
}
exports.Val = Val;
class Var extends Val {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return this.name.toString();
    }
}
exports.Var = Var;
exports.vr = (name) => new Var(name);
exports.isVar = (expr) => expr instanceof Var;
class Abs extends Val {
    constructor(name, type, body) {
        super();
        this.name = name;
        this.type = type;
        this.body = body;
    }
    toString() {
        return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.body})` : `(\\${this.name} -> ${this.body})`;
    }
}
exports.Abs = Abs;
exports.abs = (name, body) => new Abs(name, null, body);
exports.absty = (name, type, body) => new Abs(name, type, body);
exports.abss = (ns, body) => ns.reduceRight((b, n) => exports.abs(n, b), body);
exports.abstys = (ns, body) => ns.reduceRight((b, [n, t]) => exports.absty(n, t, b), body);
exports.isAbs = (expr) => expr instanceof Abs;
class AbsT extends Val {
    constructor(name, kind, body) {
        super();
        this.name = name;
        this.kind = kind;
        this.body = body;
    }
    toString() {
        return `(/\\(${this.name} : ${this.kind}) -> ${this.body})`;
    }
}
exports.AbsT = AbsT;
exports.absT = (name, kind, body) => new AbsT(name, kind, body);
exports.absTs = (ns, body) => ns.reduceRight((b, [n, k]) => exports.absT(n, k, b), body);
exports.isAbsT = (expr) => expr instanceof AbsT;
class Anno extends Val {
    constructor(expr, type) {
        super();
        this.expr = expr;
        this.type = type;
    }
    toString() {
        return `(${this.expr} : ${this.type})`;
    }
}
exports.Anno = Anno;
exports.anno = (expr, type) => new Anno(expr, type);
exports.isAnno = (expr) => expr instanceof Anno;
class Comp extends Expr {
    constructor() {
        super(...arguments);
        this._type = 'Comp';
    }
    isValue() {
        return false;
    }
}
exports.Comp = Comp;
class Return extends Comp {
    constructor(val) {
        super();
        this.val = val;
    }
    toString() {
        return `(return ${this.val})`;
    }
}
exports.Return = Return;
exports.ret = (val) => new Return(val);
exports.isReturn = (expr) => expr instanceof Return;
class App extends Comp {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} ${this.right})`;
    }
}
exports.App = App;
exports.app = (left, right) => new App(left, right);
exports.appFrom = (es) => es.reduce(exports.app);
function apps(...es) { return exports.appFrom(es); }
exports.apps = apps;
exports.isApp = (expr) => expr instanceof App;
class AppT extends Comp {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} @${this.right})`;
    }
}
exports.AppT = AppT;
exports.appT = (left, right) => new AppT(left, right);
exports.appTs = (left, ts) => ts.reduce(exports.appT, left);
exports.isAppT = (expr) => expr instanceof AppT;
class Let extends Comp {
    constructor(name, expr, body) {
        super();
        this.name = name;
        this.expr = expr;
        this.body = body;
    }
    toString() {
        return `(let ${this.name} = ${this.expr} in ${this.body})`;
    }
}
exports.Let = Let;
exports.lt = (name, expr, body) => new Let(name, expr, body);
exports.lts = (ns, body) => ns.reduceRight((b, [n, e]) => exports.lt(n, e, b), body);
exports.isLet = (expr) => expr instanceof Let;

},{}],21:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Kind {
}
exports.default = Kind;
class KVar extends Kind {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return this.name.toString();
    }
}
exports.KVar = KVar;
exports.kvar = (name) => new KVar(name);
exports.isKVar = (kind) => kind instanceof KVar;
class KFun extends Kind {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} -> ${this.right})`;
    }
}
exports.KFun = KFun;
exports.kfun = (left, right) => new KFun(left, right);
exports.kfunFrom = (ks) => ks.reduceRight((x, y) => exports.kfun(y, x));
function kfuns(...ks) { return exports.kfunFrom(ks); }
exports.kfuns = kfuns;
exports.isKFun = (kind) => kind instanceof KFun;

},{}],22:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const utils_1 = require("../utils");
function matchingBracket(c) {
    if (c === '(')
        return ')';
    if (c === ')')
        return '(';
    if (c === '{')
        return '}';
    if (c === '}')
        return '{';
    if (c === '[')
        return ']';
    if (c === ']')
        return '[';
    return '';
}
const START = 0;
const NAME = 1;
function tokenize(s) {
    let state = START;
    let t = '';
    let r = [], p = [], b = [];
    for (let i = 0; i <= s.length; i++) {
        const c = s[i] || ' ';
        if (state === START) {
            if (/[a-z\:\_]/i.test(c))
                t += c, state = NAME;
            else if (c === '(' || c === '{' || c === '[')
                b.push(c), p.push(r), r = [];
            else if (c === ')' || c === '}' || c === ']') {
                if (b.length === 0)
                    throw new SyntaxError(`unmatched bracket: ${c}`);
                const br = b.pop();
                if (matchingBracket(br) !== c)
                    throw new SyntaxError(`unmatched bracket: ${br} and ${c}`);
                const a = p.pop();
                a.push({ tag: 'list', val: r, br });
                r = a;
            }
            else if (/\s+/.test(c))
                continue;
            else
                throw new SyntaxError(`invalid char: ${c}`);
        }
        else if (state === NAME) {
            if (!/[a-z0-9\_\!]/i.test(c))
                r.push({ tag: 'name', val: t }), t = '', i--, state = START;
            else
                t += c;
        }
    }
    if (state !== START)
        throw new SyntaxError(`invalid parsing end state: ${state}`);
    return r;
}
function exprs(r, br = '[') {
    switch (br) {
        case '(': return r.length === 0 ? exprs_1.vr('Unit') : r.length === 1 ? expr(r[0]) : exprs_1.appFrom(r.map(expr));
        case '[':
            if (r.length === 0)
                return exprs_1.vr('Nil');
            if (r.length === 1)
                return expr(r[0]);
            let n = null;
            let res = [];
            for (let i = 0; i < r.length - 1; i++) {
                const c = r[i];
                if (n === null) {
                    if (c.tag === 'name' && c.val[0] === ':') {
                        n = c.val.slice(1);
                    }
                    else {
                        res.push(['_', expr(c)]);
                    }
                }
                else {
                    if (c.tag === 'name' && c.val[0] === ':') {
                        res.push([n, exprs_1.vr('Unit')]);
                        n = null;
                        i--;
                    }
                    else {
                        res.push([n, expr(c)]);
                        n = null;
                    }
                }
            }
            return exprs_1.lts(res, expr(r[r.length - 1]));
        case '{':
            if (r.length === 0)
                return exprs_1.abs('x', exprs_1.vr('x'));
            if (r.length === 1)
                return exprs_1.abs('_', expr(r[0]));
            const args = r[0];
            if (args.tag !== 'list' || args.br !== '[')
                return exprs_1.abs('_', exprs(r, '('));
            if (utils_1.any(args.val, a => a.tag !== 'name'))
                throw new SyntaxError(`invalid args: ${args.val.join(' ')}`);
            return exprs_1.abss(args.val.map(a => a.tag === 'name' ? a.val : null).filter(Boolean), exprs(r.slice(1), '('));
    }
}
function expr(r) {
    switch (r.tag) {
        case 'name': return r.val[r.val.length - 1] === '!' ? exprs_1.app(exprs_1.vr(r.val.slice(0, -1)), exprs_1.vr('Unit')) : exprs_1.vr(r.val);
        case 'list': return exprs(r.val, r.br);
    }
}
function parse(s) {
    return exprs(tokenize(s), '(');
}
exports.default = parse;

},{"../utils":25,"./exprs":20}],23:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("../il/kinds");
const types_1 = require("../il/types");
const values_1 = require("../il/values");
const computations_1 = require("../il/computations");
const kinds_2 = require("./kinds");
const types_2 = require("./types");
const exprs_1 = require("./exprs");
const TC_1 = require("../il/TC");
const utils_1 = require("../backup/utils");
const NameRep_1 = require("../NameRep");
exports.kindToIL = (k) => {
    if (kinds_2.isKVar(k))
        return TC_1.default.of(kinds_1.kvar(NameRep_1.name(k.name)));
    if (kinds_2.isKFun(k))
        return exports.kindToIL(k.left).chain2((l, r) => TC_1.default.of(kinds_1.kfun(l, r)), exports.kindToIL(k.right));
    return utils_1.impossible();
};
exports.typeToIL = (t) => {
    if (types_2.isTVar(t))
        return TC_1.default.of(types_1.tvar(NameRep_1.name(t.name)));
    if (types_2.isTApp(t))
        return exports.typeToIL(t.left).chain2((l, r) => TC_1.default.of(types_1.tapp(l, r)), exports.typeToIL(t.right));
    if (types_2.isTFun(t))
        return exports.typeToIL(t.left).chain2((a, b) => TC_1.default.of(types_1.tfun(a, b)), exports.typeToIL(t.right));
    if (types_2.isTForall(t))
        return exports.kindToIL(t.kind).chain(k => exports.typeToIL(t.type).map(ty => types_1.tforall(NameRep_1.name(t.name), k, ty)));
    if (types_2.isTEffsExtend(t))
        return exports.typeToIL(t.type).chain2((l, r) => TC_1.default.of(types_1.teffsextend(l, r)), exports.typeToIL(t.rest));
    if (types_2.isTEffsEmpty(t))
        return TC_1.default.of(types_1.teffsempty());
    return utils_1.impossible();
};
exports.exprToCompIL = (e) => {
    if (e.isValue())
        return exports.exprToValIL(e).map(v => computations_1.ret(v));
    if (exprs_1.isReturn(e))
        return exports.exprToValIL(e.val).map(v => computations_1.ret(v));
    if (exprs_1.isApp(e)) {
        if (e.left.isValue() && e.right.isValue())
            return exports.exprToValIL(e.left)
                .chain(a => exports.exprToValIL(e.right)
                .map(b => computations_1.app(a, b)));
        if (e.left.isValue())
            return TC_1.freshName(NameRep_1.name('_'))
                .chain(x => exports.exprToValIL(e.left)
                .chain(a => exports.exprToCompIL(e.right)
                .map(b => computations_1.lt(x, b, computations_1.app(a, values_1.vr(x))))));
        if (e.right.isValue())
            return TC_1.freshName(NameRep_1.name('_'))
                .chain(x => exports.exprToCompIL(e.left)
                .chain(a => exports.exprToValIL(e.right)
                .map(b => computations_1.lt(x, a, computations_1.app(values_1.vr(x), b)))));
        return TC_1.freshNames([NameRep_1.name('_'), NameRep_1.name('_')])
            .chain(([x, y]) => exports.exprToCompIL(e.left)
            .chain(a => exports.exprToCompIL(e.right)
            .map(b => computations_1.lt(x, a, computations_1.lt(y, b, computations_1.app(values_1.vr(x), values_1.vr(y)))))));
    }
    if (exprs_1.isAppT(e)) {
        if (e.left.isValue())
            return exports.exprToValIL(e.left).chain(v => exports.typeToIL(e.right).map(t => computations_1.appT(v, t)));
        return TC_1.freshName(NameRep_1.name('_'))
            .chain(x => exports.exprToCompIL(e.left)
            .chain(c => exports.typeToIL(e.right)
            .map(t => computations_1.lt(x, c, computations_1.appT(values_1.vr(x), t)))));
    }
    if (exprs_1.isLet(e))
        return exports.exprToCompIL(e.expr).chain(a => exports.exprToCompIL(e.body).map(b => computations_1.lt(NameRep_1.name(e.name), a, b)));
    return utils_1.impossible();
};
exports.exprToValIL = (e) => {
    if (!e.isValue())
        return TC_1.error(`cannot convert to value: ${e}`);
    if (exprs_1.isVar(e))
        return TC_1.default.of(values_1.vr(NameRep_1.name(e.name)));
    if (exprs_1.isAbs(e))
        return e.type ?
            exports.typeToIL(e.type)
                .chain(type => exports.exprToCompIL(e.body)
                .map(body => values_1.absty(NameRep_1.name(e.name), type, body))) :
            exports.exprToCompIL(e.body)
                .map(body => values_1.abs(NameRep_1.name(e.name), body));
    if (exprs_1.isAbsT(e))
        return exports.kindToIL(e.kind).chain(k => exports.exprToCompIL(e.body).map(body => values_1.absT(NameRep_1.name(e.name), k, body)));
    if (exprs_1.isAnno(e))
        return exports.exprToValIL(e.expr).chain(ex => exports.typeToIL(e.type).map(ty => values_1.anno(ex, ty)));
    return utils_1.impossible();
};

},{"../NameRep":2,"../backup/utils":4,"../il/TC":5,"../il/computations":6,"../il/kinds":13,"../il/types":15,"../il/values":17,"./exprs":20,"./kinds":21,"./types":24}],24:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Type {
}
exports.default = Type;
class TVar extends Type {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return this.name.toString();
    }
}
exports.TVar = TVar;
exports.tvar = (name) => new TVar(name);
exports.isTVar = (type) => type instanceof TVar;
class TApp extends Type {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        const left = this.left;
        if (exports.isTApp(left) && exports.isTVar(left.left) && /[^a-z]/i.test(left.left.toString()[0]))
            return `(${left.right} ${left.left} ${this.right})`;
        return `(${left} ${this.right})`;
    }
}
exports.TApp = TApp;
exports.tapp = (left, right) => new TApp(left, right);
exports.tappsFrom = (ts) => ts.reduce(exports.tapp);
exports.tapps = (...ts) => exports.tappsFrom(ts);
exports.isTApp = (type) => type instanceof TApp;
exports.flattenTApp = (type) => {
    if (exports.isTApp(type)) {
        const rec = exports.flattenTApp(type.left);
        return { head: rec.head, tail: rec.tail.concat([type.right]) };
    }
    return { head: type, tail: [] };
};
exports.headTApp = (type) => exports.flattenTApp(type).head;
class TFun extends Type {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} -> ${this.right})`;
    }
}
exports.TFun = TFun;
exports.tfun = (left, right) => new TFun(left, right);
exports.tfunFrom = (ts) => ts.reduceRight((x, y) => exports.tfun(y, x));
function tfuns(...ts) { return exports.tfunFrom(ts); }
exports.tfuns = tfuns;
exports.isTFun = (type) => type instanceof TFun;
class TForall extends Type {
    constructor(name, kind, type) {
        super();
        this.name = name;
        this.kind = kind;
        this.type = type;
    }
    toString() {
        return `(forall(${this.name} : ${this.kind}). ${this.type})`;
    }
}
exports.TForall = TForall;
exports.tforall = (name, kind, type) => new TForall(name, kind, type);
exports.tforalls = (ns, type) => ns.reduceRight((t, [n, k]) => exports.tforall(n, k, t), type);
exports.isTForall = (type) => type instanceof TForall;
exports.flattenTForall = (type) => {
    if (exports.isTForall(type)) {
        const rec = exports.flattenTForall(type.type);
        return { ns: [[type.name, type.kind]].concat(rec.ns), type: rec.type };
    }
    return { ns: [], type };
};
class TEffsEmpty extends Type {
    constructor() { super(); }
    toString() {
        return `{}`;
    }
}
exports.TEffsEmpty = TEffsEmpty;
exports.teffsempty = () => new TEffsEmpty();
exports.isTEffsEmpty = (type) => type instanceof TEffsEmpty;
class TEffsExtend extends Type {
    constructor(type, rest) {
        super();
        this.type = type;
        this.rest = rest;
    }
    toString() {
        return `{ ${this.type} | ${this.rest} }`;
    }
}
exports.TEffsExtend = TEffsExtend;
exports.teffsextend = (type, rest) => new TEffsExtend(type, rest);
exports.teffsFrom = (ts, rest) => ts.reduceRight((a, b) => exports.teffsextend(b, a), rest || exports.teffsempty());
exports.teffs = (...ts) => exports.teffsFrom(ts);
exports.isTEffsExtend = (type) => type instanceof TEffsExtend;
exports.flattenEffs = (row) => {
    if (exports.isTEffsExtend(row)) {
        const rec = exports.flattenEffs(row.rest);
        return { types: [row.type].concat(rec.types), rest: rec.rest };
    }
    return { types: [], rest: row };
};

},{}],25:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.impossible = () => { throw new Error('impossible'); };
exports.assocGet = (arr, val) => {
    for (let i = arr.length - 1; i >= 0; i--) {
        if (arr[i][0].equals(val))
            return arr[i][1];
    }
    return null;
};
exports.containsDuplicate = (arr) => {
    const acc = [];
    for (let i = 0; i < arr.length; i++) {
        const c = arr[i];
        for (let j = 0; j < acc.length; j++) {
            if (acc[j].equals(c))
                return true;
        }
        acc.push(c);
    }
    return false;
};
exports.any = (arr, fn) => {
    for (let i = 0; i < arr.length; i++) {
        const c = arr[i];
        if (fn(c))
            return true;
    }
    return false;
};
exports.remove = (arr, fn) => {
    const ret = [];
    for (let i = 0; i < arr.length; i++) {
        const c = arr[i];
        if (!fn(c))
            ret.push(c);
    }
    return ret;
};

},{}],26:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const repl_1 = require("./repl");
function getOutput(s, cb) {
    repl_1.default(s, cb);
}
var hist = [], index = -1;
var input = document.getElementById('input');
var content = document.getElementById('content');
function onresize() {
    content.style.height = window.innerHeight;
}
window.addEventListener('resize', onresize);
onresize();
addResult("REPL");
input.focus();
input.onkeydown = function (keyEvent) {
    var val = input.value;
    var txt = (val || '').trim();
    if (keyEvent.keyCode === 13) {
        keyEvent.preventDefault();
        if (txt) {
            hist.push(val);
            index = hist.length;
            input.value = '';
            var div = document.createElement('div');
            div.innerHTML = val;
            div.className = 'line input';
            content.insertBefore(div, input);
            getOutput(txt, addResult);
        }
    }
    else if (keyEvent.keyCode === 38 && index > 0) {
        keyEvent.preventDefault();
        input.value = hist[--index];
    }
    else if (keyEvent.keyCode === 40 && index < hist.length - 1) {
        keyEvent.preventDefault();
        input.value = hist[++index];
    }
    else if (keyEvent.keyCode === 40 && keyEvent.ctrlKey && index >= hist.length - 1) {
        index = hist.length;
        input.value = '';
    }
};
function addResult(msg, err) {
    var divout = document.createElement('pre');
    divout.className = 'line output';
    if (err)
        divout.className += ' error';
    divout.innerHTML = '' + msg;
    content.insertBefore(divout, input);
    input.focus();
    content.scrollTop = content.scrollHeight;
    return divout;
}

},{"./repl":19}]},{},[26]);

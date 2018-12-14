(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const either_1 = require("./either");
exports.ok = either_1.Right(true);

},{"./either":3}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const either_1 = require("./either");
const TC_1 = require("./TC");
exports.extend = (map, key, val) => {
    const ret = Object.create(map);
    ret[key] = val;
    return ret;
};
exports.extendMut = (map, key, val) => {
    map[key] = val;
    return map;
};
exports.append = (a, b) => {
    const n = {};
    for (let k in a)
        n[k] = a[k];
    for (let k in b)
        n[k] = b[k];
    return n;
};
exports.appendMut = (a, b) => {
    for (let k in b)
        a[k] = b[k];
    return a;
};
exports.Context = (kvars = {}, tvars = {}, effs = {}, ops = {}, vars = {}) => ({ tag: 'Context', kvars, tvars, effs, ops, vars });
exports.findKVar = (ctx, name) => ctx.kvars[name] ? TC_1.ok : either_1.Left(`undefined kvar ${name}`);
exports.findTVar = (ctx, name) => {
    const ret = ctx.tvars[name];
    return ret ? either_1.Right(ret) : either_1.Left(`undefined tvar ${name}`);
};
exports.findEff = (ctx, name) => {
    const ret = ctx.effs[name];
    return ret ? either_1.Right(ret) : either_1.Left(`undefined eff ${name}`);
};
exports.findOp = (ctx, name) => {
    const ret = ctx.ops[name];
    return ret ? either_1.Right(ret) : either_1.Left(`undefined eff ${name}`);
};
exports.findVar = (ctx, name) => {
    const ret = ctx.vars[name];
    return ret ? either_1.Right(ret) : either_1.Left(`undefined var ${name}`);
};
exports.extendVar = (ctx, name, type) => exports.Context(ctx.kvars, ctx.tvars, ctx.effs, ctx.ops, exports.extend(ctx.vars, name, type));
exports.extendContextMut = (ctx, kvs, tvs, effs, ops, vs) => {
    exports.appendMut(ctx.kvars, kvs);
    exports.appendMut(ctx.tvars, tvs);
    exports.appendMut(ctx.effs, effs);
    exports.appendMut(ctx.ops, ops);
    exports.appendMut(ctx.vars, vs);
    return ctx;
};
exports.initial = exports.Context({
    [kinds_1.nType]: true,
    [kinds_1.nEff]: true,
    [kinds_1.nEffs]: true,
}, {
    [types_1.nFun]: kinds_1.kfun(kinds_1.kType, kinds_1.kEffs, kinds_1.kType, kinds_1.kType),
    [types_1.nEffsEmpty]: kinds_1.kEffs,
    [types_1.nEffsExtend]: kinds_1.kfun(kinds_1.kEff, kinds_1.kEffs, kinds_1.kEffs),
    Unit: kinds_1.kType,
}, {}, {}, {
    Unit: types_1.Forall([], types_1.TVar('Unit')),
});

},{"./TC":1,"./either":3,"./kinds":7,"./types":11}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
;
exports.Left = (val) => ({ tag: 'Left', val });
;
exports.Right = (val) => ({ tag: 'Right', val });
exports.isLeft = (val) => val.tag === 'Left';
exports.isRight = (val) => val.tag === 'Right';
exports.caseOf = (val, cs) => val.tag === 'Left' ? cs.Left(val.val) : cs.Right(val.val);
exports.showEither = (val, showVal = x => `${x}`, showErr = x => `${x}`) => exports.caseOf(val, {
    Left: val => `Left(${showErr(val)})`,
    Right: val => `Right(${showVal(val)})`,
});
exports.mapEither = (val, fn) => exports.caseOf(val, {
    Left: () => val,
    Right: val => exports.Right(fn(val)),
});
exports.throwEither = (val) => exports.caseOf(val, {
    Left: val => { throw val; },
    Right: val => val,
});

},{}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
;
exports.Var = (name) => ({ tag: 'Var', name });
;
exports.Abs = (arg, type, body) => ({ tag: 'Abs', arg, type, body });
exports.abs = (ns, body) => ns.reduceRight((a, b) => exports.Abs(b, null, a), body);
exports.abst = (ns, body) => ns.reduceRight((a, [b, t]) => exports.Abs(b, t, a), body);
;
exports.App = (left, right) => ({ tag: 'App', left, right });
exports.app = (...es) => es.reduce(exports.App);
exports.apps = (fn, args) => [fn].concat(args).reduce(exports.App);
exports.Handler = (handler) => ({ tag: 'Handler', handler });
exports.HOp = (op, expr, rest) => ({ tag: 'HOp', op, expr, rest });
exports.HReturn = (expr) => ({ tag: 'HReturn', expr });
exports.caseHandler = (val, cs) => {
    switch (val.tag) {
        case 'HOp': return cs.HOp(val.op, val.expr, val.rest);
        case 'HReturn': return cs.HReturn(val.expr);
    }
};
exports.showHandler = (val) => exports.caseHandler(val, {
    HOp: (op, expr) => `${op} -> ${exports.showExpr(expr)}`,
    HReturn: (expr) => `return -> ${exports.showExpr(expr)}`,
});
exports.caseExpr = (val, cs) => {
    switch (val.tag) {
        case 'Var': return cs.Var(val.name);
        case 'Abs': return cs.Abs(val.arg, val.type, val.body);
        case 'App': return cs.App(val.left, val.right);
        case 'Handler': return cs.Handler(val.handler);
    }
};
exports.showExpr = (expr) => exports.caseExpr(expr, {
    Var: name => `${name}`,
    Abs: (arg, type, body) => type ? `(\\(${arg} : ${types_1.showType(type)}) -> ${exports.showExpr(body)})` :
        `(\\${arg} -> ${exports.showExpr(body)})`,
    App: (left, right) => `(${exports.showExpr(left)} ${exports.showExpr(right)})`,
    Handler: (handler) => {
        const r = [];
        let c = handler;
        while (c.tag === 'HOp') {
            r.push([c.op, exports.showExpr(c.expr)]);
            c = c.rest;
        }
        r.push(['return', exports.showExpr(c.expr)]);
        return `(handler {${r.map(([x, e]) => `${x} -> ${e}`).join(', ')}})`;
    },
});
exports.Let = (x, a, b) => exports.App(exports.Abs(x, null, b), a);
exports.letTy = (x, t, a, b) => exports.App(exports.Abs(x, t, b), a);
exports.lets = (xs, b) => exports.apps(exports.abs(xs.map(x => x[0]), b), xs.map(x => x[1]));
exports.letTys = (xs, b) => exports.apps(exports.abst(xs.map(x => [x[0], x[1]]), b), xs.map(x => x[2]));
exports.Anno = (expr, type) => exports.App(exports.Abs('x', type, exports.Var('x')), expr);

},{"./types":11}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const TC_1 = require("./TC");
const types_1 = require("./types");
const context_1 = require("./context");
const either_1 = require("./either");
const unification_1 = require("./unification");
const names_1 = require("./names");
const kinds_1 = require("./kinds");
const openFun = (type) => {
    const f = types_1.matchTFun(type);
    if (!f)
        return type;
    console.log(`openFun ${types_1.showType(type)}`);
    const fl = types_1.flattenTEffs(f.eff);
    if (!types_1.isTEffsEmpty(fl.rest))
        return type;
    const ret = types_1.TFun(f.left, types_1.teffs(fl.ts, types_1.freshMeta('e', kinds_1.kEffs)), openFun(f.right));
    console.log(`opened ${types_1.showType(ret)}`);
    return ret;
};
const instantiate = (type) => {
    const sub = {};
    const args = type.args;
    for (let i = args.length - 1; i >= 0; i--) {
        const c = args[i];
        const n = c[0];
        const k = c[1];
        sub[n] = types_1.freshMeta(n, k);
    }
    return openFun(types_1.substTVar(sub, type.type));
};
exports.infer = (ctx, expr) => {
    console.log(`infer ${exprs_1.showExpr(expr)}`);
    return exprs_1.caseExpr(expr, {
        Var: name => {
            const ty = context_1.findVar(ctx, name);
            if (either_1.isLeft(ty))
                return ty;
            return either_1.Right(types_1.typeEff(instantiate(ty.val), types_1.freshMeta('e', kinds_1.kEffs)));
        },
        Abs: (arg, type, body) => {
            const m = type || types_1.freshMeta(arg, kinds_1.kType);
            const res = exports.infer(context_1.extendVar(ctx, arg, types_1.Forall([], m)), body);
            if (either_1.isLeft(res))
                return res;
            const ty = types_1.typeEff(types_1.TFun(unification_1.prune(m), res.val.eff, res.val.type), types_1.freshMeta('e', kinds_1.kEffs));
            return either_1.Right(ty);
        },
        App: (left, right) => {
            const lr = exports.infer(ctx, left);
            if (either_1.isLeft(lr))
                return lr;
            const rr = exports.infer(ctx, right);
            if (either_1.isLeft(rr))
                return rr;
            const m = types_1.freshMeta('t', kinds_1.kType);
            const e = types_1.freshMeta('e', kinds_1.kEffs);
            const ur = unification_1.unify(ctx, lr.val.type, types_1.TFun(rr.val.type, e, m), false);
            if (either_1.isLeft(ur))
                return ur;
            const ure1 = unification_1.unify(ctx, e, lr.val.eff);
            if (either_1.isLeft(ure1))
                return ure1;
            const ure2 = unification_1.unify(ctx, e, rr.val.eff);
            if (either_1.isLeft(ure2))
                return ure2;
            const ty = types_1.typeEff(unification_1.prune(m), unification_1.prune(e));
            return either_1.Right(ty);
        },
        Handler: (handler) => {
            const effs = {};
            const a = types_1.freshMeta('a');
            const e = types_1.freshMeta('e', kinds_1.kEffs);
            const b = types_1.freshMeta('b');
            const re = types_1.freshMeta('e', kinds_1.kEffs);
            const ret = inferHandler(ctx, handler, a, e, b, re, effs);
            if (either_1.isLeft(ret))
                return ret;
            const allEffs = [];
            const incompleteEffs = [];
            for (let eff in effs) {
                allEffs.push(eff);
                const compl = checkEffComplete(ctx, eff, effs[eff]);
                if (either_1.isLeft(compl))
                    return compl;
                if (!compl.val)
                    incompleteEffs.push(eff);
            }
            const fe = types_1.teffs(incompleteEffs.map(types_1.TVar), types_1.freshMeta('e', kinds_1.kEffs));
            const u2 = unification_1.unify(ctx, fe, unification_1.prune(e));
            if (either_1.isLeft(u2))
                return u2;
            const pfe = unification_1.prune(fe);
            const rest = types_1.flattenTEffs(pfe).rest;
            return either_1.Right(types_1.typeEff(types_1.TFun(types_1.TFun(types_1.TVar('Unit'), types_1.teffs(allEffs.map(types_1.TVar), rest), unification_1.prune(a)), pfe, unification_1.prune(b)), unification_1.prune(re)));
        },
    });
};
const checkEffComplete = (ctx, eff, ops) => {
    const ret = context_1.findEff(ctx, eff);
    if (either_1.isLeft(ret))
        return ret;
    const allops = ret.val;
    for (let op in allops) {
        if (!ops[op])
            return either_1.Right(false);
    }
    return either_1.Right(true);
};
const inferHandler = (ctx, cs, a, e, b, re, effs) => {
    console.log(`inferHandler ${exprs_1.showHandler(cs)}`);
    return exprs_1.caseHandler(cs, {
        HOp: (op, expr, rest) => {
            const info = context_1.findOp(ctx, op);
            if (either_1.isLeft(info))
                return info;
            const eff = info.val.eff;
            if (!effs[eff])
                effs[eff] = {};
            if (effs[eff][op])
                return either_1.Left(`duplicat op in handler: ${op}`);
            effs[eff][op] = true;
            const retrest = inferHandler(ctx, rest, a, e, b, re, effs);
            if (either_1.isLeft(retrest))
                return retrest;
            const res = exports.infer(ctx, expr);
            if (either_1.isLeft(res))
                return res;
            const pb = unification_1.prune(b);
            const pe = unification_1.prune(e);
            const u1 = unification_1.unify(ctx, types_1.TFunP(info.val.param, types_1.TFun(types_1.TFun(info.val.ret, pe, pb), pe, pb)), res.val.type);
            if (either_1.isLeft(u1))
                return u1;
            const u2 = unification_1.unify(ctx, re, res.val.eff, true);
            if (either_1.isLeft(u2))
                return u2;
            return TC_1.ok;
        },
        HReturn: expr => {
            const res = exports.infer(ctx, expr);
            if (either_1.isLeft(res))
                return res;
            const u1 = unification_1.unify(ctx, types_1.TFun(a, e, b), res.val.type);
            if (either_1.isLeft(u1))
                return u1;
            const u2 = unification_1.unify(ctx, re, unification_1.prune(res.val.eff));
            if (either_1.isLeft(u2))
                return u2;
            return TC_1.ok;
        },
    });
};
const closeFunTy = (type, occ, closed = []) => {
    const fn = types_1.matchTFun(type);
    if (fn) {
        const rec = closeFunTy(fn.right, occ, closed);
        const fl = types_1.flattenTEffs(fn.eff);
        if (fl.rest.tag === 'TVar' && occ[fl.rest.name] === 1) {
            closed.push(fl.rest.name);
            return types_1.TFun(fn.left, types_1.teffs(fl.ts, types_1.tEffsEmpty), rec);
        }
        else
            return types_1.TFun(fn.left, fn.eff, rec);
    }
    return type;
};
const closeFun = (type) => {
    console.log(`closeFun ${types_1.showForall(type)}`);
    const args = type.args;
    const effs = args.filter(([n, k]) => kinds_1.eqKind(k, kinds_1.kEffs)).map(([n, _]) => n);
    const occ = types_1.occTVar(type.type, effs);
    const closed = [];
    const ret = closeFunTy(type.type, occ, closed);
    const retty = types_1.Forall(args.filter(([n, k]) => closed.indexOf(n) === -1), ret);
    console.log(`closed ${types_1.showForall(retty)}`);
    return retty;
};
const generalize = (ctx, typeE, topLevel = false) => {
    if (!types_1.isTEffsEmpty(typeE.eff) && typeE.eff.tag !== 'TMeta')
        return either_1.Left(`cannot generalize over ${types_1.showTypeEff(typeE)}`);
    const type = typeE.type;
    const fr = types_1.freeMeta(type);
    if (!topLevel) {
        for (let name in ctx.vars) {
            for (let k in types_1.freeMeta(ctx.vars[name].type)) {
                fr[k] = undefined;
            }
        }
    }
    const args = [];
    const sub = {};
    for (let k in fr) {
        args.push([k, fr[k].kind]);
        sub[k] = types_1.TVar(k);
    }
    return either_1.Right(closeFun(types_1.Forall(args, types_1.substMeta(sub, type))));
};
exports.inferGen = (ctx, expr, topLevel = false) => {
    names_1.resetId();
    const ty = exports.infer(ctx, expr);
    if (either_1.isLeft(ty))
        return ty;
    console.log(`infer done ${types_1.showTypeEff(ty.val)}`);
    const f = generalize(ctx, ty.val, topLevel);
    if (either_1.isLeft(f))
        return f;
    return either_1.Right(f.val);
};

},{"./TC":1,"./context":2,"./either":3,"./exprs":4,"./kinds":7,"./names":8,"./types":11,"./unification":12}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const compileHandlerToJS = (e) => exprs_1.caseHandler(e, {
    HOp: (op, expr, rest) => `${op}: ${compileToJS(expr)}, ${compileHandlerToJS(rest)}`,
    HReturn: expr => `return: ${compileToJS(expr)}`,
});
const compileToJS = (e) => exprs_1.caseExpr(e, {
    Var: name => `${name}`,
    Abs: (name, _, body) => `(${name} => ${compileToJS(body)})`,
    App: (left, right) => `$(${compileToJS(left)}, ${compileToJS(right)})`,
    Handler: cs => `_newhandler({${compileHandlerToJS(cs)}})`,
});
exports.default = compileToJS;

},{"./exprs":4}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
;
exports.KVar = (name) => ({ tag: 'KVar', name });
;
exports.KFun = (left, right) => ({ tag: 'KFun', left, right });
exports.kfun = (...ks) => ks.reduceRight((a, b) => exports.KFun(b, a));
exports.caseKind = (val, cs) => {
    switch (val.tag) {
        case 'KVar': return cs.KVar(val.name);
        case 'KFun': return cs.KFun(val.left, val.right);
    }
};
exports.showKind = (type) => exports.caseKind(type, {
    KVar: name => `${name}`,
    KFun: (left, right) => `(${exports.showKind(left)} -> ${exports.showKind(right)})`,
});
exports.eqKind = (a, b) => exports.caseKind(a, {
    KVar: name => b.tag === 'KVar' && b.name === name,
    KFun: (left, right) => b.tag === 'KFun' && exports.eqKind(left, b.left) && exports.eqKind(right, b.right),
});
exports.nType = 'Type';
exports.kType = exports.KVar(exports.nType);
exports.nEff = 'Eff';
exports.kEff = exports.KVar(exports.nEff);
exports.nEffs = 'Effs';
exports.kEffs = exports.KVar(exports.nEffs);
exports.flattenKFun = (kind) => {
    let c = kind;
    const r = [];
    while (c.tag === 'KFun') {
        r.push(c.left);
        c = c.right;
    }
    r.push(c);
    return r;
};
const KARR = '->';
exports.prettyKind = (kind) => {
    if (kind.tag === 'KVar')
        return kind.name;
    const f = exports.flattenKFun(kind);
    return f.map(k => k.tag === 'KFun' ? `(${exports.prettyKind(k)})` : exports.prettyKind(k)).join(` ${KARR} `);
};

},{}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
let id = 0;
exports.resetId = () => { id = 0; };
exports.fresh = (name = '_') => `${name.split('$')[0]}\$${id++}`;
exports.namePart = (name) => name.split('$')[0];

},{}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const exprs_1 = require("./exprs");
const utils_1 = require("./utils");
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
            if (/[a-z\:\_\?]/i.test(c))
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
function createHandler(r) {
    if (r.length === 0 || r.length % 2 !== 0)
        throw new SyntaxError(`invalid handler`);
    const map = [];
    let ret = null;
    for (let i = 0; i < r.length; i += 2) {
        const k = r[i];
        const v = r[i + 1];
        if (k.tag === 'name') {
            if (k.val === 'return')
                ret = expr(v);
            else
                map.push([k.val, expr(v)]);
        }
        else
            throw new SyntaxError('invalid op in handler');
    }
    const retcase = exprs_1.HReturn(ret || exprs_1.Abs('x', null, exprs_1.Var('x')));
    return exprs_1.Handler(map.reduceRight((p, [op, e]) => exprs_1.HOp(op, e, p), retcase));
}
function exprs(r, br = '[') {
    console.log('exprs', r, br);
    switch (br) {
        case '(': return r.length === 0 ? exprs_1.Var('Unit') : r.length === 1 ? expr(r[0]) : exprs_1.apps(expr(r[0]), r.slice(1).map(expr));
        case '[':
            if (r.length === 0)
                return exprs_1.Var('Nil');
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
                        res.push([n, exprs_1.Var('Unit')]);
                        n = null;
                        i--;
                    }
                    else {
                        res.push([n, expr(c)]);
                        n = null;
                    }
                }
            }
            return exprs_1.lets(res, expr(r[r.length - 1]));
        case '{':
            if (r.length === 0)
                return exprs_1.Abs('x', null, exprs_1.Var('x'));
            if (r.length === 1)
                return exprs_1.Abs('_', null, expr(r[0]));
            const args = r[0];
            if (args.tag === 'name' && args.val === 'handler')
                return createHandler(r.slice(1));
            if (args.tag !== 'list' || args.br !== '[' || args.val.length === 0)
                return exprs_1.Abs('_', null, exprs(r, '('));
            if (utils_1.any(args.val, a => a.tag !== 'name'))
                throw new SyntaxError(`invalid args: ${args.val.join(' ')}`);
            const argss = [];
            for (let i = 0; i < args.val.length; i += 2) {
                const a = args.val[i];
                const b = args.val[i + 1];
                if (!b) {
                    argss.push([a.val, null]);
                    continue;
                }
                if (a.tag !== 'name' || b.tag !== 'name')
                    throw new SyntaxError(`invalid arg for abs`);
                if (b.val[0] === ':') {
                    argss.push([a.val, type({ tag: 'name', val: b.val.slice(1) })]);
                }
                else {
                    argss.push([a.val, null]);
                    argss.push([b.val, null]);
                }
            }
            return argss.reduceRight((b, [a, t]) => t ? exprs_1.Abs(a, t, b) : exprs_1.Abs(a, null, b), exprs(r.slice(1), '('));
    }
}
function expr(r) {
    console.log('expr', r);
    switch (r.tag) {
        case 'name': return r.val[r.val.length - 1] === '!' ? exprs_1.App(exprs_1.Var(r.val.slice(0, -1)), exprs_1.Var('Unit')) : exprs_1.Var(r.val);
        case 'list': return exprs(r.val, r.br);
    }
}
function type(r) {
    switch (r.tag) {
        case 'name': return r.val[0] === '?' ? types_1.freshMeta(r.val.slice(1)) : types_1.TVar(r.val);
    }
    throw new SyntaxError('unimplemented type parsing');
}
function parse(s) {
    return exprs(tokenize(s), '(');
}
exports.default = parse;

},{"./exprs":4,"./types":11,"./utils":13}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const exprs_1 = require("./exprs");
const context_1 = require("./context");
const kinds_1 = require("./kinds");
const inference_1 = require("./inference");
const javascriptBackend_1 = require("./javascriptBackend");
const parser_1 = require("./parser");
const either_1 = require("./either");
const v = exprs_1.Var;
const tv = types_1.TVar;
const _context = context_1.extendContextMut(context_1.initial, {}, {
    Void: kinds_1.kType,
    Bool: kinds_1.kType,
    Pair: kinds_1.kfun(kinds_1.kType, kinds_1.kType, kinds_1.kType),
    List: kinds_1.kfun(kinds_1.kType, kinds_1.kType),
    Flip: kinds_1.kEff,
    State: kinds_1.kEff,
}, {
    Flip: { flip: true },
    State: { get: true, put: true },
}, {
    flip: { eff: 'Flip', param: tv('Unit'), ret: tv('Bool') },
    get: { eff: 'State', param: tv('Unit'), ret: tv('Bool') },
    put: { eff: 'State', param: tv('Bool'), ret: tv('Unit') },
}, {
    Void: types_1.Forall([['t', kinds_1.kType]], types_1.tfun(tv('Void'), tv('t'))),
    True: types_1.Forall([], tv('Bool')),
    False: types_1.Forall([], tv('Bool')),
    Pair: types_1.Forall([['a', kinds_1.kType], ['b', kinds_1.kType]], types_1.tfun(tv('a'), tv('b'), types_1.tapp(tv('Pair'), tv('a'), tv('b')))),
    fst: types_1.Forall([['a', kinds_1.kType], ['b', kinds_1.kType]], types_1.tfun(types_1.tapp(tv('Pair'), tv('a'), tv('b')), tv('a'))),
    snd: types_1.Forall([['a', kinds_1.kType], ['b', kinds_1.kType]], types_1.tfun(types_1.tapp(tv('Pair'), tv('a'), tv('b')), tv('b'))),
    not: types_1.Forall([], types_1.tfun(tv('Bool'), tv('Bool'))),
    id: types_1.Forall([['t', kinds_1.kType]], types_1.tfun(tv('t'), tv('t'))),
    cnst: types_1.Forall([['a', kinds_1.kType], ['b', kinds_1.kType]], types_1.tfun(tv('a'), tv('b'), tv('a'))),
    Nil: types_1.Forall([['t', kinds_1.kType]], types_1.tapp(tv('List'), tv('t'))),
    Cons: types_1.Forall([['t', kinds_1.kType]], types_1.tfun(tv('t'), types_1.tapp(tv('List'), tv('t')), types_1.tapp(tv('List'), tv('t')))),
    flip: types_1.Forall([], types_1.TFun(tv('Unit'), types_1.teffs([tv('Flip')]), tv('Bool'))),
    get: types_1.Forall([], types_1.TFun(tv('Unit'), types_1.teffs([tv('State')]), tv('Bool'))),
    put: types_1.Forall([], types_1.TFun(tv('Bool'), types_1.teffs([tv('State')]), tv('Unit'))),
    fix: types_1.Forall([['t', kinds_1.kType]], types_1.tfun(types_1.tfun(tv('t'), tv('t')), tv('t'))),
    caseList: types_1.Forall([['t', kinds_1.kType], ['r', kinds_1.kType], ['e', kinds_1.kEffs]], types_1.tfun(tv('r'), types_1.tfun(tv('t'), types_1.TFun(types_1.tapp(tv('List'), tv('t')), tv('e'), tv('r'))), types_1.TFun(types_1.tapp(tv('List'), tv('t')), tv('e'), tv('r')))),
});
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
        return `${x.op}(${_show(x.val)})`;
    return '' + x;
}
let _ctx = _context;
function _run(i, cb) {
    try {
        console.log(i);
        const p = parser_1.default(i);
        console.log(exprs_1.showExpr(p));
        const result = either_1.throwEither(inference_1.inferGen(_ctx, p));
        console.log(`${types_1.showForall(result)}`);
        const c = javascriptBackend_1.default(p);
        console.log(c);
        const res = eval(c);
        cb(`${_show(res)} : ${types_1.prettyForall(result)}`);
    }
    catch (e) {
        console.log(e);
        cb('' + e, true);
    }
}
exports.default = _run;

},{"./context":2,"./either":3,"./exprs":4,"./inference":5,"./javascriptBackend":6,"./kinds":7,"./parser":9,"./types":11}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const kinds_1 = require("./kinds");
;
exports.TVar = (name) => ({ tag: 'TVar', name });
;
exports.TMeta = (name, kind, type = null) => ({ tag: 'TMeta', name, kind, type });
exports.freshMeta = (name = 't', kind = kinds_1.kType) => exports.TMeta(names_1.fresh(name), kind);
;
exports.TApp = (left, right, kind = null) => ({ tag: 'TApp', left, right, kind });
exports.tapp = (...ts) => ts.reduce((a, b) => exports.TApp(a, b));
exports.flattenTApp = (type, ts = []) => {
    if (type.tag === 'TApp') {
        const rec = exports.flattenTApp(type.left, ts);
        ts.push(type.right);
        return ts;
    }
    ts.push(type);
    return ts;
};
exports.caseType = (val, cs) => {
    switch (val.tag) {
        case 'TVar': return cs.TVar(val.name);
        case 'TMeta': return cs.TMeta(val.name, val.kind, val.type);
        case 'TApp': return cs.TApp(val.left, val.right, val.kind);
    }
};
exports.showType = (type) => exports.caseType(type, {
    TVar: name => `${name}`,
    TMeta: name => `?${name}`,
    TApp: (left, right) => left.tag === 'TApp' && left.left.tag === 'TApp' && left.left.left.tag === 'TVar' && left.left.left.name === '->' ?
        (left.right.tag === 'TVar' && left.right.name === '{}' ?
            `(${exports.showType(left.left.right)} -> ${exports.showType(right)})` :
            `(${exports.showType(left.left.right)} -> ${exports.showType(right)}!${exports.showType(left.right)})`) :
        left.tag === 'TApp' && left.left.tag === 'TVar' && /[^a-z]/i.test(left.left.name[0]) && left.left.name !== '->' ?
            `(${exports.showType(left.right)} ${left.left.name} ${exports.showType(right)})` :
            `(${exports.showType(left)} ${exports.showType(right)})`,
});
exports.substMeta = (sub, type) => exports.caseType(type, {
    TVar: name => type,
    TMeta: name => sub[name] ? sub[name] : type,
    TApp: (left, right) => exports.TApp(exports.substMeta(sub, left), exports.substMeta(sub, right)),
});
exports.substTVar = (sub, type) => exports.caseType(type, {
    TVar: name => sub[name] ? sub[name] : type,
    TMeta: name => type,
    TApp: (left, right) => exports.TApp(exports.substTVar(sub, left), exports.substTVar(sub, right)),
});
exports.freeMeta = (type, fr = {}) => exports.caseType(type, {
    TVar: name => fr,
    TMeta: name => { fr[name] = type; return fr; },
    TApp: (left, right) => exports.freeMeta(right, exports.freeMeta(left, fr)),
});
exports.containsMeta = (m, type) => exports.caseType(type, {
    TVar: name => false,
    TMeta: name => name === m,
    TApp: (left, right) => exports.containsMeta(m, left) || exports.containsMeta(m, right),
});
exports.occTVar = (type, tvs, map = {}) => exports.caseType(type, {
    TVar: name => tvs.indexOf(name) >= 0 ? ((map[name] = (map[name] || 0) + 1), map) : map,
    TMeta: name => map,
    TApp: (left, right) => exports.occTVar(right, tvs, exports.occTVar(left, tvs, map)),
});
;
exports.Forall = (args, type) => ({ tag: 'Forall', args, type });
exports.showForall = (forall) => forall.args.length === 0 ?
    exports.showType(forall.type) :
    `forall${forall.args.map(([n, k]) => `(${n} : ${kinds_1.showKind(k)})`).join('')}. ${exports.showType(forall.type)}`;
exports.nEffsEmpty = '{}';
exports.tEffsEmpty = exports.TVar(exports.nEffsEmpty);
exports.isTEffsEmpty = (type) => type === exports.tEffsEmpty || (type.tag === 'TVar' && type.name === exports.nEffsEmpty);
exports.nEffsExtend = '|';
exports.tEffsExtend = exports.TVar(exports.nEffsExtend);
exports.TEffsExtend = (a, b) => exports.TApp(exports.TApp(exports.tEffsExtend, a), b);
exports.matchTEffsExtend = (type) => type.tag === 'TApp' && type.left.tag === 'TApp' &&
    (type.left.left === exports.tEffsExtend || (type.left.left.tag === 'TVar' && type.left.left.name === exports.nEffsExtend)) ?
    { eff: type.left.right, rest: type.right } : null;
exports.teffs = (es, rest = exports.tEffsEmpty) => es.reduceRight((a, b) => exports.TEffsExtend(b, a), rest);
exports.flattenTEffs = (type, ts = []) => {
    const m = exports.matchTEffsExtend(type);
    if (m) {
        ts.push(m.eff);
        const rec = exports.flattenTEffs(m.rest, ts);
        return { ts, rest: rec.rest };
    }
    return { ts, rest: type };
};
exports.nFun = '->';
exports.tFun = exports.TVar(exports.nFun);
exports.TFun = (a, e, b) => exports.TApp(exports.TApp(exports.TApp(exports.tFun, a), e), b);
exports.TFunP = (a, b) => exports.TFun(a, exports.tEffsEmpty, b);
exports.tfun = (...ts) => ts.reduceRight((a, b) => exports.TFunP(b, a));
exports.matchTFun = (type) => type.tag === 'TApp' && type.left.tag === 'TApp' && type.left.left.tag === 'TApp' &&
    (type.left.left.left === exports.tFun || (type.left.left.left.tag === 'TVar' && type.left.left.left.name === exports.nFun)) ?
    { left: type.left.left.right, eff: type.left.right, right: type.right } : null;
exports.flattenTFun = (type, ts = []) => {
    const f = exports.matchTFun(type);
    if (f) {
        if (exports.isTEffsEmpty(f.eff)) {
            ts.push(f.left);
            const rec = exports.flattenTFun(f.right, ts);
            return { ts, eff: rec.eff };
        }
        else {
            ts.push(f.left);
            ts.push(f.right);
            return { ts, eff: f.eff };
        }
    }
    ts.push(type);
    return { ts, eff: exports.tEffsEmpty };
};
const ARR = '->';
exports.prettyType = (type) => {
    const f = exports.matchTFun(type);
    if (f) {
        const fl = exports.flattenTFun(type);
        return `${fl.ts.map(t => exports.matchTFun(t) ? `(${exports.prettyType(t)})` : exports.prettyType(t)).join(` ${ARR} `)}${exports.isTEffsEmpty(fl.eff) ? '' : `!${exports.prettyType(fl.eff)}`}`;
    }
    if (exports.isTEffsEmpty(type))
        return '{}';
    const m = exports.matchTEffsExtend(type);
    if (m) {
        const fm = exports.flattenTEffs(type);
        return `{${fm.ts.map(exports.prettyType).join(', ')}${exports.isTEffsEmpty(fm.rest) ? '' : ` | ${exports.prettyType(fm.rest)}`}}`;
    }
    if (type.tag === 'TApp') {
        const ta = exports.flattenTApp(type);
        return `${ta.map(t => t.tag === 'TApp' || exports.matchTFun(t) ? `(${exports.prettyType(t)})` : exports.prettyType(t)).join(' ')}`;
    }
    return exports.showType(type);
};
const FORALL = 'forall';
exports.prettyForall = (forall) => {
    if (forall.args.length === 0)
        return exports.prettyType(forall.type);
    const ns = forall.args.map(x => x[0]);
    const map = {};
    const sub = {};
    for (let i = 0; i < ns.length; i++) {
        const on = ns[i];
        const n = names_1.namePart(on);
        if (!map[n])
            map[n] = 0;
        const j = map[n]++;
        sub[on] = exports.TVar(`${n}${j > 0 ? `\$${j}` : ''}`);
    }
    const nargs = forall.args.map(([x, k]) => [sub[x].name, k]);
    return `${FORALL} ${nargs.map(([n, k]) => kinds_1.eqKind(kinds_1.kType, k) ? `${n}` : `(${n} : ${kinds_1.prettyKind(k)})`).join(' ')}. ${exports.prettyType(exports.substTVar(sub, forall.type))}`;
};
exports.typeEff = (type, eff) => ({ type, eff });
exports.typePure = (type) => exports.typeEff(type, exports.tEffsEmpty);
exports.showTypeEff = (type) => `${exports.showType(type.type)}!${exports.showType(type.eff)}`;

},{"./kinds":7,"./names":8}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const context_1 = require("./context");
const kinds_1 = require("./kinds");
const TC_1 = require("./TC");
const either_1 = require("./either");
exports.prune = (type) => types_1.caseType(type, {
    TVar: name => type,
    TMeta: (name, kind, ty) => {
        if (!ty)
            return type;
        const t = exports.prune(ty);
        if (t !== ty)
            type.type = t;
        return t;
    },
    TApp: (left, right, kind) => {
        const l = exports.prune(left);
        const r = exports.prune(right);
        return l === left && r === right ? type : types_1.TApp(l, r, kind);
    },
});
exports.kindOf = (ctx, type) => types_1.caseType(type, {
    TVar: name => context_1.findTVar(ctx, name),
    TMeta: (name, kind) => either_1.Right(kind),
    TApp: (left, right, kind) => {
        if (kind)
            return either_1.Right(kind);
        const kle = exports.kindOf(ctx, left);
        if (either_1.isLeft(kle))
            return kle;
        const kl = kle.val;
        if (kl.tag !== 'KFun')
            return either_1.Left(`not a hkt in left side of ${types_1.showType(type)}`);
        const kre = exports.kindOf(ctx, right);
        if (either_1.isLeft(kre))
            return kre;
        const kr = kre.val;
        if (!kinds_1.eqKind(kl.left, kr))
            return either_1.Left(`invalid argument kind in ${types_1.showType(type)}`);
        type.kind = kl.right;
        return either_1.Right(kl.right);
    },
});
const occurs = (m, type) => types_1.caseType(type, {
    TVar: name => false,
    TMeta: name => name === m,
    TApp: (left, right) => occurs(m, left) || occurs(m, right),
});
exports.bind = (meta, type) => {
    console.log(`${types_1.showType(meta)} := ${types_1.showType(type)}`);
    if (type.tag === 'TMeta' && meta.name === type.name)
        return TC_1.ok;
    if (occurs(meta.name, type))
        return either_1.Left(`${types_1.showType(meta)} occurs in ${types_1.showType(type)}`);
    meta.type = type;
    return TC_1.ok;
};
const rewriteEffs = (ctx, eff, type) => {
    console.log(`rewriteEffs ${types_1.showType(eff)} in ${types_1.showType(type)}`);
    const ex = types_1.matchTEffsExtend(type);
    if (ex) {
        const rest = ex.rest;
        const u = exports.unify(ctx, eff, ex.eff);
        if (either_1.isRight(u))
            return either_1.Right(exports.prune(rest));
        if (rest.tag === 'TMeta') {
            const r = types_1.freshMeta('e', kinds_1.kEffs);
            const ret = exports.bind(rest, types_1.TEffsExtend(exports.prune(eff), r));
            if (either_1.isLeft(ret))
                return ret;
            return either_1.Right(types_1.TEffsExtend(exports.prune(ex.eff), r));
        }
        const rec = rewriteEffs(ctx, eff, ex.rest);
        if (either_1.isLeft(rec))
            return rec;
        return either_1.Right(types_1.TEffsExtend(exports.prune(ex.eff), rec.val));
    }
    return either_1.Left(`cannot rewrite effs: ${types_1.showType(eff)} in ${types_1.showType(type)}`);
};
exports.unify = (ctx, a_, b_, pr = true) => {
    // eq check & prune
    let a = a_;
    let b = b_;
    if (a === b)
        return TC_1.ok;
    if (pr) {
        a = exports.prune(a_);
        if (a === b_)
            return TC_1.ok;
    }
    if (pr) {
        b = exports.prune(b_);
        if (a === b)
            return TC_1.ok;
    }
    console.log(`${types_1.showType(a)} ~ ${types_1.showType(b)}`);
    // kind check
    const ka = exports.kindOf(ctx, a);
    if (either_1.isLeft(ka))
        return ka;
    const kb = exports.kindOf(ctx, b);
    if (either_1.isLeft(kb))
        return kb;
    if (!kinds_1.eqKind(ka.val, kb.val))
        return either_1.Left(`kind mismatch: (${types_1.showType(a)} :k ${kinds_1.showKind(ka.val)}) ~ (${types_1.showType(b)} :k ${kinds_1.showKind(kb.val)})`);
    // structural
    if (a.tag === 'TMeta')
        return exports.bind(a, b);
    if (b.tag === 'TMeta')
        return exports.bind(b, a);
    if (a.tag === 'TVar' && b.tag === 'TVar' && a.name === b.name)
        return TC_1.ok;
    const ea = types_1.matchTEffsExtend(a);
    const eb = types_1.matchTEffsExtend(b);
    if (ea && eb) {
        const reb = rewriteEffs(ctx, ea.eff, b);
        if (either_1.isLeft(reb))
            return reb;
        return exports.unify(ctx, ea.rest, reb.val, false);
    }
    if (a.tag === 'TApp' && b.tag === 'TApp') {
        const left = exports.unify(ctx, a.left, b.left, false);
        if (either_1.isLeft(left))
            return left;
        return exports.unify(ctx, a.right, b.right);
    }
    return either_1.Left(`cannot unify ${types_1.showType(a)} ~ ${types_1.showType(b)}`);
};

},{"./TC":1,"./context":2,"./either":3,"./kinds":7,"./types":11}],13:[function(require,module,exports){
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
exports.all = (arr, fn) => {
    for (let i = 0; i < arr.length; i++) {
        const c = arr[i];
        if (!fn(c))
            return false;
    }
    return true;
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
exports.objMap = (map, fn) => {
    const r = {};
    for (let k in map)
        r[k] = fn(map[k], k);
    return r;
};
exports.objMapToArr = (map, fn) => {
    const r = [];
    for (let k in map)
        r.push(fn(map[k], k));
    return r;
};
exports.objClone = (map) => {
    const n = {};
    for (let k in map)
        n[k] = map[k];
    return n;
};

},{}],14:[function(require,module,exports){
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

},{"./repl":10}]},{},[14]);

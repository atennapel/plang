(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
exports.compileToJS = (expr) => {
    if (exprs_1.isLit(expr))
        return JSON.stringify(expr.val);
    if (exprs_1.isVar(expr))
        return expr.name;
    if (exprs_1.isAbs(expr))
        return `(${expr.name} => ${exports.compileToJS(expr.body)})`;
    if (exprs_1.isApp(expr))
        return `${exports.compileToJS(expr.left)}(${exports.compileToJS(expr.right)})`;
    if (exprs_1.isLet(expr))
        return exports.compileToJS(exprs_1.App(exprs_1.Abs(expr.name, expr.body), expr.val));
    if (exprs_1.isAnno(expr))
        return exports.compileToJS(expr.expr);
    if (exprs_1.isWithLabel(expr)) {
        switch (expr.type) {
            case 'Select': return `_select('${expr.label}')`;
            case 'Extend': return `_extend('${expr.label}')`;
            case 'Inject': return `_inject('${expr.label}')`;
            case 'Case': return `_case('${expr.label}')`;
        }
    }
    throw new Error('unexpected expr in compileToJS');
};

},{"./exprs":3}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const utils_1 = require("./utils");
const types_1 = require("./types");
exports.findVar = (env, name) => env[name] || utils_1.err(`undefined var ${name}`);
exports.withExtend = (env, name, type, fn) => {
    const prev = env[name] || null;
    env[name] = type;
    const res = fn(env);
    if (prev)
        env[name] = prev;
    else
        delete env[name];
    return res;
};
exports.showEnv = (env) => {
    const r = [];
    for (let k in env) {
        r.push(`${k} : ${types_1.showType(env[k])}`);
    }
    return `{${r.join(', ')}}`;
};

},{"./types":10,"./utils":12}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const utils_1 = require("./utils");
const types_1 = require("./types");
exports.Lit = (val) => ({ tag: 'Lit', val });
exports.isLit = (expr) => expr.tag === 'Lit';
exports.Var = (name) => ({ tag: 'Var', name });
exports.isVar = (expr) => expr.tag === 'Var';
exports.App = (left, right) => ({ tag: 'App', left, right });
exports.isApp = (expr) => expr.tag === 'App';
exports.appFrom = (es) => es.reduce(exports.App);
exports.app = (...es) => es.reduce(exports.App);
exports.Abs = (name, body) => ({ tag: 'Abs', name, body });
exports.isAbs = (expr) => expr.tag === 'Abs';
exports.abs = (ns, body) => ns.reduceRight((a, b) => exports.Abs(b, a), body);
exports.Let = (name, val, body) => ({ tag: 'Let', name, val, body });
exports.isLet = (expr) => expr.tag === 'Let';
exports.Anno = (expr, type) => ({ tag: 'Anno', expr, type });
exports.isAnno = (expr) => expr.tag === 'Anno';
exports.WithLabel = (type, label) => ({ tag: 'WithLabel', type, label });
exports.isWithLabel = (expr) => expr.tag === 'WithLabel';
exports.isLabeled = (type, expr) => expr.tag === 'WithLabel' && expr.type === type;
exports.Select = (label) => exports.WithLabel('Select', label);
exports.Extend = (label) => exports.WithLabel('Extend', label);
exports.Inject = (label) => exports.WithLabel('Inject', label);
exports.Case = (label) => exports.WithLabel('Case', label);
const withLabelPrefixes = {
    Select: '.',
    Extend: '.+',
    Inject: '@',
    Case: '?',
};
exports.showExpr = (expr) => {
    if (exports.isLit(expr))
        return JSON.stringify(expr.val);
    if (exports.isVar(expr))
        return expr.name;
    if (exports.isApp(expr))
        return `(${exports.showExpr(expr.left)} ${exports.showExpr(expr.right)})`;
    if (exports.isAbs(expr))
        return `(\\${expr.name} -> ${exports.showExpr(expr.body)})`;
    if (exports.isLet(expr))
        return `(let ${expr.name} = ${exports.showExpr(expr.val)} in ${exports.showExpr(expr.body)})`;
    if (exports.isAnno(expr))
        return `(${exports.showExpr(expr.expr)} : ${types_1.showType(expr.type)})`;
    if (exports.isWithLabel(expr))
        return `${withLabelPrefixes[expr.type]}${expr.label}`;
    return utils_1.err('unexpected expr in showExpr');
};

},{"./types":10,"./utils":12}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const types_1 = require("./types");
const names_1 = require("./names");
const env_1 = require("./env");
const utils_1 = require("./utils");
const unification_1 = require("./unification");
const kindUnification_1 = require("./kindUnification");
const kinds_1 = require("./kinds");
const tmetas = (type, occ = {}) => {
    if (types_1.isTMeta(type)) {
        occ[type.name] = true;
        return occ;
    }
    if (types_1.isTApp(type))
        return tmetas(type.right, tmetas(type.left, occ));
    if (types_1.isTRowExtend(type))
        return tmetas(type.rest, tmetas(type.type, occ));
    return occ;
};
const tmetasEnv = (env, occ = {}) => {
    for (let k in env)
        tmetas(env[k], occ);
    return occ;
};
const inst = (type, subst = {}) => {
    if (types_1.isTVar(type))
        return subst[type.name] || (subst[type.name] = types_1.freshTMeta());
    if (types_1.isTApp(type))
        return types_1.TApp(inst(type.left, subst), inst(type.right, subst));
    if (types_1.isTRowExtend(type))
        return types_1.TRowExtend(type.label, inst(type.type, subst), inst(type.rest, subst));
    return type;
};
const genRec = (type, occ) => {
    if (types_1.isTMeta(type) && (!occ || !occ[type.name]))
        return types_1.TVar(type.name);
    if (types_1.isTApp(type))
        return types_1.TApp(genRec(type.left, occ), genRec(type.right, occ));
    if (types_1.isTRowExtend(type))
        return types_1.TRowExtend(type.label, genRec(type.type, occ), genRec(type.rest, occ));
    return type;
};
const gen = (type, env) => genRec(type, env ? tmetasEnv(env) : null);
const infer = (env, expr) => {
    // console.log('infer', showExpr(expr), showEnv(env));
    if (exprs_1.isLit(expr))
        return typeof expr.val === 'string' ? types_1.TStr : types_1.TFloat;
    if (exprs_1.isVar(expr))
        return inst(env_1.findVar(env, expr.name));
    if (exprs_1.isAbs(expr)) {
        const tv = types_1.freshTMeta();
        const tr = env_1.withExtend(env, expr.name, tv, env => infer(env, expr.body));
        return types_1.tfun(tv, tr);
    }
    if (exprs_1.isApp(expr)) {
        const tf = infer(env, expr.left);
        const ta = infer(env, expr.right);
        const tv = types_1.freshTMeta();
        unification_1.unify(tf, types_1.tfun(ta, tv));
        return tv;
    }
    if (exprs_1.isLet(expr)) {
        const tv = unification_1.prune(infer(env, expr.val));
        return env_1.withExtend(env, expr.name, tv, env => infer(env, expr.body));
    }
    if (exprs_1.isAnno(expr)) {
        const ty = infer(env, expr.expr);
        unification_1.unify(ty, expr.type, tmetasEnv(env));
        return expr.type;
    }
    if (exprs_1.isWithLabel(expr)) {
        switch (expr.type) {
            case 'Select': {
                const tt = types_1.freshTMeta();
                const tr = types_1.freshTMeta(kinds_1.KRow);
                return types_1.tfuns(types_1.tapp(types_1.TRecord, types_1.TRowExtend(expr.label, tt, tr)), tt);
            }
            case 'Extend': {
                const tt = types_1.freshTMeta();
                const tr = types_1.freshTMeta(kinds_1.KRow);
                return types_1.tfuns(tt, types_1.tapp(types_1.TRecord, tr), types_1.tapp(types_1.TRecord, types_1.TRowExtend(expr.label, tt, tr)));
            }
            case 'Inject': {
                const tt = types_1.freshTMeta();
                const tr = types_1.freshTMeta(kinds_1.KRow);
                return types_1.tfuns(tt, types_1.tapp(types_1.TVariant, types_1.TRowExtend(expr.label, tt, tr)));
            }
            case 'Case': {
                const ta = types_1.freshTMeta();
                const tb = types_1.freshTMeta();
                const tr = types_1.freshTMeta(kinds_1.KRow);
                return types_1.tfuns(types_1.tfuns(ta, tb), types_1.tfuns(types_1.tapp(types_1.TVariant, tr), tb), types_1.tapp(types_1.TVariant, types_1.TRowExtend(expr.label, ta, tr)), tb);
            }
        }
    }
    return utils_1.err('unexpected expr in infer');
};
exports.inferTop = (env, expr) => {
    names_1.resetTName();
    const ty = infer(env, expr);
    kindUnification_1.unifyKind(kinds_1.KType, unification_1.inferKind(ty));
    return gen(unification_1.prune(ty));
};

},{"./env":2,"./exprs":3,"./kindUnification":5,"./kinds":6,"./names":7,"./types":10,"./unification":11,"./utils":12}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const utils_1 = require("./utils");
const kinds_1 = require("./kinds");
exports.pruneKind = (kind) => {
    if (kinds_1.isKMeta(kind)) {
        if (!kind.kind)
            return kind;
        const ki = exports.pruneKind(kind.kind);
        kind.kind = ki;
        return ki;
    }
    if (kinds_1.isKFun(kind))
        return kinds_1.KFun(exports.pruneKind(kind.left), exports.pruneKind(kind.right));
    return kind;
};
const occurs = (a, b) => {
    if (a === b)
        return utils_1.err('occurs check failed');
    if (kinds_1.isKFun(b))
        return occurs(a, b.left), occurs(a, b.right);
};
const bind = (a, b) => {
    occurs(a, b);
    a.kind = b;
};
exports.unifyKind = (a_, b_) => {
    if (a_ === b_)
        return;
    const a = exports.pruneKind(a_);
    const b = exports.pruneKind(b_);
    if (a === b)
        return;
    if (kinds_1.isKMeta(a))
        return bind(a, b);
    if (kinds_1.isKMeta(b))
        return bind(b, a);
    if (kinds_1.isKFun(a) && kinds_1.isKFun(b))
        return exports.unifyKind(a.left, b.left), exports.unifyKind(a.right, b.right);
    return utils_1.err(`cannot unify ${kinds_1.showKind(a)} ~ ${kinds_1.showKind(b)}`);
};

},{"./kinds":6,"./utils":12}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const utils_1 = require("./utils");
exports.KConst = (name) => ({ tag: 'KConst', name });
exports.isKConst = (kind) => kind.tag === 'KConst';
exports.KMeta = (name, kind) => ({ tag: 'KMeta', name, kind });
exports.isKMeta = (kind) => kind.tag === 'KMeta';
exports.KFun = (left, right) => ({ tag: 'KFun', left, right });
exports.isKFun = (kind) => kind.tag === 'KFun';
exports.kfun = (...ks) => ks.reduceRight((a, b) => exports.KFun(b, a));
exports.showKind = (type) => {
    if (exports.isKConst(type))
        return type.name;
    if (exports.isKMeta(type))
        return `?${type.name}`;
    if (exports.isKFun(type))
        return `(${exports.showKind(type.left)} -> ${exports.showKind(type.right)})`;
    return utils_1.err('unexpected kind in showKind');
};
exports.freshKMeta = () => exports.KMeta(names_1.freshTName(), null);
exports.KType = exports.KConst('Type');
exports.KRow = exports.KConst('Row');

},{"./names":7,"./utils":12}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports._id = 0;
exports.resetTName = () => { exports._id = 0; };
exports.freshTName = () => exports._id++;

},{}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const err = (msg) => { throw new SyntaxError(msg); };
const matchingBracket = (c) => {
    if (c === '(')
        return ')';
    if (c === ')')
        return '(';
    return err(`invalid bracket: ${c}`);
};
const START = 0;
const NUM = 1;
const NAME = 2;
const STR = 3;
const tokenize = (s) => {
    let state = START;
    let t = '';
    let r = [], p = [], b = [], esc = false;
    for (let i = 0; i <= s.length; i++) {
        const c = s[i] || ' ';
        // console.log(i, c, state, t, esc);
        if (state === START) {
            if (/[a-z\.\+\?\@]/i.test(c))
                t += c, state = NAME;
            else if (/[0-9]/.test(c))
                t += c, state = NUM;
            else if (c === '"')
                state = STR;
            else if (c === '(')
                b.push(c), p.push(r), r = [];
            else if (c === ')') {
                if (b.length === 0)
                    return err(`unmatched bracket: ${c}`);
                const br = b.pop();
                if (matchingBracket(br) !== c)
                    return err(`unmatched bracket: ${br} and ${c}`);
                const a = p.pop();
                a.push({ tag: 'TkParens', val: r });
                r = a;
            }
            else if (/\s+/.test(c))
                continue;
            else
                return err(`invalid char: ${c}`);
        }
        else if (state === NUM) {
            if (!/[0-9\.]/.test(c))
                r.push({ tag: 'TkNum', val: t }), t = '', i--, state = START;
            else
                t += c;
        }
        else if (state === NAME) {
            if (!/[a-z0-9]/i.test(c))
                r.push({ tag: 'TkName', val: t }), t = '', i--, state = START;
            else
                t += c;
        }
        else if (state === STR) {
            if (esc) {
                esc = false;
                t += c;
            }
            else if (c === '\\')
                esc = true;
            else if (c === '"')
                r.push({ tag: 'TkStr', val: t }), t = '', state = START;
            else
                t += c;
        }
    }
    if (b.length > 0)
        return err(`unclosed brackets: ${b.join(' ')}`);
    if (state === STR)
        return err('unclosed string');
    if (state !== START)
        return err(`invalid parsing end state: ${state}`);
    return r;
};
const parseToken = (a) => {
    switch (a.tag) {
        case 'TkNum': {
            const n = +a.val;
            if (isNaN(n))
                return err(`invalid number: ${a.val}`);
            return exprs_1.Lit(n);
        }
        case 'TkName': {
            if (a.val[0] === '.')
                return a.val.length === 1 ? err('nothing after .') : exprs_1.Select(a.val.slice(1));
            if (a.val[0] === '+')
                return a.val.length === 1 ? err('nothing after +') : exprs_1.Extend(a.val.slice(1));
            if (a.val[0] === '@')
                return a.val.length === 1 ? err('nothing after @') : exprs_1.Inject(a.val.slice(1));
            if (a.val[0] === '?')
                return a.val.length === 1 ? err('nothing after ?') : exprs_1.Case(a.val.slice(1));
            return exprs_1.Var(a.val);
        }
        case 'TkStr': return exprs_1.Lit(a.val);
        case 'TkParens':
            return parseParens(a.val);
    }
};
const parseParens = (a) => {
    if (a.length === 0)
        return exprs_1.Var('empty');
    if (a.length === 1)
        return parseToken(a[0]);
    if (a.length >= 2 && a[0].tag === 'TkName' && a[0].val === 'fn') {
        const sa = a[1].val;
        const args = Array.isArray(sa) ? sa.map(t => t.tag === 'TkName' ? t.val : err('invalid arg in fn')) : [sa];
        return exprs_1.abs(args, parseParens(a.slice(2)));
    }
    return exprs_1.appFrom(a.map(parseToken));
};
exports.parse = (s) => parseParens(tokenize(s));

},{"./exprs":3}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const parser_1 = require("./parser");
const inference_1 = require("./inference");
const compiler_1 = require("./compiler");
const tv = types_1.TVar;
const ta = tv(0);
const tb = tv(1);
const tr = tv(1, kinds_1.KRow);
const _env = {
    fix: types_1.tfuns(types_1.tfuns(ta, ta), ta),
    empty: types_1.tapp(types_1.TRecord, types_1.TRowEmpty),
    end: types_1.tfuns(types_1.tapp(types_1.TVariant, types_1.TRowEmpty), ta),
    show: types_1.tfuns(ta, types_1.TStr),
};
function _show(x) {
    if (typeof x === 'string')
        return JSON.stringify(x);
    if (typeof x === 'function')
        return '[Function]';
    if (typeof x._tag === 'string')
        return typeof x.val === 'undefined' ? x._tag :
            Array.isArray(x.val) ? `(${x._tag} ${x.val.map(_show).join(' ')})` :
                `(${x._tag} ${_show(x.val)})`;
    if (typeof x === 'object' && x._rec) {
        const r = [];
        for (let k in x)
            if (k[0] !== '_')
                r.push(`${k}: ${_show(x[k])}`);
        return `{${r.join(', ')}}`;
    }
    return '' + x;
}
function _run(i, cb) {
    try {
        console.log(i);
        const p = parser_1.parse(i);
        console.log(exprs_1.showExpr(p));
        const result = inference_1.inferTop(_env, p);
        console.log(`${types_1.showType(result)}`);
        const c = compiler_1.compileToJS(p);
        console.log(c);
        const res = eval(c);
        cb(`${_show(res)} : ${types_1.prettyType(result)}`);
    }
    catch (e) {
        console.log(e);
        cb('' + e, true);
    }
}
exports.default = _run;

},{"./compiler":1,"./exprs":3,"./inference":4,"./kinds":6,"./parser":8,"./types":10}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const utils_1 = require("./utils");
const kinds_1 = require("./kinds");
exports.TConst = (name, kind = kinds_1.KType) => ({ tag: 'TConst', name, kind });
exports.isTConst = (type) => type.tag === 'TConst';
exports.TMeta = (name, kind, type) => ({ tag: 'TMeta', name, kind, type });
exports.isTMeta = (type) => type.tag === 'TMeta';
exports.TVar = (name, kind = kinds_1.KType) => ({ tag: 'TVar', name, kind });
exports.isTVar = (type) => type.tag === 'TVar';
exports.TApp = (left, right) => ({ tag: 'TApp', left, right });
exports.isTApp = (type) => type.tag === 'TApp';
exports.tapp = (...es) => es.reduce(exports.TApp);
exports.flattenTApp = (type) => {
    let c = type;
    const r = [];
    while (exports.isTApp(c)) {
        r.push(c.right);
        c = c.left;
    }
    r.push(c);
    return r.reverse();
};
exports.TRowExtend = (label, type, rest) => ({ tag: 'TRowExtend', label, type, rest });
exports.isTRowExtend = (type) => type.tag === 'TRowExtend';
exports.flattenTRowExtend = (type) => {
    let c = type;
    const ts = [];
    while (exports.isTRowExtend(c)) {
        ts.push([c.label, c.type]);
        c = c.rest;
    }
    return { ts, rest: c };
};
exports.showType = (type) => {
    if (exports.isTConst(type))
        return type.name;
    if (exports.isTMeta(type))
        return `?${type.name}`;
    if (exports.isTVar(type))
        return `'${type.name}`;
    if (exports.isTApp(type))
        return `(${exports.showType(type.left)} ${exports.showType(type.right)})`;
    if (exports.isTRowExtend(type))
        return `(${type.label} : ${exports.showType(type.type)} | ${exports.showType(type.rest)})`;
    return utils_1.err('unexpected type in showType');
};
exports.freshTMeta = (kind = kinds_1.KType) => exports.TMeta(names_1.freshTName(), kind, null);
exports.TFun = exports.TConst('->', kinds_1.kfun(kinds_1.KType, kinds_1.KType, kinds_1.KType));
exports.tfun = (a, b) => exports.TApp(exports.TApp(exports.TFun, a), b);
exports.tfuns = (...ts) => ts.reduceRight((a, b) => exports.tfun(b, a));
exports.isTFun = (type) => exports.isTApp(type) && exports.isTApp(type.left) && type.left.left === exports.TFun ?
    ({ left: type.left.right, right: type.right }) : null;
exports.flattenTFun = (type) => {
    let c = type;
    const r = [];
    let t = exports.isTFun(c);
    while (t) {
        r.push(t.left);
        c = t.right;
        t = exports.isTFun(c);
    }
    r.push(c);
    return r;
};
exports.TFloat = exports.TConst('Float');
exports.TStr = exports.TConst('Str');
exports.TRowEmpty = exports.TConst('()', kinds_1.KRow);
exports.TRecord = exports.TConst('Rec', kinds_1.kfun(kinds_1.KRow, kinds_1.KType));
exports.TVariant = exports.TConst('Var', kinds_1.kfun(kinds_1.KRow, kinds_1.KType));
exports.trow = (ts, rest = exports.TRowEmpty) => ts.reduceRight((r, [l, t]) => exports.TRowExtend(l, t, r), rest);
const tvAlphabet = 'abcdefghijklmnopqrstuvwxyz';
exports.prettyTypeR = (type, map, index) => {
    if (exports.isTConst(type))
        return type.name;
    if (exports.isTMeta(type))
        return `?${type.name}`;
    if (exports.isTVar(type)) {
        if (!map[type.name]) {
            const l = tvAlphabet.length;
            const ix = index.val++;
            const rank = 0 | ix / l;
            map[type.name] = `${tvAlphabet[ix % l]}${rank > 0 ? rank : ''}`;
        }
        return map[type.name];
    }
    if (exports.isTFun(type))
        return exports.flattenTFun(type)
            .map(t => exports.isTFun(t) ? `(${exports.prettyTypeR(t, map, index)})` : exports.prettyTypeR(t, map, index))
            .join(' -> ');
    if (exports.isTApp(type))
        return exports.flattenTApp(type)
            .map(t => exports.isTApp(t) ? `(${exports.prettyTypeR(t, map, index)})` : exports.prettyTypeR(t, map, index))
            .join(' ');
    if (exports.isTRowExtend(type)) {
        const fl = exports.flattenTRowExtend(type);
        const head = fl.ts.map(([l, t]) => `${l} : ${exports.prettyTypeR(t, map, index)}`).join(', ');
        return fl.rest === exports.TRowEmpty ? `(${head})` : `(${head} | ${exports.prettyTypeR(fl.rest, map, index)})`;
    }
    return utils_1.err('unexpected type in prettyTypeR');
};
exports.prettyType = (type) => {
    return exports.prettyTypeR(type, {}, { val: 0 });
};

},{"./kinds":6,"./names":7,"./utils":12}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const utils_1 = require("./utils");
const kindUnification_1 = require("./kindUnification");
const kinds_1 = require("./kinds");
exports.prune = (type) => {
    if (types_1.isTMeta(type)) {
        if (!type.type)
            return type;
        const ty = exports.prune(type.type);
        type.type = ty;
        return ty;
    }
    if (types_1.isTApp(type))
        return types_1.TApp(exports.prune(type.left), exports.prune(type.right));
    if (types_1.isTRowExtend(type))
        return types_1.TRowExtend(type.label, exports.prune(type.type), exports.prune(type.rest));
    return type;
};
const rewriteRow = (l, ty) => {
    // console.log(`rewriteRow ${l} in ${showType(ty)}`);
    const row = [];
    let c = ty;
    while (types_1.isTRowExtend(c)) {
        if (c.label === l)
            return types_1.TRowExtend(l, c.type, types_1.trow(row, c.rest));
        row.push([c.label, c.type]);
        c = c.rest;
    }
    if (c === types_1.TRowEmpty)
        return utils_1.err(`cannot find label ${l} in ${types_1.showType(ty)}`);
    if (types_1.isTMeta(c)) {
        const t = types_1.freshTMeta(kinds_1.KType);
        const r = types_1.freshTMeta(kinds_1.KRow);
        bind(c, types_1.TRowExtend(l, t, r));
        return types_1.TRowExtend(l, t, types_1.trow(row, r));
    }
    return utils_1.err(`unexpected type in rewriteRow: ${types_1.showType(c)}`);
};
const occurs = (a, b) => {
    if (a === b)
        return utils_1.err('occurs check failed');
    if (types_1.isTApp(b))
        return occurs(a, b.left), occurs(a, b.right);
    if (types_1.isTRowExtend(b))
        return occurs(a, b.type), occurs(a, b.rest);
};
const bind = (a, b, occ) => {
    if (occ && types_1.isTVar(b) && occ[a.name])
        return utils_1.err(`cannot bind ${types_1.showType(a)} := ${types_1.showType(b)} in annotation`);
    occurs(a, b);
    a.type = b;
};
exports.inferKind = (type) => {
    if (types_1.isTApp(type)) {
        const kf = exports.inferKind(type.left);
        const ka = exports.inferKind(type.right);
        const kr = kinds_1.freshKMeta();
        kindUnification_1.unifyKind(kf, kinds_1.KFun(ka, kr));
        return kr;
    }
    if (types_1.isTRowExtend(type)) {
        const kt = exports.inferKind(type.type);
        kindUnification_1.unifyKind(kt, kinds_1.KType);
        const kr = exports.inferKind(type.rest);
        kindUnification_1.unifyKind(kr, kinds_1.KRow);
        return kinds_1.KRow;
    }
    return type.kind;
};
exports.unify = (a_, b_, occ) => {
    if (a_ === b_)
        return;
    const a = exports.prune(a_);
    const b = exports.prune(b_);
    if (a === b)
        return;
    // console.log(`unify ${showType(a)} ~ ${showType(b)}`);
    kindUnification_1.unifyKind(exports.inferKind(a), exports.inferKind(b));
    if (types_1.isTMeta(a))
        return bind(a, b, occ);
    if (types_1.isTMeta(b))
        return bind(b, a, occ);
    if (types_1.isTApp(a) && types_1.isTApp(b))
        return exports.unify(a.left, b.left, occ), exports.unify(a.right, b.right, occ);
    if (types_1.isTRowExtend(a) && types_1.isTRowExtend(b)) {
        const rewr = rewriteRow(a.label, b);
        exports.unify(a.type, rewr.type, occ);
        exports.unify(a.rest, rewr.rest, occ);
        return;
    }
    return utils_1.err(`cannot unify ${types_1.showType(a)} ~ ${types_1.showType(b)}`);
};

},{"./kindUnification":5,"./kinds":6,"./types":10,"./utils":12}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.err = (msg) => { throw new TypeError(msg); };

},{}],13:[function(require,module,exports){
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

},{"./repl":9}]},{},[13]);

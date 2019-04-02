(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// immutable singly-linked list
class List {
    static from(a) {
        let c = Nil.new();
        for (let i = a.length - 1; i >= 0; i--)
            c = new Cons(a[i], c);
        return c;
    }
    static of(...a) { return List.from(a); }
    static nil() {
        return Nil.new();
    }
    static cons(head, tail) {
        return new Cons(head, tail);
    }
    isEmpty() {
        return this instanceof Nil;
    }
    isNonEmpty() {
        return this instanceof Cons;
    }
    take(amount = -1) {
        let c = this;
        const r = [];
        while (c instanceof Cons) {
            if (amount >= 0 && r.length >= amount)
                break;
            r.push(c._head);
            c = c._tail;
        }
        return r;
    }
    toString(fn = x => `${x}`) {
        const r = [];
        let l = this;
        while (l instanceof Cons) {
            r.push(fn(l._head));
            l = l._tail;
        }
        return `[${r.join(', ')}]`;
    }
    each(fn) {
        let l = this;
        while (l instanceof Cons) {
            fn(l._head);
            l = l._tail;
        }
    }
    first(fn) {
        let l = this;
        while (l instanceof Cons) {
            const h = l._head;
            if (fn(h))
                return h;
            l = l._tail;
        }
        return null;
    }
    foldl(fcons, fnil) {
        let l = this;
        let c = fnil;
        while (l instanceof Cons) {
            c = fcons(c, l._head);
            l = l._tail;
        }
        return c;
    }
}
exports.default = List;
class Nil extends List {
    constructor() { super(); }
    static new() { return Nil._nil; }
    case(fnil, fcons) {
        return fnil();
    }
    foldr(fcons, fnil) {
        return fnil;
    }
    map(fn) {
        return this;
    }
    filter(fn) {
        return this;
    }
    mapMaybe(fn) {
        return this;
    }
    append(other) {
        return other;
    }
    flatMap(fn) {
        return this;
    }
    zip(other) {
        return this;
    }
}
Nil._nil = new Nil();
exports.Nil = Nil;
class Cons extends List {
    constructor(_head, _tail) {
        super();
        this._head = _head;
        this._tail = _tail;
    }
    static new(head, tail) {
        return new Cons(head, tail);
    }
    head() { return this._head; }
    tail() { return this._tail; }
    case(fnil, fcons) {
        return fcons(this._head, this._tail);
    }
    foldr(fcons, fnil) {
        return fcons(this._head, this._tail.foldr(fcons, fnil));
    }
    map(fn) {
        return new Cons(fn(this._head), this._tail.map(fn));
    }
    filter(fn) {
        return fn(this._head) ? new Cons(this._head, this._tail.filter(fn)) : this._tail.filter(fn);
    }
    mapMaybe(fn) {
        const x = fn(this._head);
        return x === null ? this._tail.mapMaybe(fn) : new Cons(x, this._tail.mapMaybe(fn));
    }
    append(other) {
        return new Cons(this._head, this._tail.append(other));
    }
    flatMap(fn) {
        return fn(this._head).append(this._tail.flatMap(fn));
    }
    zip(other) {
        return other.case(() => other, (h, t) => new Cons([this._head, h], this._tail.zip(t)));
    }
}
exports.Cons = Cons;

},{}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
exports.compileName = (x) => {
    return keywords.indexOf(x) >= 0 ? `${x}_` : x;
};
exports.compilePat = (pat) => {
    if (pat.tag === 'PVar')
        return pat.name;
    if (pat.tag === 'PAnn')
        return exports.compilePat(pat.pat);
    if (pat.tag === 'PWildcard')
        return '_';
    if (pat.tag === 'PCon')
        return exports.compilePat(pat.pat);
    return util_1.impossible('compilePat');
};
exports.compile = (term) => {
    if (term.tag === 'Var')
        return exports.compileName(term.name);
    if (term.tag === 'Abs')
        return `(${exports.compilePat(term.pat)} => ${exports.compile(term.body)})`;
    if (term.tag === 'App')
        return `${exports.compile(term.left)}(${exports.compile(term.right)})`;
    if (term.tag === 'Ann')
        return exports.compile(term.term);
    if (term.tag === 'Let')
        return `(${exports.compileName(term.name)} => ${exports.compile(term.body)})(${exports.compile(term.val)})`;
    return util_1.impossible('compile');
};
exports.compileDef = (def, prefix) => {
    switch (def.tag) {
        case 'DType': {
            const con = `${prefix(exports.compileName(def.name))} = x => x;`;
            return `${con}`;
        }
        case 'DLet':
            return `${prefix(exports.compileName(def.name))} = ${def.args.map(exports.compilePat).join(' => ')}${def.args.length > 0 ? ' => ' : ''}${exports.compile(def.term)};`;
    }
};
exports.compileDefs = (ds, prefix) => ds.map(d => exports.compileDef(d, prefix)).join('\n') + '\n';
const keywords = `
do
if
in
for
let
new
try
var
case
else
enum
eval
null
this
true
void
with
await
break
catch
class
const
false
super
throw
while
yield
delete
export
import
public
return
static
switch
typeof
default
extends
finally
package
private
continue
debugger
function
arguments
interface
protected
implements
instanceof
`.trim().split(/\s+/);

},{"./util":15}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.config = {
    debug: false,
    showKinds: false,
};
exports.setConfig = (c) => {
    for (let k in c)
        exports.config[k] = c[k];
};
exports.log = (msg) => {
    if (exports.config.debug)
        console.log(msg);
};

},{}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const terms_1 = require("./terms");
exports.DType = (name, args, type) => ({ tag: 'DType', name, args, type });
exports.isDType = (def) => def.tag === 'DType';
exports.DLet = (name, args, term) => ({ tag: 'DLet', name, args, term });
exports.isDLet = (def) => def.tag === 'DLet';
exports.showDef = (def) => {
    switch (def.tag) {
        case 'DType': {
            const args = def.args.length > 0 ?
                `${def.args.map(([n, k]) => k ? `(${n} : ${kinds_1.showKind(k)})` : n).join(' ')} ` :
                '';
            return `type ${def.name} ${args}= ${types_1.showTy(def.type)}`;
        }
        case 'DLet': {
            const args = def.args.length > 0 ? `${def.args.map(terms_1.showPat).join(' ')} ` : '';
            return `let ${def.name} ${args}= ${terms_1.showTerm(def.term)}`;
        }
    }
};

},{"./kinds":8,"./terms":12,"./types":13}],5:[function(require,module,exports){
(function (global){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const List_1 = require("./List");
const util_1 = require("./util");
exports.Env = (global = {}, tcons = {}, local = List_1.default.nil()) => ({ global, tcons, local });
exports.showEnv = (env) => {
    const r = [];
    for (let k in env.tcons)
        r.push(`type ${k} : ${kinds_1.showKind(env.tcons[k])}`);
    for (let k in env.global)
        r.push(`${k} : ${types_1.showTy(env.global[k])}`);
    return r.join('\n');
};
exports.extendVar = (env, x, t) => exports.Env(env.global, env.tcons, List_1.default.cons([x, t], env.local));
exports.extendVars = (env, vs) => {
    const local = vs.reduce((l, kv) => List_1.default.cons(kv, l), env.local);
    return exports.Env(env.global, env.tcons, local);
};
exports.lookupVar = (env, x) => {
    const t = env.local.first(([k, _]) => x === k);
    if (t)
        return t[1];
    return env.global[x] || null;
};
exports.lookupTCon = (env, x) => env.tcons[x] || null;
exports.skolemCheckEnv = (sk, env) => {
    env.local.each(([_, t]) => util_1.skolemCheck(sk, types_1.prune(t)));
    const vars = env.global;
    for (let k in vars)
        util_1.skolemCheck(sk, types_1.prune(vars[k]));
};
exports.tmetasEnv = (env, free = [], tms = []) => {
    env.local.each(([_, t]) => types_1.tmetas(types_1.prune(t), free, tms));
    const vars = env.global;
    for (let k in vars)
        types_1.tmetas(types_1.prune(vars[k]), free, tms);
    return tms;
};
exports.initialEnv = exports.Env({}, {
    '->': kinds_1.KFun(kinds_1.kType, kinds_1.KFun(kinds_1.kType, kinds_1.kType)),
});

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./List":1,"./kinds":8,"./types":13,"./util":15}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const util_1 = require("./util");
const env_1 = require("./env");
const terms_1 = require("./terms");
const unification_1 = require("./unification");
const kindInference_1 = require("./kindInference");
const kinds_1 = require("./kinds");
const config_1 = require("./config");
const definitions_1 = require("./definitions");
const Check = (type) => ({ tag: 'Check', type });
const Infer = () => ({ tag: 'Infer', type: null });
const showEx = (ex) => {
    if (ex.tag === 'Check')
        return `Check(${types_1.showTy(ex.type)})`;
    if (ex.tag === 'Infer')
        return `Infer(${ex.type ? types_1.showTy(ex.type) : '...'})`;
    return util_1.impossible('showEx');
};
const instantiate = (ty) => {
    if (ty.tag !== 'TForall')
        return ty;
    const m = {};
    const names = ty.names;
    for (let i = 0, l = names.length; i < l; i++) {
        const x = names[i];
        m[x] = util_1.freshTMeta(ty.kinds[i], x);
    }
    return types_1.substTVar(m, ty.type);
};
const skolemise = (ty, sk = []) => {
    if (ty.tag === 'TForall') {
        const m = {};
        const names = ty.names;
        for (let i = 0, l = names.length; i < l; i++) {
            const k = util_1.freshTSkol(names[i], ty.kinds[i]);
            m[names[i]] = k;
            sk.push(k);
        }
        return skolemise(types_1.substTVar(m, ty.type), sk);
    }
    if (types_1.isTFun(ty)) {
        const { left: { right: left }, right } = ty;
        const b = skolemise(right, sk);
        return types_1.TFun(left, b);
    }
    return ty;
};
const checkRho = (env, term, ty) => tcRho(env, term, Check(ty));
const inferRho = (env, term) => {
    const i = Infer();
    tcRho(env, term, i);
    if (!i.type)
        return util_1.terr(`inferRho failed for ${terms_1.showTerm(term)}`);
    return i.type;
};
const tcRho = (env, term, ex) => {
    config_1.log(`tcRho ${terms_1.showTerm(term)} with ${showEx(ex)}`);
    if (term.tag === 'Var') {
        const ty = env_1.lookupVar(env, term.name);
        if (!ty)
            return util_1.terr(`undefined var ${terms_1.showTerm(term)}`);
        return instSigma(env, ty, ex);
    }
    if (term.tag === 'App') {
        const ty = inferRho(env, term.left);
        const { left: { right: left }, right } = unification_1.unifyTFun(env, ty);
        checkSigma(env, term.right, left);
        return instSigma(env, right, ex);
    }
    if (term.tag === 'Abs') {
        if (ex.tag === 'Check') {
            const { left: { right: left }, right } = unification_1.unifyTFun(env, ex.type);
            const bs = checkPat(env, term.pat, left);
            const nenv = env_1.extendVars(env, bs);
            return checkRho(nenv, term.body, right);
        }
        else if (ex.tag === 'Infer') {
            const [bs, ty] = inferPat(env, term.pat);
            const nenv = env_1.extendVars(env, bs);
            const bty = inferRho(nenv, term.body);
            ex.type = types_1.TFun(ty, bty);
            return;
        }
    }
    if (term.tag === 'Let') {
        const ty = inferSigma(env, term.val);
        const nenv = env_1.extendVar(env, term.name, ty);
        return tcRho(nenv, term.body, ex);
    }
    if (term.tag === 'Ann') {
        const type = kindInference_1.inferKind(env, term.type);
        checkSigma(env, term.term, type);
        return instSigma(env, type, ex);
    }
    return util_1.impossible('tcRho');
};
const checkPat = (env, pat, ty) => tcPat(env, pat, Check(ty));
const inferPat = (env, pat) => {
    const i = Infer();
    const bs = tcPat(env, pat, i);
    if (!i.type)
        return util_1.terr(`inferPat failed for ${terms_1.showPat(pat)}`);
    return [bs, i.type];
};
const tcPat = (env, pat, ex) => {
    if (pat.tag === 'PWildcard') {
        if (ex.tag === 'Infer')
            ex.type = util_1.freshTMeta(kinds_1.kType);
        return [];
    }
    if (pat.tag === 'PVar') {
        if (ex.tag === 'Check')
            return [[pat.name, ex.type]];
        const ty = util_1.freshTMeta(kinds_1.kType);
        ex.type = ty;
        return [[pat.name, ty]];
    }
    if (pat.tag === 'PAnn') {
        const ty = kindInference_1.inferKind(env, pat.type);
        const bs = checkPat(env, pat.pat, ty);
        instPatSigma(env, ty, ex);
        return bs;
    }
    if (pat.tag === 'PCon') {
        const ty = env_1.lookupVar(env, pat.name);
        if (!ty)
            return util_1.terr(`undefined constructor ${pat.name} in pattern`);
        const { left: { right: left }, right } = unification_1.unifyTFun(env, instantiate(ty));
        const bs = checkPat(env, pat.pat, left);
        instPatSigma(env, right, ex);
        return bs;
    }
    return util_1.impossible('tcPat');
};
const instPatSigma = (env, ty, ex) => {
    if (ex.tag === 'Check')
        return subsCheck(env, ex.type, ty);
    ex.type = ty;
};
const inferSigma = (env, term) => {
    const ty = inferRho(env, term);
    const etms = env_1.tmetasEnv(env);
    const tms = types_1.tmetas(types_1.prune(ty), etms);
    return types_1.quantify(tms, ty);
};
const checkSigma = (env, term, ty) => {
    const sk = [];
    const rho = skolemise(ty, sk);
    checkRho(env, term, rho);
    util_1.skolemCheck(sk, types_1.prune(ty));
    env_1.skolemCheckEnv(sk, env);
};
const subsCheck = (env, a, b) => {
    config_1.log(`subsCheck ${types_1.showTy(a)} <: ${types_1.showTy(b)}`);
    const sk = [];
    const rho = skolemise(b, sk);
    subsCheckRho(env, a, rho);
    util_1.skolemCheck(sk, types_1.prune(a));
    util_1.skolemCheck(sk, types_1.prune(b));
};
const subsCheckRho = (env, a, b) => {
    if (a.tag === 'TForall')
        return subsCheckRho(env, instantiate(a), b);
    if (types_1.isTFun(b))
        return subsCheckTFun(env, unification_1.unifyTFun(env, a), b);
    if (types_1.isTFun(a))
        return subsCheckTFun(env, a, unification_1.unifyTFun(env, b));
    return unification_1.unify(env, a, b);
};
const subsCheckTFun = (env, a, b) => {
    subsCheck(env, b.left.right, a.left.right);
    return subsCheck(env, a.right, b.right);
};
const instSigma = (env, ty, ex) => {
    if (ex.tag === 'Check')
        return subsCheckRho(env, ty, ex.type);
    ex.type = instantiate(ty);
};
exports.infer = (env, term) => {
    config_1.log(`infer ${terms_1.showTerm(term)}`);
    util_1.resetId();
    return types_1.prune(inferSigma(env, term));
};
exports.inferDef = (env, def) => {
    config_1.log(`inferDef ${definitions_1.showDef(def)}`);
    if (def.tag === 'DType') {
        const tname = def.name;
        if (env_1.lookupTCon(env, tname))
            return util_1.terr(`type ${tname} is already defined`);
        if (env_1.lookupVar(env, tname))
            return util_1.terr(`constructor ${tname} is already defined`);
        env.tcons[tname] = util_1.freshKMeta();
        const t = def.type;
        const tc = types_1.TCon(tname);
        const b = types_1.tfunFrom([t, types_1.tappFrom([tc].concat(def.args.map(([x, _]) => types_1.TVar(x))))]);
        const ty = types_1.tforall(def.args, b);
        const ti = kindInference_1.inferKind(env, ty);
        env.global[tname] = ti;
        env.tcons[tname] = kinds_1.pruneKind(env.tcons[tname]);
        return;
    }
    if (def.tag === 'DLet') {
        const name = def.name;
        if (env_1.lookupVar(env, name))
            return util_1.terr(`${name} is already defined`);
        const ty = exports.infer(env, terms_1.abs(def.args, def.term));
        env.global[name] = ty;
        return;
    }
    return util_1.impossible('inferDef');
};
exports.inferDefs = (env, ds) => {
    for (let i = 0, l = ds.length; i < l; i++)
        exports.inferDef(env, ds[i]);
};

},{"./config":3,"./definitions":4,"./env":5,"./kindInference":7,"./kinds":8,"./terms":12,"./types":13,"./unification":14,"./util":15}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const util_1 = require("./util");
const env_1 = require("./env");
const bindKMeta = (x, k) => {
    if (x.kind)
        return unifyKind(x.kind, k);
    if (k.tag === 'KMeta' && k.kind)
        return unifyKind(x, k.kind);
    if (kinds_1.occursKMeta(x, k))
        return util_1.terr(`${kinds_1.showKind(x)} occurs in ${kinds_1.showKind(k)}`);
    x.kind = k;
};
const unifyKind = (a, b) => {
    if (a === b)
        return;
    if (a.tag === 'KMeta')
        return bindKMeta(a, b);
    if (b.tag === 'KMeta')
        return bindKMeta(b, a);
    if (a.tag === 'KFun' && b.tag === 'KFun') {
        unifyKind(a.left, b.left);
        return unifyKind(a.right, b.right);
    }
    if (a.tag === 'KCon' && b.tag === 'KCon' && a.name === b.name)
        return;
    return util_1.terr(`failed to unify kinds: ${kinds_1.showKind(a)} ~ ${kinds_1.showKind(b)}`);
};
const inferKindR = (env, t) => {
    if (t.tag === 'TMeta')
        return [t.kind, t];
    if (t.tag === 'TVar')
        return util_1.terr(`tvar ${types_1.showTy(t)} in inferKindR`);
    if (t.tag === 'TSkol')
        return [t.kind, t];
    if (t.tag === 'TCon') {
        const k = env_1.lookupTCon(env, t.name);
        if (!k)
            return util_1.terr(`undefined type constructor ${types_1.showTy(t)}`);
        return [k, t];
    }
    if (t.tag === 'TApp') {
        const [l, tl] = inferKindR(env, t.left);
        const [r, tr] = inferKindR(env, t.right);
        const km = util_1.freshKMeta();
        unifyKind(l, kinds_1.KFun(r, km));
        return [km, types_1.TApp(tl, tr)];
    }
    if (t.tag === 'TForall') {
        const { names, type } = t;
        const kinds = t.kinds || [];
        const m = {};
        const nks = Array(names.length);
        for (let i = 0, l = names.length; i < l; i++) {
            const ki = kinds[i] || util_1.freshKMeta();
            const k = util_1.freshTSkol(names[i], ki);
            m[names[i]] = k;
            nks[i] = ki;
        }
        const [km, b] = inferKindR(env, types_1.substTVar(m, type));
        return [km, types_1.TForall(names, nks, b)];
    }
    return util_1.impossible('inferKindR');
};
const defaultKindInKind = (k) => {
    if (k.tag === 'KCon')
        return k;
    if (k.tag === 'KMeta') {
        if (k.kind)
            return defaultKindInKind(k.kind);
        k.kind = kinds_1.kType;
        return kinds_1.kType;
    }
    if (k.tag === 'KFun') {
        return kinds_1.KFun(defaultKindInKind(k.left), defaultKindInKind(k.right));
    }
    return util_1.impossible('defaultKindInKind');
};
const defaultKind = (t) => {
    if (t.tag === 'TApp')
        return types_1.TApp(defaultKind(t.left), defaultKind(t.right));
    if (t.tag === 'TForall') {
        const nks = t.kinds.map(k => k ? defaultKindInKind(k) : kinds_1.kType);
        return types_1.TForall(t.names, nks, defaultKind(t.type));
    }
    if (t.tag === 'TSkol')
        return types_1.TVar(t.name);
    if (t.tag === 'TMeta')
        return util_1.terr(`tmeta ${types_1.showTy(t)} in defaultKind`);
    return t;
};
exports.inferKind = (env, ty) => {
    const [_, ti] = inferKindR(env, ty);
    return defaultKind(ti);
};
exports.kindOf = (env, t) => {
    if (t.tag === 'TMeta')
        return t.kind;
    if (t.tag === 'TSkol')
        return t.kind;
    if (t.tag === 'TCon')
        return env_1.lookupTCon(env, t.name) ||
            util_1.terr(`undefined type constructor ${types_1.showTy(t)}`);
    if (t.tag === 'TApp') {
        const f = exports.kindOf(env, t.left);
        if (f.tag !== 'KFun')
            return util_1.terr(`not a kind fun in left side of type application (${types_1.showTy(t)}): ${kinds_1.showKind(f)}`);
        return f.right;
    }
    return util_1.terr(`unexpected type ${types_1.showTy(t)} in kindOf`);
};

},{"./env":5,"./kinds":8,"./types":13,"./util":15}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
exports.KCon = (name) => ({ tag: 'KCon', name });
exports.KFun = (left, right) => ({ tag: 'KFun', left, right });
exports.kfunFrom = (ks) => ks.reduceRight((x, y) => exports.KFun(y, x));
exports.KMeta = (id) => ({ tag: 'KMeta', id, kind: null });
exports.kType = exports.KCon('Type');
exports.flattenKFun = (ki) => {
    const r = [];
    let c = ki;
    while (c.tag === 'KFun') {
        r.push(c.left);
        c = c.right;
    }
    r.push(c);
    return r;
};
exports.showKind = (ki) => {
    if (ki.tag === 'KCon')
        return ki.name;
    if (ki.tag === 'KFun')
        return exports.flattenKFun(ki)
            .map(k => k.tag === 'KFun' ? `(${exports.showKind(k)})` : exports.showKind(k))
            .join(' -> ');
    if (ki.tag === 'KMeta')
        return `?${ki.id}`;
    return util_1.impossible('showKind');
};
exports.pruneKind = (ki) => {
    switch (ki.tag) {
        case 'KFun':
            return exports.KFun(exports.pruneKind(ki.left), exports.pruneKind(ki.right));
        case 'KMeta': {
            if (!ki.kind)
                return ki;
            const k = exports.pruneKind(ki.kind);
            ki.kind = k;
            return k;
        }
        default: return ki;
    }
};
exports.occursKMeta = (x, k) => {
    if (x === k)
        return true;
    if (k.tag === 'KFun')
        return exports.occursKMeta(x, k.left) || exports.occursKMeta(x, k.right);
    return false;
};
exports.eqKind = (a, b) => {
    if (a === b)
        return true;
    if (a.tag === 'KCon')
        return b.tag === 'KCon' && a.name === b.name;
    if (a.tag === 'KFun')
        return b.tag === 'KFun' && exports.eqKind(a.left, b.left)
            && exports.eqKind(a.right, b.right);
    return false;
};

},{"./util":15}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const List_1 = require("./List");
exports.MVar = (name) => ({ tag: 'MVar', name });
exports.MApp = (left, right) => ({ tag: 'MApp', left, right });
exports.mappFrom = (ts) => ts.reduce(exports.MApp);
exports.MAbs = (name, body) => ({ tag: 'MAbs', name, body });
exports.mabs = (ns, body) => ns.reduceRight((x, y) => exports.MAbs(y, x), body);
exports.MConst = (val) => ({ tag: 'MConst', val });
exports.MInc = (term) => ({ tag: 'MInc', term });
exports.showMTerm = (term) => {
    if (term.tag === 'MVar')
        return term.name;
    if (term.tag === 'MConst')
        return `${term.val}`;
    if (term.tag === 'MAbs')
        return `(\\${term.name} -> ${exports.showMTerm(term.body)})`;
    if (term.tag === 'MApp')
        return `(${exports.showMTerm(term.left)} ${exports.showMTerm(term.right)})`;
    if (term.tag === 'MInc')
        return `(inc ${exports.showMTerm(term.term)})`;
    return util_1.impossible('showMTerm');
};
const freeMTerm = (term, fr = {}) => {
    if (term.tag === 'MVar') {
        fr[term.name] = true;
        return fr;
    }
    if (term.tag === 'MAbs') {
        freeMTerm(term.body, fr);
        fr[term.name] = false;
        return fr;
    }
    if (term.tag === 'MApp') {
        freeMTerm(term.left, fr);
        return freeMTerm(term.right, fr);
    }
    if (term.tag === 'MInc')
        return freeMTerm(term.term, fr);
    return fr;
};
exports.patToMachine = (pat) => {
    if (pat.tag === 'PWildcard')
        return '_';
    if (pat.tag === 'PVar')
        return pat.name;
    if (pat.tag === 'PAnn')
        return exports.patToMachine(pat.pat);
    if (pat.tag === 'PCon')
        return exports.patToMachine(pat.pat);
    return util_1.impossible('patToMachine');
};
exports.termToMachine = (term) => {
    if (term.tag === 'Var')
        return exports.MVar(term.name);
    if (term.tag === 'Abs')
        return exports.MAbs(exports.patToMachine(term.pat), exports.termToMachine(term.body));
    if (term.tag === 'App')
        return exports.MApp(exports.termToMachine(term.left), exports.termToMachine(term.right));
    if (term.tag === 'Let')
        return exports.MApp(exports.MAbs(term.name, exports.termToMachine(term.body)), exports.termToMachine(term.val));
    if (term.tag === 'Ann')
        return exports.termToMachine(term.term);
    return util_1.impossible('termToMachine');
};
const Clos = (abs, env) => ({ tag: 'Clos', abs, env });
const VConst = (val) => ({ tag: 'VConst', val });
exports.showVal = (v) => v.tag === 'VConst' ? `${v.val}` : `Clos(${exports.showMTerm(v.abs)}, ${showEnv(v.env)})`;
const extend = (env, k, v) => List_1.default.cons([k, v], env);
const lookup = (env, k) => {
    const r = env.first(([k2, _]) => k === k2);
    if (r)
        return r[1];
    return null;
};
const showEnv = (env) => env.toString(([k, v]) => `${k} = ${exports.showVal(v)}`);
const FFun = (fn) => ({ tag: 'FFun', fn });
const FArg = (term, env) => ({ tag: 'FArg', term, env });
const FInc = { tag: 'FInc' };
const showFrame = (f) => {
    if (f.tag === 'FFun')
        return `FFun(${exports.showVal(f.fn)})`;
    if (f.tag === 'FArg')
        return `FArg(${exports.showMTerm(f.term)}, ${showEnv(f.env)})`;
    if (f.tag === 'FInc')
        return `FInc`;
    return util_1.impossible('showFrame');
};
const State = (term, env = List_1.default.nil(), stack = List_1.default.nil()) => ({ term, env, stack });
exports.showState = (s) => `State(${exports.showMTerm(s.term)}, ${showEnv(s.env)}, ${s.stack.toString(showFrame)})`;
const makeClos = (term, env) => {
    const f = freeMTerm(term);
    const nenv = env.filter(([x, _]) => f[x]);
    return Clos(term, nenv);
};
const step = (state) => {
    const { term, env, stack } = state;
    if (term.tag === 'MVar') {
        const v = lookup(env, term.name);
        if (!v)
            return null;
        if (v.tag === 'VConst')
            return State(exports.MConst(v.val), env, stack);
        return State(v.abs, v.env, stack);
    }
    if (term.tag === 'MApp')
        return State(term.left, env, List_1.default.cons(FArg(term.right, env), stack));
    if (term.tag === 'MInc')
        return State(term.term, env, List_1.default.cons(FInc, stack));
    if (stack.isNonEmpty()) {
        const top = stack.head();
        const tail = stack.tail();
        if (term.tag === 'MAbs' && top.tag === 'FArg')
            return State(top.term, top.env, List_1.default.cons(FFun(makeClos(term, env)), tail));
        if (term.tag === 'MAbs' && top.tag === 'FFun') {
            const abs = top.fn.abs;
            return State(abs.body, extend(top.fn.env, abs.name, makeClos(term, env)), tail);
        }
        if (term.tag === 'MConst' && top.tag === 'FFun') {
            const abs = top.fn.abs;
            return State(abs.body, extend(top.fn.env, abs.name, VConst(term.val)), tail);
        }
        if (term.tag === 'MConst' && top.tag === 'FInc')
            return State(exports.MConst(term.val + 1), env, tail);
    }
    return null;
};
const steps = (state) => {
    let c = state;
    while (true) {
        // console.log(showState(c));
        const next = step(c);
        if (!next)
            return c;
        c = next;
    }
};
exports.runState = (term) => steps(State(exports.termToMachine(term)));
exports.runVal = (term) => {
    const st = exports.runState(term);
    const t = st.term;
    return t.tag === 'MConst' ? VConst(t.val) : Clos(t, st.env);
};

},{"./List":1,"./util":15}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const config_1 = require("./config");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const terms_1 = require("./terms");
const definitions_1 = require("./definitions");
const err = (msg) => { throw new SyntaxError(msg); };
const VarT = (val) => ({ tag: 'VarT', val });
const matchVarT = (val, t) => t.tag === 'VarT' && t.val === val;
const SymbolT = (val) => ({ tag: 'SymbolT', val });
const matchSymbolT = (val, t) => t.tag === 'SymbolT' && t.val === val;
const StringT = (val) => ({ tag: 'StringT', val });
const NumberT = (val) => ({ tag: 'NumberT', val });
const ParenT = (val) => ({ tag: 'ParenT', val });
const showToken = (t) => {
    switch (t.tag) {
        case 'SymbolT':
        case 'VarT': return t.val;
        case 'StringT': return JSON.stringify(t.val);
        case 'ParenT': return `(${t.val.map(showToken).join(' ')})`;
        case 'NumberT': return t.val;
    }
};
const showTokens = (ts) => ts.map(showToken).join(' ');
const matchingBracket = (c) => {
    if (c === '(')
        return ')';
    if (c === ')')
        return '(';
    return err(`invalid bracket: ${c}`);
};
const SYM1 = ['\\', ':', '.', '=', '?', '_'];
const SYM2 = ['->', '<|', '|>', '<<', '>>'];
const KEYWORDS = ['let', 'in', 'type'];
const KEYWORDS_TYPE = ['forall', 'let', 'type'];
const KEYWORDS_DEF = ['let', 'type'];
const START = 0;
const NAME = 1;
const STRING = 2;
const COMMENT = 3;
const NUMBER = 4;
const tokenize = (sc) => {
    let state = START;
    let r = [];
    let t = '';
    let p = [], b = [];
    for (let i = 0, l = sc.length; i <= l; i++) {
        const c = sc[i] || ' ';
        const next = sc[i + 1] || '';
        config_1.log(`${i};${c};${next};${state};${showTokens(r)}`);
        if (state === START) {
            if (SYM2.indexOf(c + next) >= 0)
                r.push(SymbolT(c + next)), i++;
            else if (SYM1.indexOf(c) >= 0)
                r.push(SymbolT(c));
            else if (c === ';')
                state = COMMENT;
            else if (c === '"')
                state = STRING;
            else if (/[a-z]/i.test(c))
                t += c, state = NAME;
            else if (/[0-9]/i.test(c))
                t += c, state = NUMBER;
            else if (c === '(')
                b.push(c), p.push(r), r = [];
            else if (c === ')') {
                if (b.length === 0)
                    return err(`unmatched bracket: ${c}`);
                const br = b.pop();
                if (matchingBracket(br) !== c)
                    return err(`unmatched bracket: ${br} and ${c}`);
                const a = p.pop();
                a.push(ParenT(r));
                r = a;
            }
            else if (/\s/.test(c))
                continue;
            else
                return err(`invalid char ${c} in tokenize`);
        }
        else if (state === NAME) {
            if (!/[a-z0-9]/i.test(c)) {
                r.push(VarT(t));
                t = '', i--, state = START;
            }
            else
                t += c;
        }
        else if (state === NUMBER) {
            if (!/[0-9]/.test(c)) {
                r.push(NumberT(t));
                t = '', i--, state = START;
            }
            else
                t += c;
        }
        else if (state === STRING) {
            if (c === '"') {
                r.push(StringT(t));
                t = '', state = START;
            }
            else
                t += c;
        }
        else if (state === COMMENT) {
            if (c === '\n')
                state = START;
        }
    }
    if (b.length > 0)
        return err(`unclosed brackets: ${b.join(' ')}`);
    if (state !== START)
        return err('invalid tokenize end state');
    return r;
};
const indexOf = (a, fn) => {
    for (let i = 0, l = a.length; i < l; i++)
        if (fn(a[i]))
            return i;
    return -1;
};
const contains = (a, fn) => indexOf(a, fn) >= 0;
const splitTokens = (a, fn) => {
    const r = [];
    let t = [];
    for (let i = 0, l = a.length; i < l; i++) {
        const c = a[i];
        if (fn(c)) {
            r.push(t);
            t = [];
        }
        else
            t.push(c);
    }
    r.push(t);
    return r;
};
// kinds
const parseTokenKind = (ts) => {
    config_1.log(`parseTokenKind ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': return kinds_1.KCon(ts.val);
        case 'SymbolT': return err(`stuck on ${ts.val}`);
        case 'ParenT': return parseParensKind(ts.val);
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
        case 'NumberT': return err(`stuck on ${ts.val}`);
    }
};
const parseParensKind = (ts) => {
    config_1.log(`parseParensKind ${showTokens(ts)}`);
    if (ts.length === 0)
        return err('empty kind');
    if (ts.length === 1)
        return parseTokenKind(ts[0]);
    let args = [];
    const fs = [];
    for (let i = 0; i < ts.length; i++) {
        const c = ts[i];
        if (matchSymbolT('->', c)) {
            fs.push(args);
            args = [];
            continue;
        }
        args.push(c);
    }
    fs.push(args);
    return kinds_1.kfunFrom(fs.map(ts => {
        if (ts.length === 0)
            return err(`empty kind ->`);
        if (ts.length > 1)
            return err(`kind applications unimplemented`);
        return parseTokenKind(ts[0]);
    }));
};
// types
const isCon = (x) => /[A-Z]/.test(x[0]);
const parseTokenType = (ts) => {
    config_1.log(`parseTokenType ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS_TYPE.indexOf(ts.val) >= 0)
                return err(`stuck on ${ts.val}`);
            return isCon(ts.val) ? types_1.TCon(ts.val) : types_1.TVar(ts.val);
        }
        case 'SymbolT': {
            if (ts.val === '->')
                return types_1.tFun;
            return err(`stuck on ${ts.val}`);
        }
        case 'ParenT': return parseParensType(ts.val);
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
        case 'NumberT': return err(`stuck on ${ts.val}`);
    }
};
const parseTypePat = (ts) => {
    config_1.log(`parseTypePat ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS_TYPE.indexOf(ts.val) >= 0 || isCon(ts.val))
                return err(`stuck on ${ts.val}`);
            return [[ts.val, null]];
        }
        case 'SymbolT': return err(`stuck on ${ts.val}`);
        case 'ParenT': {
            const parts = splitTokens(ts.val, t => matchSymbolT(':', t));
            if (parts.length !== 2)
                return err(`invalid use of : in forall argument`);
            const as = parts[0].map(t => {
                if (t.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(t.val) >= 0)
                    return err(`not a valid arg in forall: ${t.val}`);
                return t.val;
            });
            const ki = parseParensKind(parts[1]);
            return as.map(x => [x, ki]);
        }
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
        case 'NumberT': return err(`stuck on ${ts.val}`);
    }
};
const parseParensType = (ts) => {
    config_1.log(`parseParensType ${showTokens(ts)}`);
    if (ts.length === 0)
        return err('empty type');
    if (ts.length === 1)
        return parseTokenType(ts[0]);
    if (matchVarT('forall', ts[0])) {
        const args = [];
        let i = 1;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no . after forall`);
            if (matchSymbolT('.', c))
                break;
            const ps = parseTypePat(c);
            for (let j = 0; j < ps.length; j++)
                args.push(ps[j]);
        }
        if (args.length === 0)
            return err(`forall without args`);
        const body = parseParensType(ts.slice(i));
        return types_1.tforall(args, body);
    }
    let args = [];
    const fs = [];
    for (let i = 0; i < ts.length; i++) {
        const c = ts[i];
        if (matchVarT('forall', c)) {
            const rest = parseParensType(ts.slice(i));
            const app = types_1.tappFrom(args.map(parseTokenType).concat([rest]));
            return types_1.tfunFrom(fs.map(ts => types_1.tappFrom(ts.map(parseTokenType))).concat([app]));
        }
        if (matchSymbolT('->', c)) {
            fs.push(args);
            args = [];
            continue;
        }
        args.push(c);
    }
    fs.push(args);
    if (fs.length === 2) {
        // special case (t ->)
        if (fs[1].length === 0) {
            return types_1.TApp(types_1.tFun, types_1.tappFrom(fs[0].map(parseTokenType)));
            // special case (-> t)
        }
        else if (fs[0].length === 0) {
            return types_1.TApp(types_1.tFun, types_1.tappFrom(fs[1].map(parseTokenType)));
        }
    }
    return types_1.tfunFrom(fs.map(ts => {
        if (ts.length === 0)
            return err(`empty type ->`);
        return types_1.tappFrom(ts.map(parseTokenType));
    }));
};
// terms
const parseToken = (ts) => {
    config_1.log(`parseToken ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS.indexOf(ts.val) >= 0)
                return err(`stuck on ${ts.val}`);
            return terms_1.Var(ts.val);
        }
        case 'SymbolT': {
            if (ts.val === '<|') {
                const f = 'f';
                const x = 'x';
                return terms_1.abs([terms_1.PVar(f), terms_1.PVar(x)], terms_1.appFrom([terms_1.Var(f), terms_1.Var(x)]));
            }
            if (ts.val === '|>') {
                const f = 'f';
                const x = 'x';
                return terms_1.abs([terms_1.PVar(x), terms_1.PVar(f)], terms_1.appFrom([terms_1.Var(f), terms_1.Var(x)]));
            }
            if (ts.val === '<<') {
                const f = 'f';
                const g = 'g';
                const x = 'x';
                return terms_1.abs([terms_1.PVar(f), terms_1.PVar(g), terms_1.PVar(x)], terms_1.App(terms_1.Var(f), terms_1.App(terms_1.Var(g), terms_1.Var(x))));
            }
            if (ts.val === '>>') {
                const f = 'f';
                const g = 'g';
                const x = 'x';
                return terms_1.abs([terms_1.PVar(g), terms_1.PVar(f), terms_1.PVar(x)], terms_1.App(terms_1.Var(f), terms_1.App(terms_1.Var(g), terms_1.Var(x))));
            }
            return err(`stuck on ${ts.val}`);
        }
        case 'ParenT': return parseParens(ts.val);
        case 'StringT': {
            const val = ts.val;
            const r = Array(val.length);
            const l = val.length;
            for (let i = 0; i < l; i++)
                r[i] = val.charCodeAt(i);
            let c = terms_1.Var('nil');
            const cons = terms_1.Var('cons');
            const s = terms_1.Var('s');
            for (let i = l - 1; i >= 0; i--) {
                const n = r[i];
                let t = terms_1.Var('z');
                for (let j = 0; j < n; j++)
                    t = terms_1.App(s, t);
                c = terms_1.appFrom([cons, t, c]);
            }
            return terms_1.App(terms_1.Var('Str'), c);
        }
        case 'NumberT': {
            const val = ts.val;
            const n = parseInt(val, 10);
            if (isNaN(n) || n < 0)
                return err(`invalid number: ${val}`);
            let c = terms_1.Var('z');
            const s = terms_1.Var('s');
            for (let i = 0; i < n; i++)
                c = terms_1.App(s, c);
            return c;
        }
    }
};
const parsePat = (ts) => {
    config_1.log(`parsePat ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS.indexOf(ts.val) >= 0 || isCon(ts.val))
                return err(`stuck on ${ts.val}`);
            return [terms_1.PVar(ts.val)];
        }
        case 'SymbolT': {
            if (ts.val === '_')
                return [terms_1.PWildcard];
            return err(`stuck on ${ts.val}`);
        }
        case 'ParenT': {
            const a = ts.val;
            if (a.length === 1)
                return [terms_1.PWildcard];
            if (a.length === 2 && a[0].tag === 'VarT' && isCon(a[0].val)) {
                const con = a[0].val;
                const pat = parsePat(a[1]);
                if (pat.length !== 1)
                    return err(`con with too many arguments: ${con}`);
                return [terms_1.PCon(con, pat[0])];
            }
            const args = [];
            let i = 0;
            while (true) {
                const c = a[i++];
                if (!c)
                    return err(`no : in abs annotation`);
                if (matchSymbolT(':', c))
                    break;
                const ps = parsePat(c);
                for (let j = 0; j < ps.length; j++)
                    args.push(ps[j]);
            }
            if (args.length === 0)
                return err(`empty args in abs annotation`);
            const ty = parseParensType(a.slice(i));
            return args.map(p => terms_1.PAnn(p, ty));
        }
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
        case 'NumberT': return err(`stuck on ${ts.val}`);
    }
};
const parseParens = (ts) => {
    config_1.log(`parseParens ${showTokens(ts)}`);
    if (ts.length === 0)
        return err('empty');
    if (ts.length === 1)
        return parseToken(ts[0]);
    if (contains(ts, t => matchSymbolT(':', t))) {
        const parts = splitTokens(ts, t => matchSymbolT(':', t));
        if (parts.length !== 2)
            return err(`invalid use (${parts.length}) of :`);
        const left = parseParens(parts[0]);
        const right = parseParensType(parts[1]);
        return terms_1.Ann(left, right);
    }
    if (matchSymbolT('\\', ts[0])) {
        const args = [];
        let i = 1;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no -> after \\`);
            if (matchSymbolT('->', c))
                break;
            const ps = parsePat(c);
            for (let j = 0; j < ps.length; j++)
                args.push(ps[j]);
        }
        if (args.length === 0)
            args.push(terms_1.PWildcard);
        const body = parseParens(ts.slice(i));
        return terms_1.abs(args, body);
    }
    if (matchVarT('let', ts[0])) {
        if (ts.length < 2)
            return err(`let without name`);
        if (ts[1].tag !== 'VarT' || isCon(ts[0].val))
            return err(`invalid name for let`);
        const name = ts[1].val;
        const args = [];
        let i = 2;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no = after let`);
            if (matchSymbolT('=', c))
                break;
            const ps = parsePat(c);
            for (let j = 0; j < ps.length; j++)
                args.push(ps[j]);
        }
        const bodyts = [];
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no in after = in let`);
            if (matchVarT('in', c))
                break;
            bodyts.push(c);
        }
        const body = parseParens(bodyts);
        const rest = parseParens(ts.slice(i));
        return terms_1.Let(name, args.length > 0 ? terms_1.abs(args.slice(1), body) : body, rest);
    }
    if (contains(ts, t => matchSymbolT('<|', t))) {
        const split = splitTokens(ts, t => matchSymbolT('<|', t));
        // special case
        if (split.length === 2) {
            // (f <|) = \x -> f x
            if (split[1].length === 0) {
                const f = parseParens(split[0]);
                const x = '_x';
                return terms_1.abs([terms_1.PVar(x)], terms_1.App(f, terms_1.Var(x)));
                // (<| x) = \f -> f x
            }
            else if (split[0].length === 0) {
                const f = '_f';
                const x = parseParens(split[1]);
                return terms_1.abs([terms_1.PVar(f)], terms_1.App(terms_1.Var(f), x));
            }
        }
        const terms = split.map(parseParens);
        return terms.reduceRight((x, y) => terms_1.App(y, x));
    }
    if (contains(ts, t => matchSymbolT('|>', t))) {
        const split = splitTokens(ts, t => matchSymbolT('|>', t));
        // special case
        if (split.length === 2) {
            // (x |>) = \f -> f x
            if (split[1].length === 0) {
                const f = '_f';
                const x = parseParens(split[0]);
                return terms_1.abs([terms_1.PVar(f)], terms_1.App(terms_1.Var(f), x));
                // (|> f) = \x -> f x
            }
            else if (split[0].length === 0) {
                const f = parseParens(split[1]);
                const x = '_x';
                return terms_1.abs([terms_1.PVar(x)], terms_1.App(f, terms_1.Var(x)));
            }
        }
        const terms = split.map(parseParens);
        return terms.reverse().reduceRight((x, y) => terms_1.App(y, x));
    }
    if (contains(ts, t => matchSymbolT('<<', t))) {
        const split = splitTokens(ts, t => matchSymbolT('<<', t));
        // special case
        if (split.length === 2) {
            // (f <<) = \g x -> f (g x)
            if (split[1].length === 0) {
                const f = parseParens(split[0]);
                const g = '_g';
                const x = '_x';
                return terms_1.abs([terms_1.PVar(g), terms_1.PVar(x)], terms_1.App(f, terms_1.App(terms_1.Var(g), terms_1.Var(x))));
                // (<< g) = \f x -> f (g x)
            }
            else if (split[0].length === 0) {
                const f = '_f';
                const g = parseParens(split[1]);
                const x = '_x';
                return terms_1.abs([terms_1.PVar(f), terms_1.PVar(x)], terms_1.App(terms_1.Var(f), terms_1.App(g, terms_1.Var(x))));
            }
        }
        const terms = split.map(parseParens);
        const x = '_x';
        return terms_1.abs([terms_1.PVar(x)], terms.reduceRight((x, y) => terms_1.App(y, x), terms_1.Var(x)));
    }
    if (contains(ts, t => matchSymbolT('>>', t))) {
        const split = splitTokens(ts, t => matchSymbolT('>>', t));
        // special case
        if (split.length === 2) {
            // (f >>) = \g x -> g (f x)
            if (split[1].length === 0) {
                const f = parseParens(split[0]);
                const g = '_g';
                const x = '_x';
                return terms_1.abs([terms_1.PVar(g), terms_1.PVar(x)], terms_1.App(terms_1.Var(g), terms_1.App(f, terms_1.Var(x))));
                // (>> g) = \f x -> g (f x)
            }
            else if (split[0].length === 0) {
                const f = '_f';
                const g = parseParens(split[1]);
                const x = '_x';
                return terms_1.abs([terms_1.PVar(f), terms_1.PVar(x)], terms_1.App(g, terms_1.App(terms_1.Var(f), terms_1.Var(x))));
            }
        }
        const terms = split.map(parseParens);
        const x = '_x';
        return terms_1.abs([terms_1.PVar(x)], terms.reverse().reduceRight((x, y) => terms_1.App(y, x), terms_1.Var(x)));
    }
    const args = [];
    for (let i = 0; i < ts.length; i++) {
        const c = ts[i];
        if (matchSymbolT('\\', c)) {
            args.push(parseParens(ts.slice(i)));
            return terms_1.appFrom(args);
        }
        args.push(parseToken(c));
    }
    return terms_1.appFrom(args);
};
// definitions
const parseParensDefs = (ts) => {
    if (ts.length === 0)
        return [];
    if (matchVarT('type', ts[0])) {
        if (ts[1].tag !== 'VarT' || !isCon(ts[1].val))
            return err(`invalid type name: ${ts[1].val}`);
        const tname = ts[1].val;
        const args = [];
        let i = 2;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no = after type def`);
            if (matchSymbolT('=', c))
                break;
            const ps = parseTypePat(c);
            for (let j = 0; j < ps.length; j++)
                args.push(ps[j]);
        }
        const bodyts = [];
        while (true) {
            const c = ts[i++];
            if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0))
                break;
            bodyts.push(c);
        }
        const body = parseParensType(bodyts);
        const rest = parseParensDefs(ts.slice(i - 1));
        return [definitions_1.DType(tname, args, body)].concat(rest);
    }
    if (matchVarT('let', ts[0])) {
        if (ts.length < 2)
            return err(`let without name`);
        if (ts[1].tag !== 'VarT' || isCon(ts[0].val))
            return err(`invalid name for let`);
        const name = ts[1].val;
        const args = [];
        let i = 2;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no = after let`);
            if (matchSymbolT('=', c))
                break;
            const ps = parsePat(c);
            for (let j = 0; j < ps.length; j++)
                args.push(ps[j]);
        }
        const bodyts = [];
        while (true) {
            const c = ts[i++];
            if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0))
                break;
            bodyts.push(c);
        }
        const body = parseParens(bodyts);
        const rest = parseParensDefs(ts.slice(i - 1));
        return [definitions_1.DLet(name, args, body)].concat(rest);
    }
    return err(`def stuck on ${ts[0].val}`);
};
// parsing
exports.parseKind = (sc) => {
    const ts = tokenize(sc);
    const ex = parseParensKind(ts);
    return ex;
};
exports.parseType = (sc) => {
    const ts = tokenize(sc);
    const ex = parseParensType(ts);
    return ex;
};
exports.parse = (sc) => {
    const ts = tokenize(sc);
    const ex = parseParens(ts);
    return ex;
};
exports.parseDefs = (sc) => {
    const ts = tokenize(sc);
    const ex = parseParensDefs(ts);
    return ex;
};

},{"./config":3,"./definitions":4,"./kinds":8,"./terms":12,"./types":13}],11:[function(require,module,exports){
(function (global){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const config_1 = require("./config");
const env_1 = require("./env");
const terms_1 = require("./terms");
const types_1 = require("./types");
const compiler_1 = require("./compiler");
const inference_1 = require("./inference");
const parser_1 = require("./parser");
const machine_1 = require("./machine");
const _showR = (x) => {
    if (typeof x === 'function')
        return '[Fn]';
    if (typeof x === 'string')
        return JSON.stringify(x);
    if (Array.isArray(x))
        return `[${x.map(_showR).join(', ')}]`;
    if (typeof x === 'object' && typeof x._tag === 'string') {
        if (x._tag === 'Pair')
            return `(Pair ${_showR(x.val[0])} ${_showR(x.val[1])})`;
        return x.val === null ? x._tag : `(${x._tag} ${_showR(x.val)})`;
    }
    return '' + x;
};
const _show = (x, t) => {
    if (t.tag === 'TCon' && t.name === 'Bool')
        return x('true')('false');
    if (t.tag === 'TCon' && t.name === 'Nat')
        return `${x((x) => x + 1)(0)}`;
    if (t.tag === 'TCon' && t.name === 'Str') {
        const r = [];
        x(r)((n) => (r) => {
            r.push(String.fromCharCode(n((x) => x + 1)(0)));
            return r;
        });
        return JSON.stringify(r.reverse().join(''));
    }
    if (t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === 'List') {
        const ty = t.right;
        const r = [];
        x(r)((y) => (r) => {
            r.push(_show(y, ty));
            return r;
        });
        return `[${r.reverse().join(', ')}]`;
    }
    return _showR(x);
};
const _env = env_1.initialEnv;
const _global = typeof global === 'undefined' ? 'window' : 'global';
exports.run = (_s, _cb) => {
    try {
        if (_s === ':env' || _s === ':e')
            return _cb(env_1.showEnv(_env));
        if (_s === ':showkinds' || _s === ':k') {
            config_1.config.showKinds = !config_1.config.showKinds;
            return _cb(`showKinds: ${config_1.config.showKinds}`);
        }
        if (_s === ':debug' || _s === ':d') {
            config_1.config.debug = !config_1.config.debug;
            return _cb(`debug: ${config_1.config.debug}`);
        }
        if (_s.startsWith(':load ') || _s.startsWith(':l ') || _s === ':l' || _s === ':load') {
            const _rest = (_s.startsWith(':load') ? _s.slice(5) : _s.slice(2)).trim();
            fetch(`lib/${_rest || 'prelude.p'}`)
                .then(res => res.text())
                .then(_rest => {
                const _ds = parser_1.parseDefs(_rest);
                inference_1.inferDefs(_env, _ds);
                const _c = compiler_1.compileDefs(_ds, n => `${_global}['${n}']`);
                eval(`(() => {${_c}})()`);
                return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
            })
                .catch(_err => _cb(`${_err}`, true));
            return;
        }
        if (_s.startsWith(':let ') || _s.startsWith(':type ')) {
            const _rest = _s.slice(1);
            const _ds = parser_1.parseDefs(_rest);
            inference_1.inferDefs(_env, _ds);
            const _c = compiler_1.compileDefs(_ds, n => `${_global}['${n}']`);
            eval(`(() => {${_c}})()`);
            return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
        }
        if (_s.startsWith(':cek ')) {
            const _rest = _s.slice(4);
            const _e = parser_1.parse(_s);
            const _st = machine_1.runState(_e);
            return _cb(machine_1.showState(_st));
        }
        if (_s.startsWith(':cek ')) {
            const _rest = _s.slice(4);
            const _e = parser_1.parse(_rest);
            const _st = machine_1.runState(_e);
            return _cb(machine_1.showState(_st));
        }
        if (_s.startsWith(':cekv ')) {
            const _rest = _s.slice(5);
            const _e = parser_1.parse(_rest);
            const _st = machine_1.runVal(_e);
            return _cb(machine_1.showVal(_st));
        }
        if (_s.startsWith(':t')) {
            const _rest = _s.slice(2);
            const _e = parser_1.parse(_rest);
            const _t = inference_1.infer(_env, _e);
            return _cb(types_1.showTy(_t));
        }
        const _e = parser_1.parse(_s);
        config_1.log(terms_1.showTerm(_e));
        const _t = inference_1.infer(_env, _e);
        config_1.log(types_1.showTy(_t));
        const _c = compiler_1.compile(_e);
        config_1.log(_c);
        const _v = eval(_c);
        config_1.log(_v);
        return _cb(`${_show(_v, _t)} : ${types_1.showTy(_t)}`);
    }
    catch (_err) {
        return _cb(`${_err}`, true);
    }
};

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./compiler":2,"./config":3,"./env":5,"./inference":6,"./machine":9,"./parser":10,"./terms":12,"./types":13}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const types_1 = require("./types");
exports.Var = (name) => ({ tag: 'Var', name });
exports.App = (left, right) => ({ tag: 'App', left, right });
exports.appFrom = (ts) => ts.reduce(exports.App);
exports.Abs = (pat, body) => ({ tag: 'Abs', pat, body });
exports.abs = (ns, body) => ns.reduceRight((x, y) => exports.Abs(y, x), body);
exports.Let = (name, val, body) => ({ tag: 'Let', name, val, body });
exports.Ann = (term, type) => ({ tag: 'Ann', term, type });
exports.PVar = (name) => ({ tag: 'PVar', name });
exports.PWildcard = ({ tag: 'PWildcard' });
exports.PAnn = (pat, type) => ({ tag: 'PAnn', pat, type });
exports.PCon = (name, pat) => ({ tag: 'PCon', name, pat });
exports.showPat = (p) => {
    if (p.tag === 'PVar')
        return p.name;
    if (p.tag === 'PWildcard')
        return '_';
    if (p.tag === 'PAnn')
        return `(${exports.showPat(p.pat)} : ${types_1.showTy(p.type)})`;
    if (p.tag === 'PCon')
        return `(${p.name} ${exports.showPat(p.pat)})`;
    return util_1.impossible('showPat');
};
exports.showTerm = (t) => {
    if (t.tag === 'Var')
        return t.name;
    if (t.tag === 'Abs')
        return `(\\${exports.showPat(t.pat)} -> ${exports.showTerm(t.body)})`;
    if (t.tag === 'App')
        return `(${exports.showTerm(t.left)} ${exports.showTerm(t.right)})`;
    if (t.tag === 'Ann')
        return `(${exports.showTerm(t.term)} : ${types_1.showTy(t.type)})`;
    if (t.tag === 'Let')
        return `(let ${t.name} = ${exports.showTerm(t.val)} in ${exports.showTerm(t.body)})`;
    return util_1.impossible('showTerm');
};

},{"./types":13,"./util":15}],13:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const kinds_1 = require("./kinds");
const config_1 = require("./config");
exports.TForall = (names, kinds, type) => ({ tag: 'TForall', names, kinds, type });
exports.tforall = (ns, type) => {
    const [names, kinds] = ns.reduce((c, [x, k]) => {
        c[0].push(x);
        c[1].push(k);
        return c;
    }, [[], []]);
    return exports.TForall(names, kinds, type);
};
exports.TApp = (left, right) => ({ tag: 'TApp', left, right });
exports.tappFrom = (ts) => ts.reduce(exports.TApp);
exports.TCon = (name) => ({ tag: 'TCon', name });
exports.TVar = (name) => ({ tag: 'TVar', name });
exports.TSkol = (name, id, kind) => ({ tag: 'TSkol', name, id, kind });
exports.TMeta = (id, kind, name = null) => ({ tag: 'TMeta', id, kind, name, type: null });
exports.tFun = exports.TCon('->');
exports.TFun = (left, right) => exports.TApp(exports.TApp(exports.tFun, left), right);
exports.isTFun = (ty) => ty.tag === 'TApp' && ty.left.tag === 'TApp' &&
    (ty.left.left === exports.tFun ||
        (ty.left.left.tag === 'TCon' &&
            ty.left.left.name === exports.tFun.name));
exports.tfunFrom = (ts) => ts.reduceRight((x, y) => exports.TFun(y, x));
exports.flattenTFun = (t) => {
    let c = t;
    const r = [];
    while (exports.isTFun(c)) {
        r.push(c.left.right);
        c = c.right;
    }
    r.push(c);
    return r;
};
exports.flattenTApp = (t) => {
    let c = t;
    const r = [];
    while (c.tag === 'TApp') {
        r.push(c.right);
        c = c.left;
    }
    r.push(c);
    return r.reverse();
};
exports.showTy = (t) => {
    if (t.tag === 'TCon')
        return t.name;
    if (t.tag === 'TVar')
        return t.name;
    if (t.tag === 'TMeta')
        return `?${t.name ? `${t.name}\$` : ''}${t.id}`;
    if (t.tag === 'TSkol')
        return `'${t.name}\$${t.id}`;
    if (t.tag === 'TForall')
        return `forall ${t.names.map((tv, i) => t.kinds[i] && config_1.config.showKinds ?
            `(${tv} : ${kinds_1.showKind(t.kinds[i])})` :
            `${tv}`).join(' ')}. ${exports.showTy(t.type)}`;
    if (exports.isTFun(t))
        return exports.flattenTFun(t)
            .map(t => exports.isTFun(t) || t.tag === 'TForall' ? `(${exports.showTy(t)})` : exports.showTy(t))
            .join(' -> ');
    if (t.tag === 'TApp')
        return exports.flattenTApp(t)
            .map(t => t.tag === 'TApp' || t.tag === 'TForall' ? `(${exports.showTy(t)})` : exports.showTy(t))
            .join(' ');
    return util_1.impossible('showTy');
};
exports.substTVar = (map, ty) => {
    if (ty.tag === 'TVar')
        return map[ty.name] || ty;
    if (ty.tag === 'TApp')
        return exports.TApp(exports.substTVar(map, ty.left), exports.substTVar(map, ty.right));
    if (ty.tag === 'TForall') {
        const { names, kinds, type } = ty;
        const m = {};
        for (let k in map)
            if (names.indexOf(k) < 0)
                m[k] = map[k];
        return exports.TForall(names, kinds, exports.substTVar(m, type));
    }
    return ty;
};
exports.tmetas = (ty, free = [], tms = []) => {
    if (ty.tag === 'TMeta') {
        if (free.indexOf(ty) >= 0 || tms.indexOf(ty) >= 0)
            return tms;
        tms.push(ty);
        return tms;
    }
    if (ty.tag === 'TApp')
        return exports.tmetas(ty.right, free, exports.tmetas(ty.left, free, tms));
    if (ty.tag === 'TForall')
        return exports.tmetas(ty.type, free, tms);
    return tms;
};
exports.prune = (ty) => {
    if (ty.tag === 'TMeta') {
        if (!ty.type)
            return ty;
        const t = exports.prune(ty.type);
        ty.type = t;
        return t;
    }
    if (ty.tag === 'TApp')
        return exports.TApp(exports.prune(ty.left), exports.prune(ty.right));
    if (ty.tag === 'TForall')
        return exports.TForall(ty.names, ty.kinds, exports.prune(ty.type));
    return ty;
};
exports.occursTMeta = (x, t) => {
    if (x === t)
        return true;
    if (t.tag === 'TApp')
        return exports.occursTMeta(x, t.left) || exports.occursTMeta(x, t.right);
    if (t.tag === 'TForall')
        return exports.occursTMeta(x, t.type);
    return false;
};
exports.tbinders = (ty, bs = []) => {
    if (ty.tag === 'TApp')
        return exports.tbinders(ty.right, exports.tbinders(ty.left, bs));
    if (ty.tag === 'TForall') {
        const names = ty.names;
        for (let i = 0, l = names.length; i < l; i++) {
            const x = names[i];
            if (bs.indexOf(x) < 0)
                bs.push(x);
        }
        return exports.tbinders(ty.type, bs);
    }
    return bs;
};
exports.quantify = (tms, ty) => {
    config_1.log(`quantify ${exports.showTy(ty)} with [${tms.map(exports.showTy).join(', ')}]`);
    const len = tms.length;
    if (len === 0)
        return ty;
    const used = exports.tbinders(ty);
    const tvs = Array(len);
    const ks = Array(len);
    let i = 0;
    let l = 0;
    let j = 0;
    while (i < len) {
        const x = tms[i].name;
        const v = x && used.indexOf(x) < 0 ? x :
            `${String.fromCharCode(l + 97)}${j > 0 ? j : ''}`;
        if (used.indexOf(v) < 0) {
            used.push(v);
            tms[i].type = exports.TVar(v);
            tvs[i] = v;
            ks[i] = tms[i].kind;
            i++;
        }
        l = (l + 1) % 26;
        if (l === 0)
            j++;
    }
    return exports.TForall(tvs, ks, exports.prune(ty));
};

},{"./config":3,"./kinds":8,"./util":15}],14:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const util_1 = require("./util");
const kinds_1 = require("./kinds");
const kindInference_1 = require("./kindInference");
const config_1 = require("./config");
const bindTMeta = (env, x, t) => {
    if (x.type)
        return exports.unify(env, x.type, t);
    if (t.tag === 'TMeta' && t.type) {
        if (!x.name && t.name)
            x.name = t.name;
        return exports.unify(env, x, t.type);
    }
    if (types_1.occursTMeta(x, t))
        return util_1.terr(`${types_1.showTy(x)} occurs in ${types_1.showTy(t)}`);
    const k1 = kindInference_1.kindOf(env, x);
    const k2 = kindInference_1.kindOf(env, t);
    if (!kinds_1.eqKind(k1, k2))
        return util_1.terr(`kind mismatch in unification of ${types_1.showTy(x)} ~ ${types_1.showTy(t)}: ${kinds_1.showKind(k1)} ~ ${kinds_1.showKind(k2)}`);
    if (!x.name && t.tag === 'TMeta' && t.name)
        x.name = t.name;
    x.type = t;
};
exports.unify = (env, a, b) => {
    config_1.log(`unify ${types_1.showTy(a)} ~ ${types_1.showTy(b)}`);
    if (a.tag === 'TVar' || b.tag === 'TVar')
        return util_1.terr(`tvar in unify: ${types_1.showTy(a)} ~ ${types_1.showTy(b)}`);
    if (a === b)
        return;
    if (a.tag === 'TMeta')
        return bindTMeta(env, a, b);
    if (b.tag === 'TMeta')
        return bindTMeta(env, b, a);
    if (a.tag === 'TApp' && b.tag === 'TApp') {
        exports.unify(env, a.left, b.left);
        return exports.unify(env, a.right, b.right);
    }
    if (a.tag === 'TSkol' && b.tag === 'TSkol' && a.id === b.id)
        return;
    if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name)
        return;
    return util_1.terr(`failed to unify: ${types_1.showTy(a)} ~ ${types_1.showTy(b)}`);
};
exports.unifyTFun = (env, ty) => {
    if (types_1.isTFun(ty))
        return ty;
    const fn = types_1.TFun(util_1.freshTMeta(kinds_1.kType), util_1.freshTMeta(kinds_1.kType));
    exports.unify(env, ty, fn);
    return fn;
};

},{"./config":3,"./kindInference":7,"./kinds":8,"./types":13,"./util":15}],15:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
exports.impossible = (msg) => {
    throw new Error(`impossible: ${msg}`);
};
exports.terr = (msg) => { throw new TypeError(msg); };
let _id = 0;
exports.resetId = () => { _id = 0; };
exports.freshId = () => _id++;
exports.freshTMeta = (kind, name = null) => types_1.TMeta(exports.freshId(), kind, name);
exports.freshTSkol = (name, kind) => types_1.TSkol(name, exports.freshId(), kind);
exports.freshKMeta = () => kinds_1.KMeta(exports.freshId());
exports.skolemCheck = (sk, ty) => {
    if (ty.tag === 'TSkol' && sk.indexOf(ty) >= 0)
        return exports.terr(`skolem check failed: ${types_1.showTy(ty)}`);
    if (ty.tag === 'TApp') {
        exports.skolemCheck(sk, ty.left);
        return exports.skolemCheck(sk, ty.right);
    }
    if (ty.tag === 'TForall')
        return exports.skolemCheck(sk, ty.type);
};

},{"./kinds":8,"./types":13}],16:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const repl_1 = require("./repl");
function getOutput(s, cb) {
    repl_1.run(s, cb);
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

},{"./repl":11}]},{},[16]);

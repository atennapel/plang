(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Nil = { tag: 'Nil' };
exports.Cons = (head, tail) => ({ tag: 'Cons', head, tail });
exports.toString = (l, fn = x => `${x}`) => {
    const r = [];
    let c = l;
    while (c.tag === 'Cons') {
        r.push(fn(c.head));
        c = c.tail;
    }
    return `[${r.join(', ')}]`;
};
exports.filter = (l, fn) => l.tag === 'Cons' ? (fn(l.head) ? exports.Cons(l.head, exports.filter(l.tail, fn)) : exports.filter(l.tail, fn)) : l;
exports.first = (l, fn) => {
    let c = l;
    while (c.tag === 'Cons') {
        if (fn(c.head))
            return c.head;
        c = c.tail;
    }
    return null;
};
exports.each = (l, fn) => {
    let c = l;
    while (c.tag === 'Cons') {
        fn(c.head);
        c = c.tail;
    }
};
exports.toArray = (l, fn) => {
    let c = l;
    const r = [];
    while (c.tag === 'Cons') {
        r.push(fn(c.head));
        c = c.tail;
    }
    return r;
};
exports.append = (a, b) => a.tag === 'Cons' ? exports.Cons(a.head, exports.append(a.tail, b)) : b;

},{}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.config = {
    debug: false,
    showKinds: false,
    time: false,
};
exports.setConfig = (c) => {
    for (let k in c)
        exports.config[k] = c[k];
};
exports.log = (msg) => {
    if (exports.config.debug)
        console.log(msg());
};

},{}],3:[function(require,module,exports){
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
exports.findDef = (ds, name) => {
    for (let i = ds.length - 1; i >= 0; i--) {
        const d = ds[i];
        if (d.tag === 'DLet' && d.name === name)
            return d;
    }
    return null;
};
exports.findDefType = (ds, name) => {
    for (let i = ds.length - 1; i >= 0; i--) {
        const d = ds[i];
        if (d.tag === 'DType' && d.name === name)
            return d;
    }
    return null;
};

},{"./kinds":8,"./terms":13,"./types":14}],4:[function(require,module,exports){
(function (global){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const List_1 = require("./List");
const util_1 = require("./util");
exports.Env = (global = {}, tcons = {}, local = List_1.Nil) => ({ global, tcons, local });
const clone = (o) => {
    const n = {};
    for (let k in o)
        n[k] = o[k];
    return n;
};
exports.cloneEnv = (e) => exports.Env(clone(e.global), clone(e.tcons), e.local);
exports.showEnv = (env) => {
    const r = [];
    for (let k in env.tcons)
        r.push(`type ${k} : ${kinds_1.showKind(env.tcons[k])}`);
    for (let k in env.global)
        r.push(`${k} : ${types_1.showTy(env.global[k])}`);
    return r.join('\n');
};
exports.extendVar = (env, x, t) => exports.Env(env.global, env.tcons, List_1.Cons([x, t], env.local));
exports.extendVars = (env, vs) => {
    const local = vs.reduce((l, kv) => List_1.Cons(kv, l), env.local);
    return exports.Env(env.global, env.tcons, local);
};
exports.lookupVar = (env, x) => {
    const t = List_1.first(env.local, ([k, _]) => x === k);
    if (t)
        return t[1];
    return env.global[x] || null;
};
exports.lookupTCon = (env, x) => env.tcons[x] || null;
exports.skolemCheckEnv = (sk, env) => {
    List_1.each(env.local, ([_, t]) => util_1.skolemCheck(sk, types_1.prune(t)));
    const vars = env.global;
    for (let k in vars)
        util_1.skolemCheck(sk, types_1.prune(vars[k]));
};
exports.tmetasEnv = (env, free = [], tms = []) => {
    List_1.each(env.local, ([_, t]) => types_1.tmetas(types_1.prune(t), free, tms));
    const vars = env.global;
    for (let k in vars)
        types_1.tmetas(types_1.prune(vars[k]), free, tms);
    return tms;
};
const initialEnv = exports.Env({}, {
    '->': kinds_1.KFun(kinds_1.kType, kinds_1.KFun(kinds_1.kType, kinds_1.kType)),
});
exports.getInitialEnv = () => exports.cloneEnv(initialEnv);

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./List":1,"./kinds":8,"./types":14,"./util":16}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isBrowser = typeof window !== 'undefined';
exports.load = (lib, cb) => {
    if (exports.isBrowser) {
        fetch(`lib/${lib || 'prelude'}.p`)
            .then(x => x.text()).then(x => cb(null, x))
            .catch(err => cb(err, ''));
    }
    else {
        require('fs').readFile(`./lib/${lib || 'prelude'}.p`, 'utf8', cb);
    }
};
exports.loadPromise = (lib) => new Promise((resolve, reject) => exports.load(lib, (err, file) => err ? reject(err) : resolve(file)));
exports.store = (state) => {
    const doc = JSON.stringify(state);
    if (typeof localStorage === 'undefined') {
        require('fs').writeFileSync('_repl', doc, 'utf8');
    }
    else {
        localStorage.setItem('replstate', doc);
    }
};
exports.restore = () => {
    try {
        let doc = null;
        if (typeof localStorage === 'undefined') {
            doc = require('fs').readFileSync('.repl', 'utf8');
        }
        else {
            doc = localStorage.getItem('replstate');
        }
        if (!doc)
            return null;
        return JSON.parse(doc);
    }
    catch (err) {
        return null;
    }
};

},{"fs":18}],6:[function(require,module,exports){
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
const positivity_1 = require("./positivity");
const List_1 = require("./List");
const tBool = types_1.TCon('Bool');
const tNat = types_1.TCon('Nat');
const tChar = types_1.TCon('Char');
const tStr = types_1.TCon('Str');
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
    config_1.log(() => `tcRho ${terms_1.showTerm(term)} with ${showEx(ex)}`);
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
    if (term.tag === 'If') {
        if (ex.tag === 'Check') {
            checkRho(env, term.cond, tBool);
            tcRho(env, term.ifTrue, ex);
            tcRho(env, term.ifFalse, ex);
            return;
        }
        else if (ex.tag === 'Infer') {
            checkRho(env, term.cond, tBool);
            const t1 = inferRho(env, term.ifTrue);
            const t2 = inferRho(env, term.ifFalse);
            subsCheck(env, t1, t2);
            subsCheck(env, t2, t1);
            ex.type = t1;
            return;
        }
    }
    if (term.tag === 'LitNat') {
        instSigma(env, tNat, ex);
        return;
    }
    if (term.tag === 'LitChar') {
        instSigma(env, tChar, ex);
        return;
    }
    if (term.tag === 'LitStr') {
        instSigma(env, tStr, ex);
        return;
    }
    if (term.tag === 'Hole') {
        const ty = util_1.freshTMeta(kinds_1.kType);
        holes.push([term.name, ty, env.local]);
        instSigma(env, ty, ex);
        return;
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
    config_1.log(() => `subsCheck ${types_1.showTy(a)} <: ${types_1.showTy(b)}`);
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
let holes;
exports.infer = (env, term) => {
    config_1.log(() => `infer ${terms_1.showTerm(term)}`);
    util_1.resetId();
    holes = [];
    const nty = inferSigma(env, term);
    const ty = types_1.prune(nty);
    if (holes.length > 0)
        return util_1.terr(`${types_1.showTy(ty)}\nholes:\n\n${holes.map(([n, t, e]) => `_${n} : ${types_1.showTy(types_1.prune(t))}\n${List_1.toArray(e, ([x, t]) => `${x} : ${types_1.showTy(types_1.prune(t))}`).join('\n')}`).join('\n\n')}`);
    return ty;
};
exports.inferDef = (env, def) => {
    config_1.log(() => `inferDef ${definitions_1.showDef(def)}`);
    if (def.tag === 'DType') {
        const tname = def.name;
        if (env_1.lookupTCon(env, tname))
            return util_1.terr(`type ${tname} is already defined`);
        if (env_1.lookupVar(env, tname))
            return util_1.terr(`constructor ${tname} is already defined`);
        const t = def.type;
        const tc = types_1.TCon(tname);
        const b = types_1.tfunFrom([t, types_1.tappFrom([tc].concat(def.args.map(([x, _]) => types_1.TVar(x))))]);
        const ty = types_1.tforall(def.args, b);
        const nenv = env_1.cloneEnv(env);
        nenv.tcons[tname] = util_1.freshKMeta();
        const ti = kindInference_1.inferKind(nenv, ty);
        positivity_1.positivityCheck(tname, ty);
        env.global[tname] = ti;
        env.tcons[tname] = kinds_1.pruneKind(nenv.tcons[tname]);
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

},{"./List":1,"./config":2,"./definitions":3,"./env":4,"./kindInference":7,"./kinds":8,"./positivity":11,"./terms":13,"./types":14,"./unification":15,"./util":16}],7:[function(require,module,exports){
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

},{"./env":4,"./kinds":8,"./types":14,"./util":16}],8:[function(require,module,exports){
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

},{"./util":16}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const terms_1 = require("./terms");
const List_1 = require("./List");
const config_1 = require("./config");
exports.MVar = (name) => ({ tag: 'MVar', name });
exports.MApp = (left, right) => ({ tag: 'MApp', left, right });
exports.mappFrom = (ts) => ts.reduce(exports.MApp);
function mapp(...ts) { return exports.mappFrom(ts); }
exports.mapp = mapp;
;
exports.MAbs = (name, body) => ({ tag: 'MAbs', name, body });
exports.mabs = (ns, body) => ns.reduceRight((x, y) => exports.MAbs(y, x), body);
exports.MAtom = (val) => ({ tag: 'MAtom', val });
exports.MPair = (left, right) => ({ tag: 'MPair', left, right });
exports.MPairC = (left, right) => ({ tag: 'MPairC', left, right });
exports.showMTerm = (term) => {
    if (term.tag === 'MVar')
        return term.name;
    if (term.tag === 'MAtom')
        return term.val;
    if (term.tag === 'MAbs')
        return `(\\${term.name} -> ${exports.showMTerm(term.body)})`;
    if (term.tag === 'MApp')
        return `(${exports.showMTerm(term.left)} ${exports.showMTerm(term.right)})`;
    if (term.tag === 'MPair')
        return `(${exports.showVal(term.left)}, ${exports.showVal(term.right)})`;
    if (term.tag === 'MPairC')
        return `(${exports.showMTerm(term.left)}, ${exports.showMTerm(term.right)})`;
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
    if (term.tag === 'MPairC') {
        freeMTerm(term.left, fr);
        return freeMTerm(term.right, fr);
    }
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
const tZ = exports.MVar('z');
const tS = exports.MVar('s');
const tNil = exports.MVar('nil');
const tCons = exports.MVar('cons');
const tBZ = exports.MVar('bz');
const tBT = exports.MVar('bt');
const tBTI = exports.MVar('bti');
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
    if (term.tag === 'If')
        return exports.MApp(exports.MApp(exports.MApp(exports.MVar('if'), exports.termToMachine(term.cond)), exports.MAbs('_', exports.termToMachine(term.ifTrue))), exports.MAbs('_', exports.termToMachine(term.ifFalse)));
    if (term.tag === 'LitNat') {
        let n = term.val;
        const r = [];
        while (n > 0) {
            if (n % 2 === 0) {
                n /= 2;
                r.push(tBT);
            }
            else {
                n = (n - 1) / 2;
                r.push(tBTI);
            }
        }
        let c = tBZ;
        for (let i = r.length - 1; i >= 0; i--)
            c = exports.MApp(r[i], c);
        return c;
    }
    if (term.tag === 'LitChar') {
        const n = term.val.charCodeAt(0);
        return exports.termToMachine(terms_1.LitNat(n));
    }
    if (term.tag === 'LitStr') {
        const val = term.val;
        let c = tNil;
        for (let i = val.length - 1; i >= 0; i--)
            c = exports.MApp(exports.MApp(tCons, exports.termToMachine(terms_1.LitChar(val[i]))), c);
        return c;
    }
    return util_1.impossible('termToMachine');
};
exports.Clos = (abs, env) => ({ tag: 'Clos', abs, env });
exports.VAtom = (val) => ({ tag: 'VAtom', val });
exports.VPair = (left, right) => ({ tag: 'VPair', left, right });
exports.showVal = (v) => v.tag === 'VAtom' ? v.val :
    v.tag === 'VPair' ? `(${exports.showVal(v.left)}, ${exports.showVal(v.right)})` :
        `Clos(${exports.showMTerm(v.abs)}, ${exports.showEnv(v.env)})`;
const extend = (env, k, v) => List_1.Cons([k, v], env);
const lookup = (env, k) => {
    const r = List_1.first(env, ([k2, _]) => k === k2);
    if (r)
        return r[1];
    return null;
};
exports.showEnv = (env) => List_1.toString(env, ([k, v]) => `${k} = ${exports.showVal(v)}`);
const FArg = (term, env) => ({ tag: 'FArg', term, env });
const FFun = (abs, env) => ({ tag: 'FFun', abs, env });
const FFst = (term, env) => ({ tag: 'FFst', term, env });
const FSnd = (val) => ({ tag: 'FSnd', val });
const showFrame = (f) => {
    if (f.tag === 'FFun')
        return `FFun(${exports.showMTerm(f.abs)}, ${exports.showEnv(f.env)})`;
    if (f.tag === 'FArg')
        return `FArg(${exports.showMTerm(f.term)}, ${exports.showEnv(f.env)})`;
    if (f.tag === 'FFst')
        return `FFst(${exports.showMTerm(f.term)}, ${exports.showEnv(f.env)})`;
    if (f.tag === 'FSnd')
        return `FSnd(${exports.showVal(f.val)})`;
    return util_1.impossible('showFrame');
};
exports.State = (term, env = List_1.Nil, stack = List_1.Nil) => ({ term, env, stack });
exports.showState = (s) => `State(${exports.showMTerm(s.term)}, ${exports.showEnv(s.env)}, ${List_1.toString(s.stack, showFrame)})`;
exports.showStateMin = (s) => `State(${exports.showMTerm(s.term)}, ${s.stack.tag === 'Nil' ? '[]' : s.stack.head.tag})`;
const makeClos = (term, env) => {
    const f = freeMTerm(term);
    const got = {};
    const nenv = List_1.filter(env, ([x, _]) => {
        const free = f[x];
        if (free) {
            if (!got[x]) {
                got[x] = true;
                return true;
            }
            return false;
        }
        return false;
    });
    // const nenv = filter(env, ([x, _]) => f[x]);
    return exports.Clos(term, nenv);
};
const step = (state, global) => {
    const { term, env, stack } = state;
    if (term.tag === 'MVar') {
        let v = lookup(env, term.name) || global[term.name];
        if (!v)
            return null;
        if (v.tag === 'Clos')
            return exports.State(v.abs, v.env, stack);
        return exports.State(v.tag === 'VAtom' ? exports.MAtom(v.val) : exports.MPair(v.left, v.right), env, stack);
    }
    if (term.tag === 'MApp')
        return exports.State(term.left, env, List_1.Cons(FArg(term.right, env), stack));
    if (term.tag === 'MPairC')
        return exports.State(term.left, env, List_1.Cons(FFst(term.right, env), stack));
    if (stack.tag === 'Nil')
        return null;
    const top = stack.head;
    const tail = stack.tail;
    if (term.tag === 'MAbs') {
        if (top.tag === 'FArg')
            return exports.State(top.term, top.env, List_1.Cons(FFun(term, env), tail));
        if (top.tag === 'FFun') {
            const { name, body } = top.abs;
            return exports.State(body, extend(top.env, name, makeClos(term, env)), tail);
        }
        if (top.tag === 'FFst')
            return exports.State(top.term, top.env, List_1.Cons(FSnd(makeClos(term, env)), tail));
        if (top.tag === 'FSnd')
            return exports.State(exports.MPair(top.val, makeClos(term, env)), env, tail);
    }
    if (term.tag === 'MAtom') {
        if (top.tag === 'FArg')
            return null;
        if (top.tag === 'FFun') {
            const { name, body } = top.abs;
            return exports.State(body, extend(top.env, name, exports.VAtom(term.val)), tail);
        }
        if (top.tag === 'FFst')
            return exports.State(top.term, top.env, List_1.Cons(FSnd(exports.VAtom(term.val)), tail));
        if (top.tag === 'FSnd')
            return exports.State(exports.MPair(top.val, exports.VAtom(term.val)), env, tail);
    }
    if (term.tag === 'MPair') {
        if (top.tag === 'FArg')
            return null;
        if (top.tag === 'FFun') {
            const { name, body } = top.abs;
            return exports.State(body, extend(top.env, name, exports.VPair(term.left, term.right)), tail);
        }
        if (top.tag === 'FFst')
            return exports.State(top.term, top.env, List_1.Cons(FSnd(exports.VPair(term.left, term.right)), tail));
        if (top.tag === 'FSnd')
            return exports.State(exports.MPair(top.val, exports.VPair(term.left, term.right)), env, tail);
    }
    return null;
};
exports.stepCount = 0;
exports.resetStepCount = () => { exports.stepCount = 0; };
exports.steps = (state, global) => {
    let c = state;
    while (true) {
        config_1.log(() => `${exports.stepCount}: ${exports.showStateMin(c)}`);
        const next = step(c, global);
        if (!next)
            return c;
        exports.stepCount++;
        c = next;
    }
};
exports.stepsVal = (state, global) => {
    const st = exports.steps(state, global);
    const t = st.term;
    return t.tag === 'MAtom' ? exports.VAtom(t.val) :
        t.tag === 'MPair' ? exports.VPair(t.left, t.right) :
            makeClos(t, st.env);
};
exports.runState = (term, global) => {
    const m = exports.termToMachine(term);
    return exports.steps(exports.State(m), global);
};
exports.runVal = (term, global) => {
    const st = exports.runState(term, global);
    const t = st.term;
    return t.tag === 'MAtom' ? exports.VAtom(t.val) :
        t.tag === 'MPair' ? exports.VPair(t.left, t.right) :
            makeClos(t, st.env);
};
exports.runEnv = (defs, global) => {
    for (let i = 0, l = defs.length; i < l; i++) {
        const d = defs[i];
        if (d.tag === 'DType') {
            global[d.name] = exports.Clos(exports.MAbs('x', exports.MVar('x')), List_1.Nil);
        }
        else if (d.tag === 'DLet') {
            const n = d.name;
            const t = terms_1.abs(d.args, d.term);
            const v = exports.runVal(t, global);
            global[n] = v;
        }
    }
};
/*
// testing
const v = MVar;
const z = mabs(['f', 'x'], v('x'));
const s = mabs(['n', 'f', 'x'], mapp(v('f'), mapp(v('n'), v('f'), v('x'))));
const inc = mabs(['x'], MPairC(MAtom('S'), v('x')));
const st = mapp(mapp(s, mapp(s, z)), inc, MAtom('Z'));
steps(State(st));
*/

},{"./List":1,"./config":2,"./terms":13,"./util":16}],10:[function(require,module,exports){
"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const config_1 = require("./config");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const terms_1 = require("./terms");
const definitions_1 = require("./definitions");
const import_1 = require("./import");
const err = (msg) => { throw new SyntaxError(msg); };
const VarT = (val) => ({ tag: 'VarT', val });
const matchVarT = (val, t) => t.tag === 'VarT' && t.val === val;
const SymbolT = (val) => ({ tag: 'SymbolT', val });
const matchSymbolT = (val, t) => t.tag === 'SymbolT' && t.val === val;
const StringT = (val) => ({ tag: 'StringT', val });
const CharT = (val) => ({ tag: 'CharT', val });
const NumberT = (val) => ({ tag: 'NumberT', val });
const ParenT = (val) => ({ tag: 'ParenT', val });
const HoleT = (val) => ({ tag: 'HoleT', val });
const showToken = (t) => {
    switch (t.tag) {
        case 'SymbolT':
        case 'VarT': return t.val;
        case 'HoleT': return `_${t.val}`;
        case 'StringT': return JSON.stringify(t.val);
        case 'CharT': return `'JSON.stringify(t.val).slice(1, -1)'`;
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
const SYM1 = ['\\', ':', '.', '=', '?'];
const SYM2 = ['->', '<|', '|>', '<<', '>>'];
const KEYWORDS_DEF = ['let', 'type', 'import'];
const KEYWORDS = ['let', 'in', 'if', 'then', 'else'].concat(KEYWORDS_DEF);
const KEYWORDS_TYPE = ['forall'].concat(KEYWORDS_DEF);
const ESCAPES = {
    'n': '\n',
    't': '\t',
    'r': '\r',
};
const START = 0;
const NAME = 1;
const STRING = 2;
const COMMENT = 3;
const NUMBER = 4;
const CHAR = 5;
const tokenize = (sc) => {
    let state = START;
    let r = [];
    let t = '';
    let esc = false;
    let p = [], b = [];
    for (let i = 0, l = sc.length; i <= l; i++) {
        const c = sc[i] || ' ';
        const next = sc[i + 1] || '';
        config_1.log(() => `${i};${c};${next};${state};${showTokens(r)}`);
        if (state === START) {
            if (SYM2.indexOf(c + next) >= 0)
                r.push(SymbolT(c + next)), i++;
            else if (SYM1.indexOf(c) >= 0)
                r.push(SymbolT(c));
            else if (c === ';')
                state = COMMENT;
            else if (c === '"')
                state = STRING;
            else if (c === "'")
                state = CHAR;
            else if (/[\_a-z]/i.test(c))
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
                r.push(t === '_' ? SymbolT(t) : t[0] === '_' ? HoleT(t.slice(1)) : VarT(t));
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
            if (esc) {
                t += ESCAPES[c] || c;
                esc = false;
            }
            else if (c === '\\')
                esc = true;
            else if (c === '"') {
                r.push(StringT(t));
                t = '', state = START;
            }
            else
                t += c;
        }
        else if (state === CHAR) {
            if (esc) {
                t += ESCAPES[c] || c;
                esc = false;
            }
            else if (c === '\\')
                esc = true;
            else if (c === "'") {
                if (t.length === 0 || t.length > 1)
                    return err(`invalid char literal: '${JSON.stringify(t).slice(1, -1)}'`);
                r.push(CharT(t));
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
    if (state === STRING)
        return err(`unclosed string: "${t}`);
    if (state === CHAR)
        return err(`unclosed char: '${t}`);
    if (state !== START)
        return err('invalid tokenize end state');
    if (esc)
        return err(`escape is true after tokenize`);
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
    config_1.log(() => `parseTokenKind ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': return kinds_1.KCon(ts.val);
        case 'ParenT': return parseParensKind(ts.val);
        default: return err(`stuck on ${showToken(ts)} in kind`);
    }
};
const parseParensKind = (ts) => {
    config_1.log(() => `parseParensKind ${showTokens(ts)}`);
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
    config_1.log(() => `parseTokenType ${showToken(ts)}`);
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
        default: return err(`stuck on ${showToken(ts)} in type`);
    }
};
const parseTypePat = (ts) => {
    config_1.log(() => `parseTypePat ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS_TYPE.indexOf(ts.val) >= 0 || isCon(ts.val))
                return err(`stuck on ${ts.val}`);
            return [[ts.val, null]];
        }
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
        default: return err(`stuck on ${showToken(ts)} in forall pattern`);
    }
};
const parseParensType = (ts) => {
    config_1.log(() => `parseParensType ${showTokens(ts)}`);
    if (ts.length === 0)
        return types_1.TCon('Unit');
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
    config_1.log(() => `parseToken ${showToken(ts)}`);
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
            return terms_1.LitStr(ts.val);
        }
        case 'NumberT': {
            const val = ts.val;
            const n = parseInt(val, 10);
            if (isNaN(n) || n < 0 || !isFinite(n))
                return err(`invalid number: ${val}`);
            return terms_1.LitNat(n);
        }
        case 'CharT': {
            return terms_1.LitChar(ts.val);
        }
        case 'HoleT': {
            return terms_1.Hole(ts.val);
        }
    }
};
const parsePat = (ts) => {
    config_1.log(() => `parsePat ${showToken(ts)}`);
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
            if (a.length === 0)
                return [terms_1.PAnn(terms_1.PWildcard, types_1.TCon('Unit'))];
            if (a.length === 1)
                return parsePat(a[0]);
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
        default: return err(`stuck on ${showToken(ts)} in pattern`);
    }
};
const parseParens = (ts) => {
    config_1.log(() => `parseParens ${showTokens(ts)}`);
    if (ts.length === 0)
        return terms_1.Var('unit');
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
    if (matchVarT('if', ts[0])) {
        let i = 1;
        const condts = [];
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no then after if`);
            if (matchVarT('then', c))
                break;
            condts.push(c);
        }
        const truets = [];
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no else after then after if`);
            if (matchVarT('else', c))
                break;
            truets.push(c);
        }
        const cond = parseParens(condts);
        const true_ = parseParens(truets);
        const false_ = parseParens(ts.slice(i));
        return terms_1.If(cond, true_, false_);
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
const parseParensDefs = (ts, map) => __awaiter(this, void 0, void 0, function* () {
    if (ts.length === 0)
        return [];
    if (matchVarT('import', ts[0])) {
        if (ts[1].tag !== 'VarT')
            return err(`invalid import name: ${ts[1].val}`);
        const name = ts[1].val;
        let ds;
        if (!map[name]) {
            map[name] = true;
            const file = yield import_1.loadPromise(name);
            ds = yield parseParensDefs(tokenize(file), map);
        }
        else
            ds = [];
        const rest = yield parseParensDefs(ts.slice(2), map);
        return ds.concat(rest);
    }
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
        const rest = yield parseParensDefs(ts.slice(i - 1), map);
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
        const rest = yield parseParensDefs(ts.slice(i - 1), map);
        return [definitions_1.DLet(name, args, body)].concat(rest);
    }
    return err(`def stuck on ${ts[0].val}`);
});
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
exports.parseDefs = (sc, map) => {
    const ts = tokenize(sc);
    return parseParensDefs(ts, map);
};

},{"./config":2,"./definitions":3,"./import":5,"./kinds":8,"./terms":13,"./types":14}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const util_1 = require("./util");
exports.positivityCheckArg = (c, t, b = true) => {
    if (types_1.isTFun(t)) {
        exports.positivityCheckArg(c, t.left.right, !b);
        exports.positivityCheckArg(c, t.right, b);
        return;
    }
    if (t.tag === 'TApp') {
        exports.positivityCheckArg(c, t.left, b);
        exports.positivityCheckArg(c, t.right, b);
        return;
    }
    if (t.tag === 'TCon' && t.name === c) {
        if (!b)
            return util_1.terr(`positivity check failed: ${c}`);
        return;
    }
};
exports.positivityCheck = (c, t) => {
    const ty = t.tag === 'TForall' ? t.type : t;
    const args = types_1.flattenTFun(ty).slice(0, -1);
    for (let i = 0; i < args.length; i++)
        exports.positivityCheckArg(c, args[i]);
};

},{"./types":14,"./util":16}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const config_1 = require("./config");
const env_1 = require("./env");
const terms_1 = require("./terms");
const types_1 = require("./types");
const inference_1 = require("./inference");
const parser_1 = require("./parser");
const machine_1 = require("./machine");
const definitions_1 = require("./definitions");
const kinds_1 = require("./kinds");
const List_1 = require("./List");
const HELP = `
  commands :help :env :showKinds :debug :time :reset :let :type :import :t :perf :showdefs :showdef :showtype :eval
`.trim();
const cenv = {
    importmap: {},
    tenv: env_1.getInitialEnv(),
    venv: {},
    defs: [],
};
const _part = machine_1.MAbs('x', machine_1.MApp(machine_1.MVar('f'), machine_1.MAbs('v', machine_1.MApp(machine_1.MApp(machine_1.MVar('x'), machine_1.MVar('x')), machine_1.MVar('v')))));
const _ycomb = machine_1.MAbs('f', machine_1.MApp(_part, _part));
const _yval = machine_1.Clos(_ycomb, List_1.Nil);
const setupEnv = () => {
    cenv.tenv.global.unsafeFix = types_1.tforall([['t', kinds_1.kType]], types_1.tfunFrom([types_1.tfunFrom([types_1.TVar('t'), types_1.TVar('t')]), types_1.TVar('t')]));
    cenv.venv.unsafeFix = _yval;
};
setupEnv();
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
const matchTCon = (t, name) => t.tag === 'TCon' && t.name === name;
const matchTApp = (t, name) => t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === name;
const matchTApp2 = (t, name) => t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
    t.left.left.name === name;
const reify = (v, t) => {
    /*
    if (matchTCon(t, 'Nat')) {
      const cl = v as Clos;
      const st = State(mapp(MVar('cataNat'), cl.abs, MAbs('x', MPairC(MAtom('S'), MVar('x'))), MAtom('Z')), cl.env);
      let c = stepsVal(st, cenv.venv);
      let n = 0;
      while (c.tag === 'VPair') {
        n++;
        c = c.right;
      }
      return n;
    }
    */
    if (matchTCon(t, 'Nat')) {
        const cl = v;
        const st = machine_1.State(machine_1.mapp(machine_1.MVar('cataNat'), cl.abs, machine_1.MAtom('Z'), machine_1.MAbs('x', machine_1.MPairC(machine_1.MAtom('T'), machine_1.MVar('x'))), machine_1.MAbs('x', machine_1.MPairC(machine_1.MAtom('TI'), machine_1.MVar('x')))), cl.env);
        let c = machine_1.stepsVal(st, cenv.venv);
        const ar = [];
        while (c.tag === 'VPair') {
            let a = c.left.val;
            ar.push(a === 'T' ? 0 : a === 'TI' ? 1 : 0);
            c = c.right;
        }
        let n = 0;
        for (let i = ar.length - 1; i >= 0; i--)
            n = (n * 2) + ar[i];
        return n;
    }
    if (matchTCon(t, 'Bool')) {
        const cl = v;
        const st = machine_1.State(machine_1.mapp(machine_1.MVar('cond'), cl.abs, machine_1.MAtom('T'), machine_1.MAtom('F')), cl.env);
        let c = machine_1.stepsVal(st, cenv.venv);
        return c.tag === 'VAtom' && c.val === 'T';
    }
    if (matchTCon(t, 'Char')) {
        const n = reify(v, types_1.TCon('Nat'));
        return String.fromCharCode(n);
    }
    if (matchTApp(t, 'List')) {
        const cl = v;
        const st = machine_1.State(machine_1.mapp(machine_1.MVar('cataList'), cl.abs, machine_1.MAtom('Nil'), machine_1.mabs(['h', 'r'], machine_1.MPairC(machine_1.MVar('h'), machine_1.MVar('r')))), cl.env);
        let c = machine_1.stepsVal(st, cenv.venv);
        const r = [];
        while (c.tag === 'VPair') {
            r.push(c.left);
            c = c.right;
        }
        return r;
    }
    if (matchTCon(t, 'Str')) {
        const l = reify(v, types_1.TApp(types_1.TCon('List'), types_1.TCon('Nat')));
        return l.map((v) => String.fromCharCode(reify(v, types_1.TCon('Nat')))).join('');
    }
    if (matchTApp2(t, 'Pair')) {
        const cl = v;
        const st = machine_1.State(machine_1.mapp(cl.abs, machine_1.mabs(['x', 'y'], machine_1.MPairC(machine_1.MVar('x'), machine_1.MVar('y')))), cl.env);
        const c = machine_1.stepsVal(st, cenv.venv);
        return [c.left, c.right];
    }
    if (matchTApp2(t, 'Sum')) {
        const cl = v;
        const st = machine_1.State(machine_1.mapp(cl.abs, machine_1.mabs(['x'], machine_1.MPairC(machine_1.MAtom('L'), machine_1.MVar('x'))), machine_1.mabs(['x'], machine_1.MPairC(machine_1.MAtom('R'), machine_1.MVar('x')))), cl.env);
        const c = machine_1.stepsVal(st, cenv.venv);
        const tag = c.left.val;
        return [tag, c.right];
    }
    if (v.tag === 'Clos')
        return `*closure*`;
    return '?';
};
const _showVal = (v, t) => {
    if (matchTCon(t, 'Unit'))
        return '()';
    if (matchTCon(t, 'Nat'))
        return `${reify(v, t)}`;
    if (matchTCon(t, 'Bool'))
        return `${reify(v, t)}`;
    if (matchTCon(t, 'Char'))
        return `'${JSON.stringify(reify(v, t)).slice(1, -1)}'`;
    if (matchTApp(t, 'List'))
        return `[${reify(v, t).map((x) => _showVal(x, t.right)).join(', ')}]`;
    if (matchTCon(t, 'Str'))
        return JSON.stringify(reify(v, t));
    if (matchTApp2(t, 'Pair')) {
        const [a, b] = reify(v, t);
        const sa = _showVal(a, t.left.right);
        const sb = _showVal(b, t.right);
        return `(${sa}, ${sb})`;
    }
    if (matchTApp2(t, 'Sum')) {
        const [tag, val] = reify(v, t);
        const str = tag === 'L' ?
            _showVal(val, t.left.right) : _showVal(val, t.right);
        return `(${tag} ${str})`;
    }
    if (v.tag === 'Clos') {
        return `*closure*`;
    }
    return '?';
};
exports.init = () => { };
exports.run = (_s, _cb) => {
    try {
        if (_s === ':help' || _s === ':h')
            return _cb(HELP);
        if (_s === ':env' || _s === ':e')
            return _cb(env_1.showEnv(cenv.tenv));
        if (_s === ':showkinds' || _s === ':k') {
            config_1.config.showKinds = !config_1.config.showKinds;
            return _cb(`showKinds: ${config_1.config.showKinds}`);
        }
        if (_s === ':debug' || _s === ':d') {
            config_1.config.debug = !config_1.config.debug;
            return _cb(`debug: ${config_1.config.debug}`);
        }
        if (_s === ':time') {
            config_1.config.time = !config_1.config.time;
            return _cb(`time: ${config_1.config.time}`);
        }
        if (_s === ':reset' || _s === ':r') {
            cenv.importmap = {};
            cenv.tenv = env_1.getInitialEnv();
            cenv.venv = {};
            cenv.defs = [];
            setupEnv();
            return _cb(`environment reset`);
        }
        if (_s === ':showdefs') {
            return _cb(cenv.defs.map(definitions_1.showDef).join('\n'));
        }
        if (_s.startsWith(':showdef ')) {
            const name = _s.slice(8).trim();
            const def = definitions_1.findDef(cenv.defs, name);
            if (!def)
                return _cb(`def not found: ${name}`);
            return _cb(definitions_1.showDef(def));
        }
        if (_s.startsWith(':showtype')) {
            const name = _s.slice(9).trim();
            const def = definitions_1.findDefType(cenv.defs, name);
            if (!def)
                return _cb(`type not found: ${name}`);
            return _cb(definitions_1.showDef(def));
        }
        _s = _s + '\n';
        if (_s.startsWith(':let ') || _s.startsWith(':type ') || _s.startsWith(':import ')) {
            const _rest = _s.slice(1);
            let ptime = Date.now();
            const importmap = Object.assign({}, cenv.importmap);
            parser_1.parseDefs(_rest, importmap).then(_ds => {
                ptime = Date.now() - ptime;
                let itime = Date.now();
                inference_1.inferDefs(cenv.tenv, _ds);
                itime = Date.now() - itime;
                machine_1.resetStepCount();
                let etime = Date.now();
                cenv.defs = cenv.defs.concat(_ds);
                machine_1.runEnv(_ds, cenv.venv);
                const esteps = machine_1.stepCount;
                etime = Date.now() - etime;
                cenv.importmap = importmap;
                return _cb(`defined ${_ds.map(d => d.name).join(' ')}${config_1.config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/total:${ptime + itime + etime}ms(${esteps}steps))` : ''}`);
            }).catch(err => _cb(`${err}`, true));
            return;
        }
        if (_s.startsWith(':perf ')) {
            const rest = _s.slice(6);
            const p = parser_1.parse(rest);
            const res = [];
            for (let i = 0; i < 100; i++) {
                const e = terms_1.App(p, terms_1.LitNat(i));
                const t = inference_1.infer(cenv.tenv, e);
                machine_1.resetStepCount();
                let et = Date.now();
                const v = machine_1.runVal(e, cenv.venv);
                et = Date.now() - et;
                const esteps = machine_1.stepCount;
                machine_1.resetStepCount();
                let vt = Date.now();
                const r = _showVal(v, t);
                vt = Date.now() - vt;
                const vsteps = machine_1.stepCount;
                res.push({ val: i, evaltime: et, reify: vt, evalSteps: esteps, reifySteps: vsteps, total: et + vt, totalSteps: esteps + vsteps });
            }
            return _cb(res.map(({ val, evaltime, reify, evalSteps, reifySteps, total, totalSteps }) => [val, evaltime, evalSteps, reify, reifySteps, total, totalSteps].join(',')).join('\n'));
        }
        if (_s.startsWith(':t ')) {
            const _rest = _s.slice(3);
            let ptime = Date.now();
            const _e = parser_1.parse(_rest);
            ptime = Date.now() - ptime;
            let itime = Date.now();
            const _t = inference_1.infer(cenv.tenv, _e);
            itime = Date.now() - itime;
            return _cb(`${types_1.showTy(_t)}${config_1.config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/total:${ptime + itime}ms)` : ''}`);
        }
        if (_s.startsWith(':eval ')) {
            const rest = _s.slice(5);
            let ptime = Date.now();
            const _e = parser_1.parse(rest);
            ptime = Date.now() - ptime;
            machine_1.resetStepCount();
            let etime = Date.now();
            const _v = machine_1.runVal(_e, cenv.venv);
            etime = Date.now() - etime;
            const esteps = machine_1.stepCount;
            return _cb(`${machine_1.showVal(_v)}${config_1.config.time ? ` (parsing:${ptime}ms/evaluation:${etime}ms(${esteps}steps))` : ''}`);
        }
        let ptime = Date.now();
        const _e = parser_1.parse(_s);
        ptime = Date.now() - ptime;
        config_1.log(() => terms_1.showTerm(_e));
        let itime = Date.now();
        const _t = inference_1.infer(cenv.tenv, _e);
        itime = Date.now() - itime;
        config_1.log(() => types_1.showTy(_t));
        machine_1.resetStepCount();
        let etime = Date.now();
        const _v = machine_1.runVal(_e, cenv.venv);
        etime = Date.now() - etime;
        const esteps = machine_1.stepCount;
        machine_1.resetStepCount();
        let rtime = Date.now();
        const rv = _showVal(_v, _t);
        const rsteps = machine_1.stepCount;
        rtime = Date.now() - rtime;
        return _cb(`${rv} : ${types_1.showTy(_t)}${config_1.config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/reification:${rtime}ms(${rsteps}steps)/total:${ptime + itime + etime + rtime}ms(${esteps + rsteps}steps))` : ''}`);
    }
    catch (_err) {
        config_1.log(() => _err);
        return _cb(`${_err}`, true);
    }
};

},{"./List":1,"./config":2,"./definitions":3,"./env":4,"./inference":6,"./kinds":8,"./machine":9,"./parser":10,"./terms":13,"./types":14}],13:[function(require,module,exports){
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
exports.If = (cond, ifTrue, ifFalse) => ({ tag: 'If', cond, ifTrue, ifFalse });
exports.LitNat = (val) => ({ tag: 'LitNat', val });
exports.LitChar = (val) => ({ tag: 'LitChar', val });
exports.LitStr = (val) => ({ tag: 'LitStr', val });
exports.Hole = (name) => ({ tag: 'Hole', name });
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
    if (t.tag === 'If')
        return `(if ${exports.showTerm(t.cond)} then ${exports.showTerm(t.ifTrue)} else ${exports.showTerm(t.ifFalse)})`;
    if (t.tag === 'LitNat')
        return `${t.val}`;
    if (t.tag === 'LitChar')
        return `'${JSON.stringify(t.val).slice(1, -1)}'`;
    if (t.tag === 'LitStr')
        return JSON.stringify(t.val);
    if (t.tag === 'Hole')
        return `_${t.name}`;
    return util_1.impossible('showTerm');
};

},{"./types":14,"./util":16}],14:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const kinds_1 = require("./kinds");
const config_1 = require("./config");
exports.TForall = (names, kinds, type) => ({ tag: 'TForall', names, kinds, type });
exports.tforall = (ns, type) => {
    if (ns.length === 0)
        return type;
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
    if (t.tag === 'TMeta' && t.type)
        return exports.occursTMeta(x, t.type);
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
    config_1.log(() => `quantify ${exports.showTy(ty)} with [${tms.map(exports.showTy).join(', ')}]`);
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

},{"./config":2,"./kinds":8,"./util":16}],15:[function(require,module,exports){
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
    config_1.log(() => `unify ${types_1.showTy(a)} ~ ${types_1.showTy(b)}`);
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

},{"./config":2,"./kindInference":7,"./kinds":8,"./types":14,"./util":16}],16:[function(require,module,exports){
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

},{"./kinds":8,"./types":14}],17:[function(require,module,exports){
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
// getOutput(':i', addResult);
repl_1.init();
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

},{"./repl":12}],18:[function(require,module,exports){

},{}]},{},[17]);

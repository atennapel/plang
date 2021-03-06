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
exports.mapList = (l, fn) => l.tag === 'Cons' ? exports.Cons(fn(l.head), exports.mapList(l.tail, fn)) : l;
exports.lookupList = (l, i) => {
    while (l.tag === 'Cons') {
        if (i-- === 0)
            return l.head;
        l = l.tail;
    }
    return null;
};

},{}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const terms_1 = require("./terms");
const machine_1 = require("./machine");
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
const tIf = terms_1.Var('if');
const tNil = machine_1.MFVar('nil');
const tCons = machine_1.MFVar('cons');
const tBZ = machine_1.MFVar('BZ');
const tBT = machine_1.MFVar('unsafeBT');
const tBTI = machine_1.MFVar('BTI');
const tMakeInt = machine_1.MFVar('makeInt');
const tRat = machine_1.MFVar('rat');
exports.termToMachine = (term, map = {}, level = 0) => {
    if (term.tag === 'Var') {
        const ix = map[term.name];
        if (typeof ix === 'number')
            return machine_1.MBVar(level - ix - 1);
        return machine_1.MFVar(term.name);
    }
    if (term.tag === 'Abs') {
        const x = exports.patToMachine(term.pat);
        const nmap = {};
        for (let k in map)
            nmap[k] = map[k];
        nmap[x] = level;
        return machine_1.MAbs(exports.termToMachine(term.body, nmap, level + 1));
    }
    if (term.tag === 'App')
        return machine_1.MApp(exports.termToMachine(term.left, map, level), exports.termToMachine(term.right, map, level));
    if (term.tag === 'Let')
        return exports.termToMachine(terms_1.App(terms_1.Abs(term.pat, term.body), term.val), map, level);
    if (term.tag === 'Ann')
        return exports.termToMachine(term.term, map, level);
    if (term.tag === 'If')
        return exports.termToMachine(terms_1.appFrom([tIf, term.cond, terms_1.Abs(terms_1.PWildcard, term.ifTrue), terms_1.Abs(terms_1.PWildcard, term.ifFalse)]), map, level);
    if (term.tag === 'LitNat') {
        let n = BigInt(term.val);
        const r = [];
        while (n > 0n) {
            if (n % 2n === 0n) {
                n /= 2n;
                r.push(tBT);
            }
            else {
                n = (n - 1n) / 2n;
                r.push(tBTI);
            }
        }
        let c = tBZ;
        for (let i = r.length - 1; i >= 0; i--)
            c = machine_1.MApp(r[i], c);
        return c;
    }
    if (term.tag === 'LitInt') {
        let t = exports.termToMachine(terms_1.LitNat(term.val), map, level);
        return term.neg ? machine_1.MApp(machine_1.MApp(tMakeInt, tBZ), t) : machine_1.MApp(machine_1.MApp(tMakeInt, t), tBZ);
    }
    if (term.tag === 'LitRat') {
        const a = exports.termToMachine(terms_1.LitInt(term.val1, term.neg), map, level);
        const b = exports.termToMachine(terms_1.LitNat(term.val2), map, level);
        return machine_1.MApp(machine_1.MApp(tRat, a), b);
    }
    if (term.tag === 'LitChar') {
        const n = term.val.charCodeAt(0);
        return exports.termToMachine(terms_1.LitNat(`${n}`), map, level);
    }
    if (term.tag === 'LitStr') {
        const val = term.val;
        let c = tNil;
        for (let i = val.length - 1; i >= 0; i--)
            c = machine_1.MApp(machine_1.MApp(tCons, exports.termToMachine(terms_1.LitChar(val[i]), map, level)), c);
        return c;
    }
    return util_1.impossible('termToMachine');
};
exports.reduceTerm = (genv, term) => {
    const mterm = exports.termToMachine(term);
    return machine_1.reduce(genv, mterm);
};
exports.reduceDefs = (global, defs) => {
    for (let i = 0, l = defs.length; i < l; i++) {
        const d = defs[i];
        if (d.tag === 'DType') {
            global[d.name] = machine_1.makeClos(machine_1.MAbs(machine_1.MBVar(0)), machine_1.LNil);
        }
        else if (d.tag === 'DLet') {
            const n = d.name;
            const t = terms_1.abs(d.args, d.term);
            const v = exports.reduceTerm(global, t);
            global[n] = v;
        }
    }
};

},{"./machine":10,"./terms":15,"./util":18}],3:[function(require,module,exports){
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

},{"./kinds":9,"./terms":15,"./types":16}],5:[function(require,module,exports){
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
},{"./List":1,"./kinds":9,"./types":16,"./util":18}],6:[function(require,module,exports){
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

},{"fs":20}],7:[function(require,module,exports){
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
exports.tBool = types_1.TCon('Bool');
exports.tNat = types_1.TCon('Nat');
exports.tInt = types_1.TCon('Int');
exports.tRat = types_1.TCon('Rat');
exports.tChar = types_1.TCon('Char');
exports.tStr = types_1.TCon('Str');
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
        if (term.pat.tag === 'PVar') {
            const nenv = env_1.extendVar(env, term.pat.name, ty);
            return tcRho(nenv, term.body, ex);
        }
        else {
            const vars = checkPatSigma(env, term.pat, ty);
            const nenv = env_1.extendVars(env, vars);
            return tcRho(nenv, term.body, ex);
        }
    }
    if (term.tag === 'Ann') {
        const type = kindInference_1.inferKind(env, term.type);
        checkSigma(env, term.term, type);
        return instSigma(env, type, ex);
    }
    if (term.tag === 'If') {
        if (ex.tag === 'Check') {
            checkRho(env, term.cond, exports.tBool);
            tcRho(env, term.ifTrue, ex);
            tcRho(env, term.ifFalse, ex);
            return;
        }
        else if (ex.tag === 'Infer') {
            checkRho(env, term.cond, exports.tBool);
            const t1 = inferRho(env, term.ifTrue);
            const t2 = inferRho(env, term.ifFalse);
            subsCheck(env, t1, t2);
            subsCheck(env, t2, t1);
            ex.type = t1;
            return;
        }
    }
    if (term.tag === 'LitNat') {
        instSigma(env, exports.tNat, ex);
        return;
    }
    if (term.tag === 'LitInt') {
        instSigma(env, exports.tInt, ex);
        return;
    }
    if (term.tag === 'LitRat') {
        instSigma(env, exports.tRat, ex);
        return;
    }
    if (term.tag === 'LitChar') {
        instSigma(env, exports.tChar, ex);
        return;
    }
    if (term.tag === 'LitStr') {
        instSigma(env, exports.tStr, ex);
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
const checkPatSigma = (env, pat, ty) => {
    const rho = instantiate(ty);
    return checkPat(env, pat, rho);
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
        positivity_1.positivityCheck(tname, t);
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

},{"./List":1,"./config":3,"./definitions":4,"./env":5,"./kindInference":8,"./kinds":9,"./positivity":12,"./terms":15,"./types":16,"./unification":17,"./util":18}],8:[function(require,module,exports){
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

},{"./env":5,"./kinds":9,"./types":16,"./util":18}],9:[function(require,module,exports){
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

},{"./util":18}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const config_1 = require("./config");
const List_1 = require("./List");
exports.MFVar = (name) => ({ tag: 'MFVar', name });
exports.MBVar = (ix) => ({ tag: 'MBVar', ix });
exports.MApp = (left, right) => ({ tag: 'MApp', left, right });
exports.mappFrom = (ts) => ts.reduce(exports.MApp);
function mapp(...ts) { return exports.mappFrom(ts); }
exports.mapp = mapp;
;
exports.MAbs = (body) => ({ tag: 'MAbs', body });
exports.MExec = (name, fn, body) => ({ tag: 'MExec', name, fn, body });
exports.MClosExpr = (clos) => ({ tag: 'MClosExpr', clos });
exports.showMTerm = (term) => {
    if (term.tag === 'MFVar')
        return `${term.name}`;
    if (term.tag === 'MBVar')
        return `${term.ix}`;
    if (term.tag === 'MAbs')
        return `(\\${exports.showMTerm(term.body)})`;
    if (term.tag === 'MApp')
        return `(${exports.showMTerm(term.left)} ${exports.showMTerm(term.right)})`;
    if (term.tag === 'MExec')
        return `(exec ${term.name} ${exports.showMTerm(term.body)})`;
    if (term.tag === 'MClosExpr')
        return exports.showMClos(term.clos);
    return util_1.impossible('showMTerm');
};
exports.LNil = { tag: 'LNil' };
exports.LCons = (head, tail) => ({ tag: 'LCons', head, tail });
const extend = (val, env) => exports.LCons(val, env);
const lookup = (env, ix) => {
    while (env.tag === 'LCons') {
        if (ix-- === 0)
            return env.head;
        env = env.tail;
    }
    return null;
};
exports.showLEnv = (list) => {
    const r = [];
    while (list.tag === 'LCons') {
        r.push(list.head ? exports.showMClos(list.head) : '_');
        list = list.tail;
    }
    return `[${r.join(', ')}]`;
};
exports.mapLEnv = (env, fn) => env.tag === 'LCons' ? exports.LCons(env.head && fn(env.head), exports.mapLEnv(env.tail, fn)) :
    env;
exports.mapLEnvToList = (env, fn) => env.tag === 'LCons' ? List_1.Cons(fn(env.head), exports.mapLEnvToList(env.tail, fn)) : List_1.Nil;
exports.MClos = (abs, env) => ({ abs, env });
exports.showMClos = (clos) => `{${exports.showMTerm(clos.abs)}@${exports.showLEnv(clos.env)}}`;
const freeVarsMClos = (clos, fr) => {
    freeVars(clos.abs, fr);
    freeVarsEnv(clos.env, fr);
};
const freeVarsEnv = (env, fr) => {
    let c = env;
    while (c.tag === 'LCons') {
        const cclos = c.head;
        if (cclos) {
            freeVars(cclos.abs, fr);
            freeVarsEnv(cclos.env, fr);
        }
        c = c.tail;
    }
};
const freeVars = (term, fr) => {
    if (term.tag === 'MFVar') {
        fr[term.name] = true;
        return;
    }
    if (term.tag === 'MAbs')
        return freeVars(term.body, fr);
    if (term.tag === 'MApp') {
        freeVars(term.left, fr);
        return freeVars(term.right, fr);
    }
    if (term.tag === 'MExec')
        return freeVars(term.body, fr);
    if (term.tag === 'MClosExpr') {
        freeVarsMClos(term.clos, fr);
        return;
    }
};
const free = (term, fr, under) => {
    if (term.tag === 'MFVar')
        return -1;
    if (term.tag === 'MBVar') {
        const ix = term.ix - under;
        if (ix >= 0)
            fr[ix] = true;
        return ix;
    }
    if (term.tag === 'MAbs') {
        const max = free(term.body, fr, under + 1);
        return max;
    }
    if (term.tag === 'MApp') {
        const a = free(term.left, fr, under);
        const b = free(term.right, fr, under);
        return Math.max(a, b);
    }
    if (term.tag === 'MExec')
        return free(term.body, fr, under);
    if (term.tag === 'MClosExpr')
        return -1;
    return util_1.impossible('free');
};
const makeClosEnv = (fr, max, env, i) => i > max || env.tag === 'LNil' ? exports.LNil :
    exports.LCons(fr[i] ? env.head : null, makeClosEnv(fr, max, env.tail, i + 1));
exports.makeClos = (abs, env) => {
    const fr = {};
    const max = free(abs, fr, 0);
    const nenv = makeClosEnv(fr, max, env, 0);
    return exports.MClos(abs, nenv);
};
exports.MTop = { tag: 'MTop' };
exports.MArg = (term, env, rest) => ({ tag: 'MArg', term, env, rest });
exports.MFun = (body, env, rest) => ({ tag: 'MFun', body, env, rest });
exports.MExecArg = (exec, rest) => ({ tag: 'MExecArg', exec, rest });
exports.showMCont = (cont) => {
    if (cont.tag === 'MTop')
        return 'Top';
    if (cont.tag === 'MArg')
        return `Arg(${exports.showMTerm(cont.term)}, ${exports.showLEnv(cont.env)}):${exports.showMCont(cont.rest)}`;
    if (cont.tag === 'MFun')
        return `Fun(${exports.showMTerm(cont.body)}, ${exports.showLEnv(cont.env)}):${exports.showMCont(cont.rest)}`;
    if (cont.tag === 'MExecArg')
        return `MExecArg(${cont.exec.name}):${exports.showMCont(cont.rest)}`;
    return util_1.impossible('showMCont');
};
exports.MState = (term, env, cont) => ({ term, env, cont });
exports.showMState = (st) => `(${exports.showMTerm(st.term)}, ${exports.showLEnv(st.env)}, ${exports.showMCont(st.cont)})`;
// evaluation
exports.step = (genv, state) => {
    const { term, env, cont } = state;
    if (term.tag === 'MFVar') {
        const v = genv[term.name];
        if (!v)
            return false;
        state.term = v.abs;
        state.env = v.env;
        return true;
    }
    if (term.tag === 'MBVar') {
        const v = lookup(env, term.ix);
        if (!v)
            return false;
        state.term = v.abs;
        state.env = v.env;
        return true;
    }
    if (term.tag === 'MClosExpr') {
        const clos = term.clos;
        state.term = clos.abs;
        state.env = clos.env;
        return true;
    }
    if (term.tag === 'MApp') {
        state.term = term.left;
        state.cont = exports.MArg(term.right, env, cont);
        return true;
    }
    if (term.tag === 'MExec') {
        state.term = term.body;
        state.cont = exports.MExecArg(term, cont);
        return true;
    }
    if (cont.tag === 'MArg') {
        state.term = cont.term;
        state.env = cont.env;
        state.cont = exports.MFun(term.body, env, cont.rest);
        return true;
    }
    if (cont.tag === 'MFun') {
        state.term = cont.body;
        state.env = extend(exports.MClos(term, env), cont.env);
        state.cont = cont.rest;
        return true;
    }
    if (cont.tag === 'MExecArg') {
        state.cont = cont.rest;
        return cont.exec.fn(state, cont.exec);
    }
    return false;
};
exports.stepCount = 0;
exports.resetStepCount = () => { exports.stepCount = 0; };
exports.steps = (genv, state) => {
    config_1.log(() => exports.showMState(state));
    while (exports.step(genv, state)) {
        config_1.log(() => exports.showMState(state));
        exports.stepCount++;
    }
};
exports.initial = (term) => exports.MState(term, exports.LNil, exports.MTop);
exports.reduce = (genv, term) => {
    const st = exports.initial(term);
    exports.steps(genv, st);
    if (st.cont.tag !== 'MTop' || st.term.tag !== 'MAbs')
        throw new Error(`evaluation got stuck: ${exports.showMState(st)}`);
    return exports.makeClos(st.term, st.env);
};
exports.makeClosPackage = (clos, genv) => {
    const fr = {};
    freeVarsMClos(clos, fr);
    for (let k in fr)
        freeVarsMClos(genv[k], fr);
    for (let k in fr)
        fr[k] = genv[k];
    return { clos, env: fr };
};
exports.showClosPackage = (p) => {
    const m = [];
    for (let k in p.env)
        m.push(`${k}: ${exports.showMClos(p.env[k])}`);
    return `(${exports.showMClos(p.clos)}, {${m.join(', ')}})`;
};
exports.flattenMClos = (genv, clos, mem = {}) => {
    const abs = exports.flattenMTerm(genv, clos.abs, mem);
    const env = exports.mapLEnv(clos.env, c => exports.flattenMClos(genv, c, mem));
    return exports.MClos(abs, env);
};
exports.flattenMTerm = (genv, term, mem = {}) => {
    if (term.tag === 'MFVar') {
        if (mem[term.name])
            return mem[term.name];
        if (genv[term.name]) {
            const f = exports.MClosExpr(exports.flattenMClos(genv, genv[term.name], mem));
            mem[term.name] = f;
            return f;
        }
        return term;
    }
    if (term.tag === 'MBVar')
        return term;
    if (term.tag === 'MAbs')
        return exports.MAbs(exports.flattenMTerm(genv, term.body, mem));
    if (term.tag === 'MApp')
        return exports.MApp(exports.flattenMTerm(genv, term.left, mem), exports.flattenMTerm(genv, term.right, mem));
    if (term.tag === 'MExec')
        return exports.flattenMTerm(genv, term.body, mem);
    if (term.tag === 'MClosExpr')
        return exports.MClosExpr(exports.flattenMClos(genv, term.clos, mem));
    return util_1.impossible('flattenMTerm');
};
exports.mclosToLC = (genv, clos, mem = {}) => exports.mtermToLC(genv, exports.mapLEnvToList(clos.env, c => c && exports.mclosToLC(genv, c, mem)), clos.abs, mem);
exports.mtermToLC = (genv, lenv, term, mem = {}) => {
    if (term.tag === 'MFVar') {
        if (mem[term.name])
            return mem[term.name];
        if (genv[term.name]) {
            const f = exports.mclosToLC(genv, genv[term.name], mem);
            mem[term.name] = f;
            return f;
        }
        return term;
    }
    if (term.tag === 'MBVar')
        return List_1.lookupList(lenv, term.ix) || term;
    if (term.tag === 'MAbs')
        return exports.MAbs(exports.mtermToLC(genv, List_1.Cons(null, lenv), term.body, mem));
    if (term.tag === 'MApp')
        return exports.MApp(exports.mtermToLC(genv, lenv, term.left, mem), exports.mtermToLC(genv, lenv, term.right, mem));
    if (term.tag === 'MExec')
        return exports.mtermToLC(genv, lenv, term.body, mem);
    if (term.tag === 'MClosExpr')
        return exports.mclosToLC(genv, term.clos, mem);
    return util_1.impossible('mtermToLC');
};
exports.mclosToBLC = (genv, clos) => exports.mtermToBLC(exports.mclosToLC(genv, clos));
exports.mtermToBLC = (term) => {
    if (term.tag === 'MBVar')
        return `${Array.from({ length: term.ix + 1 }, () => '1').join('')}0`;
    if (term.tag === 'MAbs')
        return `00${exports.mtermToBLC(term.body)}`;
    if (term.tag === 'MApp')
        return `01${exports.mtermToBLC(term.left)}${exports.mtermToBLC(term.right)}`;
    return util_1.impossible('mtermToBLC');
};

},{"./List":1,"./config":3,"./util":18}],11:[function(require,module,exports){
"use strict";
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
            if (!/[a-z0-9\_]/i.test(c)) {
                if (t === '_')
                    r.push(SymbolT(t));
                else if (t[0] === '_') {
                    if (/[0-9]/.test(t[1]))
                        r.push(NumberT(t));
                    else
                        r.push(HoleT(t.slice(1)));
                }
                else
                    r.push(VarT(t));
                t = '', i--, state = START;
            }
            else
                t += c;
        }
        else if (state === NUMBER) {
            if (!/[0-9a-z\_]/i.test(c)) {
                r.push(NumberT(t.toLowerCase()));
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
    if (state !== START && state !== COMMENT)
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
            let val = ts.val;
            let neg = val[0] === '_';
            val = (neg ? val.slice(1) : val).replace(/\_/g, '');
            if (/^[0-9]+$/.test(val)) {
                if (neg)
                    return err(`natural number cannot be negative: ${ts.val}`);
                return terms_1.LitNat(val);
            }
            if (/^[0-9]+i$/.test(val)) {
                const num = val.slice(0, -1);
                return terms_1.LitInt(num, neg);
            }
            if (/^[0-9]+r[0-9]*$/.test(val)) {
                const [a, b] = val.split('r');
                return terms_1.LitRat(a, b || '1', neg);
            }
            return err(`invalid number: ${ts.val}`);
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
        const names = parsePat(ts[1]);
        if (names.length !== 1)
            return err(`too many/few patterns in let`);
        const name = names[0];
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
        return terms_1.Let(name, args.length > 0 ? terms_1.abs(args, body) : body, rest);
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
const parseParensDefs = async (ts, map) => {
    if (ts.length === 0)
        return [];
    if (matchVarT('import', ts[0])) {
        if (ts[1].tag !== 'VarT')
            return err(`invalid import name: ${ts[1].val}`);
        const name = ts[1].val;
        let ds;
        if (!map[name]) {
            map[name] = true;
            const file = await import_1.loadPromise(name);
            ds = await parseParensDefs(tokenize(file), map);
        }
        else
            ds = [];
        const rest = await parseParensDefs(ts.slice(2), map);
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
        const rest = await parseParensDefs(ts.slice(i - 1), map);
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
        const rest = await parseParensDefs(ts.slice(i - 1), map);
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
exports.parseDefs = (sc, map) => {
    const ts = tokenize(sc);
    return parseParensDefs(ts, map);
};

},{"./config":3,"./definitions":4,"./import":6,"./kinds":9,"./terms":15,"./types":16}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const util_1 = require("./util");
const config_1 = require("./config");
exports.positivityCheck = (c, t, b = false) => {
    config_1.log(() => `positivityCheck ${c} ${types_1.showTy(t)} ${b}`);
    if (types_1.isTFun(t)) {
        exports.positivityCheck(c, t.left.right, !b);
        exports.positivityCheck(c, t.right, b);
        return;
    }
    if (t.tag === 'TForall') {
        exports.positivityCheck(c, t.type, b);
        return;
    }
    if (t.tag === 'TApp') {
        exports.positivityCheck(c, t.left, b);
        if (types_1.containsTCon(c, t.right))
            return util_1.terr(`positivity check failed: ${c}`);
        return;
    }
    if (t.tag === 'TCon') {
        if (t.name !== c)
            return;
        if (b)
            return util_1.terr(`positivity check failed: ${c}`);
        return;
    }
};

},{"./config":3,"./types":16,"./util":18}],13:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const machine_1 = require("./machine");
const types_1 = require("./types");
const util_1 = require("./util");
const inference_1 = require("./inference");
const _id = machine_1.MAbs(machine_1.MBVar(0));
const _iterBNat = machine_1.MFVar('iterBNat');
const _if = machine_1.MFVar('if');
const _casePair = machine_1.MFVar('casePair');
const _caseSum = machine_1.MFVar('caseSum');
const _foldr = machine_1.MFVar('foldr');
const matchTCon = (t, name) => t.tag === 'TCon' && t.name === name;
const matchTApp = (t, name) => t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === name;
const matchTApp2 = (t, name) => t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
    t.left.left.name === name;
exports.reify = (v, t, env) => {
    if (matchTCon(t, 'Nat')) {
        let n = 0n;
        const mt = machine_1.mapp(_iterBNat, v.abs, machine_1.MAbs(_id), machine_1.MAbs(machine_1.MApp(machine_1.MApp(machine_1.MBVar(0), _id), machine_1.MExec('twice', () => { n *= 2n; return true; }, _id))), machine_1.MAbs(machine_1.MApp(machine_1.MApp(machine_1.MBVar(0), _id), machine_1.MExec('twicePlusOne', () => { n = (n * 2n) + 1n; return true; }, _id))));
        const st = machine_1.MState(mt, v.env, machine_1.MTop);
        machine_1.steps(env, st);
        return n;
    }
    if (matchTCon(t, 'Int')) {
        const [a, b] = exports.reify(v, types_1.TApp(types_1.TApp(types_1.TCon('Pair'), types_1.TCon('Nat')), types_1.TCon('Nat')), env);
        const na = exports.reify(a, types_1.TCon('Nat'), env);
        const nb = exports.reify(b, types_1.TCon('Nat'), env);
        return na - nb;
    }
    if (matchTCon(t, 'Rat')) {
        const [a, b] = exports.reify(v, types_1.TApp(types_1.TApp(types_1.TCon('Pair'), types_1.TCon('Int')), types_1.TCon('Nat')), env);
        const na = exports.reify(a, types_1.TCon('Int'), env);
        const nb = exports.reify(b, types_1.TCon('Nat'), env);
        return [na, nb];
    }
    if (matchTCon(t, 'Bool')) {
        let b = false;
        const mt = machine_1.mapp(_if, v.abs, machine_1.MAbs(machine_1.MExec('true', () => { b = true; return true; }, _id)), _id);
        const st = machine_1.MState(mt, v.env, machine_1.MTop);
        machine_1.steps(env, st);
        return b;
    }
    if (matchTApp2(t, 'Pair')) {
        let p = [null, null];
        const mt = machine_1.mapp(_casePair, v.abs, machine_1.MAbs(machine_1.MAbs(machine_1.mapp(_id, machine_1.MExec('fst', st => { p[0] = machine_1.makeClos(st.term, st.env); return true; }, machine_1.MBVar(1)), machine_1.MExec('snd', st => { p[1] = machine_1.makeClos(st.term, st.env); return true; }, machine_1.MBVar(0))))));
        const st = machine_1.MState(mt, v.env, machine_1.MTop);
        machine_1.steps(env, st);
        return p;
    }
    if (matchTApp2(t, 'Sum')) {
        let s = [false, null];
        const mt = machine_1.mapp(_caseSum, v.abs, machine_1.MAbs(machine_1.MExec('inl', st => { s[0] = true; s[1] = machine_1.makeClos(st.term, st.env); return true; }, machine_1.MBVar(0))), machine_1.MAbs(machine_1.MExec('inr', st => { s[1] = machine_1.makeClos(st.term, st.env); return true; }, machine_1.MBVar(0))));
        const st = machine_1.MState(mt, v.env, machine_1.MTop);
        machine_1.steps(env, st);
        return s;
    }
    if (matchTApp(t, 'List')) {
        const a = [];
        const mt = machine_1.mapp(_foldr, machine_1.MAbs(machine_1.MAbs(machine_1.MExec('push', st => { a.push(machine_1.makeClos(st.term, st.env)); return true; }, machine_1.MBVar(1)))), _id, v.abs);
        const st = machine_1.MState(mt, v.env, machine_1.MTop);
        machine_1.steps(env, st);
        return a.reverse();
    }
    if (matchTCon(t, 'Str')) {
        const l = exports.reify(v, types_1.TApp(types_1.TCon('List'), types_1.TCon('Nat')), env);
        return l.map((v) => String.fromCharCode(Number(exports.reify(v, types_1.TCon('Nat'), env)))).join('');
    }
    return util_1.impossible('reify');
};
exports.showReifyClos = (v, t, env) => {
    if (matchTCon(t, 'Unit'))
        return '()';
    if (matchTCon(t, 'Bool'))
        return `${exports.reify(v, t, env)}`;
    if (matchTCon(t, 'Nat'))
        return `${exports.reify(v, t, env)}`;
    if (matchTCon(t, 'Int'))
        return `${exports.reify(v, t, env)}`;
    if (matchTCon(t, 'Rat')) {
        const [a, b] = exports.reify(v, t, env);
        return `${a}/${b}`;
    }
    if (matchTCon(t, 'Char'))
        return `'${JSON.stringify(String.fromCharCode(Number(exports.reify(v, inference_1.tNat, env)))).slice(1, -1)}'`;
    if (matchTApp2(t, 'Pair')) {
        const [a, b] = exports.reify(v, t, env);
        const sa = exports.showReifyClos(a, t.left.right, env);
        const sb = exports.showReifyClos(b, t.right, env);
        return `(${sa}, ${sb})`;
    }
    if (matchTApp2(t, 'Sum')) {
        const [a, b] = exports.reify(v, t, env);
        const s = a ? exports.showReifyClos(b, t.left.right, env) : exports.showReifyClos(b, t.right, env);
        return `(${a ? 'L' : 'R'} ${s})`;
    }
    if (matchTApp(t, 'List')) {
        return `[${exports.reify(v, t, env).map((x) => exports.showReifyClos(x, t.right, env)).join(', ')}]`;
    }
    if (matchTCon(t, 'Str'))
        return JSON.stringify(exports.reify(v, t, env));
    if (matchTApp(t, 'IO'))
        return 'IO';
    return machine_1.showMClos(v);
};

},{"./inference":7,"./machine":10,"./types":16,"./util":18}],14:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const config_1 = require("./config");
const env_1 = require("./env");
const terms_1 = require("./terms");
const types_1 = require("./types");
const inference_1 = require("./inference");
const parser_1 = require("./parser");
const definitions_1 = require("./definitions");
const kinds_1 = require("./kinds");
const machine_1 = require("./machine");
const compilerMachine_1 = require("./compilerMachine");
const reification_1 = require("./reification");
const util_1 = require("./util");
const HELP = `
  commands :help :env :showKinds :debug :time :reset :let :type :import :t :perf :showdefs :showdef :showtype :eval :pack :flat :pure :bblc :hblc :ablc :cblc :io
`.trim();
const _caseIO = machine_1.MFVar('caseIO');
const cenv = {
    importmap: {},
    tenv: env_1.getInitialEnv(),
    venv: {},
    defs: [],
};
const _part = machine_1.MAbs(machine_1.MApp(machine_1.MBVar(1), machine_1.MAbs(machine_1.MApp(machine_1.MApp(machine_1.MBVar(1), machine_1.MBVar(1)), machine_1.MBVar(0)))));
const _ycomb = machine_1.MAbs(machine_1.MApp(_part, _part));
const _yval = machine_1.makeClos(_ycomb, machine_1.LNil);
const setupEnv = () => {
    cenv.tenv.global.unsafeFix = types_1.tforall([['t', kinds_1.kType]], types_1.tfunFrom([types_1.tfunFrom([types_1.TVar('t'), types_1.TVar('t')]), types_1.TVar('t')]));
    cenv.venv.unsafeFix = _yval;
};
setupEnv();
const runIO = (_v, _t, _cb, output, input, cont = machine_1.MTop) => {
    const mt = machine_1.mapp(_caseIO, _v.abs, machine_1.MAbs(machine_1.MExec('return', st => {
        const t = machine_1.makeClos(st.term, st.env);
        machine_1.resetStepCount();
        let rtime = Date.now();
        const rv = reification_1.showReifyClos(t, _t, cenv.venv);
        rtime = Date.now() - rtime;
        _cb(`${rv} : ${types_1.showTy(_t)}`);
        return false;
    }, machine_1.MBVar(0))), machine_1.MAbs(machine_1.MExec('getLine', st => {
        input(msg => {
            const clos = machine_1.makeClos(st.term, st.env);
            const t = compilerMachine_1.termToMachine(terms_1.LitStr(msg));
            const io = machine_1.reduce(cenv.venv, machine_1.MApp(machine_1.MClosExpr(clos), t));
            setTimeout(() => runIO(io, _t, _cb, output, input, st.cont), 10);
        });
        return false;
    }, machine_1.MBVar(0))), machine_1.MAbs(machine_1.MAbs(machine_1.mapp(machine_1.MAbs(machine_1.MAbs(machine_1.MBVar(0))), machine_1.MExec('putLine1', st => {
        const str = machine_1.makeClos(st.term, st.env);
        const rstr = reification_1.reify(str, inference_1.tStr, cenv.venv);
        output(rstr);
        setTimeout(() => { machine_1.steps(cenv.venv, st); }, 10);
        return false;
    }, machine_1.MBVar(1)), machine_1.MExec('putLine2', st => {
        setTimeout(() => runIO(machine_1.makeClos(st.term, st.env), _t, _cb, output, input, st.cont), 10);
        return false;
    }, machine_1.MBVar(0))))));
    const st = machine_1.MState(mt, _v.env, cont);
    machine_1.steps(cenv.venv, st);
};
exports.init = () => { };
exports.run = (_s, _cb, output, input) => {
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
                compilerMachine_1.reduceDefs(cenv.venv, _ds);
                const esteps = machine_1.stepCount;
                etime = Date.now() - etime;
                cenv.importmap = importmap;
                return _cb(`defined ${_ds.map(d => d.name).join(' ')}${config_1.config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/total:${ptime + itime + etime}ms(${esteps}steps))` : ''}`);
            }).catch(err => _cb(`${err}`, true));
            return;
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
        if (_s.startsWith(':eval ') || _s.startsWith(':pack ') || _s.startsWith(':flat ') ||
            _s.startsWith(':pure ') || _s.startsWith(':bblc ') || _s.startsWith(':hblc') ||
            _s.startsWith(':ablc ') || _s.startsWith(':cblc')) {
            const mode = _s.startsWith(':eval') ? 0 :
                _s.startsWith(':pack') ? 1 :
                    _s.startsWith(':flat') ? 2 :
                        _s.startsWith(':pure') ? 3 :
                            _s.startsWith(':bblc') ? 4 :
                                _s.startsWith(':hblc') ? 5 :
                                    _s.startsWith(':ablc') ? 6 :
                                        7;
            const rest = _s.slice(5);
            let ptime = Date.now();
            const _e = parser_1.parse(rest);
            ptime = Date.now() - ptime;
            machine_1.resetStepCount();
            let etime = Date.now();
            const _v = compilerMachine_1.reduceTerm(cenv.venv, _e);
            etime = Date.now() - etime;
            const esteps = machine_1.stepCount;
            const show = mode === 0 ? machine_1.showMClos(_v) :
                mode === 1 ? machine_1.showClosPackage(machine_1.makeClosPackage(_v, cenv.venv)) :
                    mode === 2 ? machine_1.showMClos(machine_1.flattenMClos(cenv.venv, _v)) :
                        mode === 3 ? machine_1.showMTerm(machine_1.mclosToLC(cenv.venv, _v)) :
                            mode === 4 ? machine_1.mclosToBLC(cenv.venv, _v) :
                                mode === 5 ? util_1.binToHex(machine_1.mclosToBLC(cenv.venv, _v)) :
                                    mode === 6 ? util_1.binToASCII(machine_1.mclosToBLC(cenv.venv, _v)) :
                                        util_1.binToBase64(machine_1.mclosToBLC(cenv.venv, _v));
            return _cb(`${show}${config_1.config.time ? ` (parsing:${ptime}ms/evaluation:${etime}ms(${esteps}steps))` : ''}`);
        }
        if (_s.startsWith(':io ')) {
            const rest = _s.slice(3);
            const _e = parser_1.parse(rest);
            const _t = inference_1.infer(cenv.tenv, _e);
            if (_t.tag !== 'TApp' || _t.left.tag !== 'TCon' || _t.left.name !== 'IO')
                throw `Expected IO but got: ${types_1.showTy(_t)}`;
            const _v = compilerMachine_1.reduceTerm(cenv.venv, _e);
            runIO(_v, _t.right, _cb, output, input);
            return;
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
        const _v = compilerMachine_1.reduceTerm(cenv.venv, _e);
        etime = Date.now() - etime;
        const esteps = machine_1.stepCount;
        machine_1.resetStepCount();
        let rtime = Date.now();
        const rv = reification_1.showReifyClos(_v, _t, cenv.venv);
        const rsteps = machine_1.stepCount;
        rtime = Date.now() - rtime;
        return _cb(`${rv} : ${types_1.showTy(_t)}${config_1.config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/reification:${rtime}ms(${rsteps}steps)/total:${ptime + itime + etime + rtime}ms(${esteps + rsteps}steps))` : ''}`);
    }
    catch (_err) {
        config_1.log(() => _err);
        return _cb(`${_err}`, true);
    }
};

},{"./compilerMachine":2,"./config":3,"./definitions":4,"./env":5,"./inference":7,"./kinds":9,"./machine":10,"./parser":11,"./reification":13,"./terms":15,"./types":16,"./util":18}],15:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const types_1 = require("./types");
exports.Var = (name) => ({ tag: 'Var', name });
exports.App = (left, right) => ({ tag: 'App', left, right });
exports.appFrom = (ts) => ts.reduce(exports.App);
exports.Abs = (pat, body) => ({ tag: 'Abs', pat, body });
exports.abs = (ns, body) => ns.reduceRight((x, y) => exports.Abs(y, x), body);
exports.Let = (pat, val, body) => ({ tag: 'Let', pat, val, body });
exports.Ann = (term, type) => ({ tag: 'Ann', term, type });
exports.If = (cond, ifTrue, ifFalse) => ({ tag: 'If', cond, ifTrue, ifFalse });
exports.LitNat = (val) => ({ tag: 'LitNat', val });
exports.LitInt = (val, neg) => ({ tag: 'LitInt', val, neg });
exports.LitRat = (val1, val2, neg) => ({ tag: 'LitRat', val1, val2, neg });
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
        return `(let ${exports.showPat(t.pat)} = ${exports.showTerm(t.val)} in ${exports.showTerm(t.body)})`;
    if (t.tag === 'If')
        return `(if ${exports.showTerm(t.cond)} then ${exports.showTerm(t.ifTrue)} else ${exports.showTerm(t.ifFalse)})`;
    if (t.tag === 'LitNat')
        return `${t.val}`;
    if (t.tag === 'LitInt')
        return `${t.neg ? '-' : ''}${t.val}`;
    if (t.tag === 'LitRat')
        return `${t.neg ? '-' : ''}${t.val1}/${t.val2}`;
    if (t.tag === 'LitChar')
        return `'${JSON.stringify(t.val).slice(1, -1)}'`;
    if (t.tag === 'LitStr')
        return JSON.stringify(t.val);
    if (t.tag === 'Hole')
        return `_${t.name}`;
    return util_1.impossible('showTerm');
};

},{"./types":16,"./util":18}],16:[function(require,module,exports){
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
exports.containsTCon = (c, t) => {
    if (t.tag === 'TCon')
        return t.name === c;
    if (t.tag === 'TApp')
        return exports.containsTCon(c, t.left) || exports.containsTCon(c, t.right);
    if (t.tag === 'TForall')
        return exports.containsTCon(c, t.type);
    return false;
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

},{"./config":3,"./kinds":9,"./util":18}],17:[function(require,module,exports){
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

},{"./config":3,"./kindInference":8,"./kinds":9,"./types":16,"./util":18}],18:[function(require,module,exports){
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
const HEX = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
exports.binToHex = (s) => {
    const r = [];
    for (let i = 0, l = s.length; i < l; i += 4) {
        let t = s.slice(i, i + 4);
        if (t.length === 0)
            t = '0000';
        else if (t.length === 1)
            t = `${t}000`;
        else if (t.length === 2)
            t = `${t}00`;
        else if (t.length === 3)
            t = `${t}0`;
        const x = parseInt(t, 2);
        r.push(HEX[x]);
    }
    return r.join('');
};
exports.binToASCII = (s) => {
    const r = [];
    for (let i = 0, l = s.length; i < l; i += 7) {
        let t = s.slice(i, i + 7);
        if (t.length === 0)
            t = '0000000';
        else if (t.length === 1)
            t = `${t}000000`;
        else if (t.length === 2)
            t = `${t}00000`;
        else if (t.length === 3)
            t = `${t}0000`;
        else if (t.length === 4)
            t = `${t}000`;
        else if (t.length === 5)
            t = `${t}00`;
        else if (t.length === 6)
            t = `${t}0`;
        r.push(String.fromCharCode(parseInt(t, 2)));
    }
    return r.join('');
};
exports.binToBase64 = (s) => btoa(exports.binToASCII(s));

},{"./kinds":9,"./types":16}],19:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const repl_1 = require("./repl");
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
            repl_1.run(txt, addResult, msg => addResult(msg), cb => { const x = prompt('prompt:') || ''; cb(x); });
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

},{"./repl":14}],20:[function(require,module,exports){

},{}]},{},[19]);

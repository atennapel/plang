(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
exports.compileName = (name) => {
    const x = names_1.showName(name);
    return keywords.indexOf(x) >= 0 ? `${x}_` : x;
};
exports.compile = (term) => {
    switch (term.tag) {
        case 'Var': return exports.compileName(term.name);
        case 'Abs': return `(${exports.compileName(term.name)} => ${exports.compile(term.body)})`;
        case 'App': return `${exports.compile(term.left)}(${exports.compile(term.right)})`;
        case 'Ann': return exports.compile(term.term);
        case 'Let': return `(${exports.compileName(term.name)} => ${exports.compile(term.body)})(${exports.compile(term.term)})`;
    }
};
exports.compileDef = (def, prefix) => {
    switch (def.tag) {
        case 'DType': {
            const con = `${prefix(exports.compileName(def.name))} = x => x;`;
            const uncon = `${prefix(`un${exports.compileName(def.name)}`)} = x => x;`;
            return `${con}\n${uncon}`;
        }
        case 'DLet':
            return `${prefix(exports.compileName(def.name))} = ${def.args.map(exports.compileName).join(' => ')}${def.args.length > 0 ? ' => ' : ''}${exports.compile(def.term)};`;
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

},{"./names":11}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const elems_1 = require("./elems");
class Context {
    constructor(elems = []) {
        this.elems = elems;
    }
    static of(...es) {
        return new Context(es);
    }
    toString() {
        return `[${this.elems.map(elems_1.showElem).join(', ')}]`;
    }
    clone() {
        return new Context(this.elems.slice(0));
    }
    addAll(es) {
        for (let i = 0, l = es.length; i < l; i++)
            this.elems.push(es[i]);
        return this;
    }
    add(...es) {
        return this.addAll(es);
    }
    append(c) {
        return this.addAll(c.elems);
    }
    indexOf(ty, name) {
        for (let a = this.elems, l = a.length, i = 0; i < l; i++) {
            const c = a[i];
            if (c.tag === ty && names_1.eqName(c.name, name))
                return i;
        }
        return -1;
    }
    contains(ty, name) {
        return this.indexOf(ty, name) >= 0;
    }
    lookup(ty, name) {
        const i = this.indexOf(ty, name);
        if (i < 0)
            return null;
        return this.elems[i];
    }
    pop() {
        return this.elems.pop() || null;
    }
    split(ty, name) {
        const i = this.indexOf(ty, name);
        if (i < 0)
            return [];
        const ret = this.elems.splice(i);
        ret.shift();
        return ret;
    }
    replace(ty, name, es) {
        const right = this.split(ty, name);
        this.addAll(es);
        this.addAll(right);
        return this;
    }
    isComplete() {
        for (let a = this.elems, l = a.length, i = 0; i < l; i++) {
            const c = a[i];
            if (c.tag === 'CTMeta' && !c.type)
                return false;
            if (c.tag === 'CKMeta' && !c.kind)
                return false;
        }
        return true;
    }
    enter(m, ...es) {
        this.add(elems_1.CMarker(m));
        this.addAll(es);
    }
    leave(m) {
        this.split('CMarker', m);
    }
    leaveWithUnsolved(m) {
        const ret = this.split('CMarker', m);
        const ns = [];
        for (let i = 0, l = ret.length; i < l; i++) {
            const c = ret[i];
            if (elems_1.isCTMeta(c) && !c.type)
                ns.push(c.name);
        }
        return ns;
    }
}
exports.Context = Context;

},{"./elems":4,"./names":11}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const terms_1 = require("./terms");
exports.DType = (name, args, type) => ({ tag: 'DType', name, args, type });
exports.isDType = (def) => def.tag === 'DType';
exports.DLet = (name, args, term) => ({ tag: 'DLet', name, args, term });
exports.isDLet = (def) => def.tag === 'DLet';
exports.showDef = (def) => {
    switch (def.tag) {
        case 'DType': {
            const args = def.args.length > 0 ?
                `${def.args.map(([n, k]) => k ? `(${names_1.showName(n)} : ${kinds_1.showKind(k)})` : names_1.showName(n)).join(' ')} ` :
                '';
            return `type ${names_1.showName(def.name)} ${args}= ${types_1.showType(def.type)}`;
        }
        case 'DLet': {
            const args = def.args.length > 0 ? `${def.args.map(names_1.showName).join(' ')} ` : '';
            return `let ${names_1.showName(def.name)} ${args}= ${terms_1.showTerm(def.term)}`;
        }
    }
};

},{"./kinds":10,"./names":11,"./terms":16,"./types":17}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
exports.CKVar = (name) => ({ tag: 'CKVar', name });
exports.isCKVar = (elem) => elem.tag === 'CKVar';
exports.CKMeta = (name, kind = null) => ({ tag: 'CKMeta', name, kind });
exports.isCKMeta = (elem) => elem.tag === 'CKMeta';
exports.CTVar = (name, kind) => ({ tag: 'CTVar', name, kind });
exports.isCTVar = (elem) => elem.tag === 'CTVar';
exports.CTMeta = (name, kind, type = null) => ({ tag: 'CTMeta', name, kind, type });
exports.isCTMeta = (elem) => elem.tag === 'CTMeta';
exports.CVar = (name, type) => ({ tag: 'CVar', name, type });
exports.isCVar = (elem) => elem.tag === 'CVar';
exports.CMarker = (name) => ({ tag: 'CMarker', name });
exports.isCMarker = (elem) => elem.tag === 'CMarker';
exports.showElem = (elem) => {
    switch (elem.tag) {
        case 'CKVar':
            return `kind ${names_1.showName(elem.name)}`;
        case 'CKMeta':
            return `kind ?${names_1.showName(elem.name)}${elem.kind ? ` = ${kinds_1.showKind(elem.kind)}` : ''}`;
        case 'CTVar':
            return `${names_1.showName(elem.name)} :k ${kinds_1.showKind(elem.kind)}`;
        case 'CTMeta':
            return `?${names_1.showName(elem.name)} :k ${kinds_1.showKind(elem.kind)}${elem.type ? ` = ${types_1.showType(elem.type)}` : ''}`;
        case 'CVar':
            return `${names_1.showName(elem.name)} : ${types_1.showType(elem.type)}`;
        case 'CMarker':
            return `|>${names_1.showName(elem.name)}`;
    }
};

},{"./kinds":10,"./names":11,"./types":17}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class InferError extends TypeError {
    constructor(msg) { super(msg); }
}
exports.InferError = InferError;
exports.infererr = (msg) => {
    throw new InferError(msg);
};

},{}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const context_1 = require("./context");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const namestore_1 = require("./namestore");
const initialContext = () => context_1.Context.of(elems_1.CKVar(kinds_1.nType), elems_1.CTVar(types_1.nFun, kinds_1.kfun(kinds_1.kType, kinds_1.kType, kinds_1.kType)));
exports.context = initialContext();
exports.resetContext = () => {
    exports.context = initialContext();
};
const stored = [];
exports.storeContext = (ctx) => {
    const ctx_ = ctx || exports.context;
    stored.push(ctx_.clone());
};
exports.restoreContext = () => {
    exports.context = stored.pop() || exports.context;
};
exports.discardContext = () => {
    stored.pop();
};
exports.namestore = new namestore_1.NameStore();
exports.applyKind = (kind, ctx_) => {
    const ctx = ctx_ || exports.context;
    switch (kind.tag) {
        case 'KVar': return kind;
        case 'KMeta': {
            const t = ctx.lookup('CKMeta', kind.name);
            return t && t.kind ? exports.applyKind(t.kind, ctx) : kind;
        }
        case 'KFun': {
            const left = exports.applyKind(kind.left, ctx);
            const right = exports.applyKind(kind.right, ctx);
            return kind.left === left && kind.right === right ? kind : kinds_1.KFun(left, right);
        }
    }
};
exports.apply = (type, ctx_) => {
    const ctx = ctx_ || exports.context;
    switch (type.tag) {
        case 'TVar': return type;
        case 'TMeta': {
            const t = ctx.lookup('CTMeta', type.name);
            return t && t.type ? exports.apply(t.type, ctx) : type;
        }
        case 'TApp': {
            const left = exports.apply(type.left, ctx);
            const right = exports.apply(type.right, ctx);
            return type.left === left && type.right === right ? type : types_1.TApp(left, right);
        }
        case 'TForall': {
            const body = exports.apply(type.type, ctx);
            const kind = type.kind && exports.applyKind(type.kind, ctx);
            return type.type === body && type.kind === kind ?
                type :
                types_1.TForallK(type.name, kind, body);
        }
    }
};

},{"./context":2,"./elems":4,"./kinds":10,"./namestore":12,"./types":17}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const terms_1 = require("./terms");
const types_1 = require("./types");
const global_1 = require("./global");
const wellformedness_1 = require("./wellformedness");
const error_1 = require("./error");
const names_1 = require("./names");
const subsumption_1 = require("./subsumption");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const kindInference_1 = require("./kindInference");
const unsolvedInType = (unsolved, type, ns = []) => {
    switch (type.tag) {
        case 'TVar': return ns;
        case 'TMeta': {
            const x = type.name;
            if (names_1.nameContains(unsolved, x) && !names_1.nameContains(ns, x))
                ns.push(x);
            return ns;
        }
        case 'TApp': {
            unsolvedInType(unsolved, type.left, ns);
            return unsolvedInType(unsolved, type.right, ns);
        }
        case 'TForall':
            return unsolvedInType(unsolved, type.type, ns);
    }
};
const generalize = (unsolved, type) => {
    const ns = unsolvedInType(unsolved, type);
    const m = names_1.createNameMap();
    for (let i = 0, l = ns.length; i < l; i++) {
        const x = ns[i];
        const y = global_1.namestore.fresh(x);
        names_1.insertNameMap(x, types_1.TVar(y), m);
    }
    let c = types_1.substTMetas(type, m);
    for (let i = ns.length - 1; i >= 0; i--)
        c = types_1.TForall(names_1.getNameMap(ns[i], m).name, c);
    return c;
};
const generalizeFrom = (marker, type) => generalize(global_1.context.leaveWithUnsolved(marker), type);
const typesynth = (term) => {
    if (terms_1.isVar(term)) {
        const x = global_1.context.lookup('CVar', term.name);
        if (!x)
            return error_1.infererr(`undefined var ${names_1.showName(term.name)}`);
        return x.type;
    }
    if (terms_1.isAbs(term)) {
        const x = global_1.namestore.fresh(term.name);
        const a = global_1.namestore.fresh(term.name);
        const b = global_1.namestore.fresh(term.name);
        const ta = types_1.TMeta(a);
        const tb = types_1.TMeta(b);
        global_1.context.enter(x, elems_1.CTMeta(a, kinds_1.kType), elems_1.CTMeta(b, kinds_1.kType), elems_1.CVar(x, ta));
        typecheck(terms_1.openAbs(term, terms_1.Var(x)), tb);
        const ty = global_1.apply(types_1.TFun(ta, tb));
        return generalizeFrom(x, ty);
    }
    if (terms_1.isApp(term)) {
        const left = typesynth(term.left);
        return typeappsynth(global_1.apply(left), term.right);
    }
    if (terms_1.isAnn(term)) {
        const ty = term.type;
        wellformedness_1.wfType(ty);
        kindInference_1.checkKindType(ty);
        typecheck(term.term, ty);
        return ty;
    }
    if (terms_1.isLet(term)) {
        const ty = typesynth(term.term);
        const x = global_1.namestore.fresh(term.name);
        global_1.context.enter(x, elems_1.CVar(x, ty));
        const rty = global_1.apply(typesynth(terms_1.openLet(term, terms_1.Var(x))));
        return generalizeFrom(x, rty);
    }
    return error_1.infererr(`cannot synth: ${terms_1.showTerm(term)}`);
};
const typecheck = (term, type) => {
    if (types_1.isTForall(type)) {
        const x = global_1.namestore.fresh(type.name);
        if (type.kind) {
            global_1.context.enter(x, elems_1.CTVar(x, type.kind));
        }
        else {
            const k = global_1.namestore.fresh(type.name);
            global_1.context.enter(x, elems_1.CKMeta(k), elems_1.CTVar(x, kinds_1.KMeta(k)));
        }
        typecheck(term, types_1.openTForall(type, types_1.TVar(x)));
        global_1.context.leave(x);
        return;
    }
    const f = types_1.matchTFun(type);
    if (terms_1.isAbs(term) && f) {
        const x = global_1.namestore.fresh(term.name);
        global_1.context.enter(x, elems_1.CVar(x, f.left));
        typecheck(terms_1.openAbs(term, terms_1.Var(x)), f.right);
        global_1.context.leave(x);
        return;
    }
    if (terms_1.isLet(term)) {
        const ty = typesynth(term.term);
        const x = global_1.namestore.fresh(term.name);
        global_1.context.enter(x, elems_1.CVar(x, ty));
        typecheck(terms_1.openLet(term, terms_1.Var(x)), type);
        global_1.context.leave(x);
        return;
    }
    const ty = typesynth(term);
    return subsumption_1.subsume(global_1.apply(ty), global_1.apply(type));
};
const typeappsynth = (type, term) => {
    if (types_1.isTForall(type)) {
        const x = global_1.namestore.fresh(type.name);
        if (type.kind) {
            global_1.context.add(elems_1.CTMeta(x, type.kind));
        }
        else {
            const k = global_1.namestore.fresh(type.name);
            global_1.context.add(elems_1.CKMeta(k), elems_1.CTMeta(x, kinds_1.KMeta(k)));
        }
        return typeappsynth(types_1.openTForall(type, types_1.TMeta(x)), term);
    }
    if (types_1.isTMeta(type)) {
        const x = type.name;
        const a = global_1.namestore.fresh(x);
        const b = global_1.namestore.fresh(x);
        const ta = types_1.TMeta(a);
        const tb = types_1.TMeta(b);
        global_1.context.replace('CTMeta', x, [
            elems_1.CTMeta(b, kinds_1.kType),
            elems_1.CTMeta(a, kinds_1.kType),
            elems_1.CTMeta(x, kinds_1.kType, types_1.TFun(ta, tb)),
        ]);
        typecheck(term, ta);
        return tb;
    }
    const f = types_1.matchTFun(type);
    if (f) {
        typecheck(term, f.left);
        return f.right;
    }
    return error_1.infererr(`cannot typeappsynth: ${types_1.showType(type)} @ ${terms_1.showTerm(term)}`);
};
exports.infer = (term) => {
    global_1.namestore.reset();
    wellformedness_1.wfContext();
    const m = global_1.namestore.fresh('m');
    const m2 = global_1.namestore.fresh('m');
    global_1.context.enter(m);
    global_1.context.enter(m2);
    try {
        const ty = generalizeFrom(m2, global_1.apply(typesynth(term)));
        kindInference_1.checkKindType(ty);
        global_1.context.leave(m);
        if (!global_1.context.isComplete())
            return error_1.infererr(`incomplete context: ${global_1.context}`);
        return types_1.simplifyType(ty);
    }
    catch (err) {
        global_1.context.leave(m);
        throw err;
    }
};
exports.inferDef = (def) => {
    // console.log(`inferDef ${showDef(def)}`);
    switch (def.tag) {
        case 'DType': {
            wellformedness_1.wfType(types_1.tforallK(def.args, def.type));
            const tname = def.name;
            const untname = names_1.Name(`un${tname.name}`);
            const targs = def.args;
            if (global_1.context.lookup('CTVar', tname))
                throw new TypeError(`type ${names_1.showName(tname)} is already defined`);
            if (global_1.context.lookup('CVar', tname))
                throw new TypeError(`${names_1.showName(tname)} is already defined`);
            if (global_1.context.lookup('CVar', untname))
                throw new TypeError(`${names_1.showName(untname)} is already defined`);
            global_1.context.add(elems_1.CTVar(tname, kinds_1.kfunFrom(targs.map(([_, k]) => k || kinds_1.kType).concat([kinds_1.kType]))), elems_1.CVar(tname, types_1.tforallK(targs, types_1.tfun(def.type, types_1.tappFrom([types_1.TVar(tname)].concat(targs.map(([n]) => types_1.TVar(n))))))), elems_1.CVar(untname, types_1.tforallK(targs, types_1.tfun(types_1.tappFrom([types_1.TVar(tname)].concat(targs.map(([n]) => types_1.TVar(n)))), def.type))));
            return;
        }
        case 'DLet': {
            const name = def.name;
            if (global_1.context.lookup('CVar', name))
                throw new TypeError(`${names_1.showName(name)} is already defined`);
            const ty = exports.infer(terms_1.abs(def.args, def.term));
            // console.log(`${showName(name)} : ${showType(ty)}`);
            global_1.context.add(elems_1.CVar(name, ty));
            return;
        }
    }
};
exports.inferDefs = (ds) => ds.forEach(exports.inferDef);

},{"./elems":4,"./error":5,"./global":6,"./kindInference":8,"./kinds":10,"./names":11,"./subsumption":15,"./terms":16,"./types":17,"./wellformedness":20}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const kindUnification_1 = require("./kindUnification");
const global_1 = require("./global");
const error_1 = require("./error");
const elems_1 = require("./elems");
exports.inferKind = (type) => {
    // console.log(`inferKind ${showType(type)}`);
    switch (type.tag) {
        case 'TVar': {
            const e = global_1.context.lookup('CTVar', type.name);
            if (!e)
                return error_1.infererr(`undefined tvar ${types_1.showType(type)}`);
            return e.kind;
        }
        case 'TMeta': {
            const e = global_1.context.lookup('CTMeta', type.name);
            if (!e)
                return error_1.infererr(`undefined tmeta ${types_1.showType(type)}`);
            return e.kind;
        }
        case 'TApp': {
            const l = exports.inferKind(type.left);
            const r = exports.inferKind(type.right);
            const kv = global_1.namestore.fresh('k');
            const km = kinds_1.KMeta(kv);
            global_1.context.enter(kv, elems_1.CKMeta(kv));
            kindUnification_1.unifyKinds(l, kinds_1.KFun(r, km));
            const ki = global_1.applyKind(km);
            global_1.context.leave(kv);
            return ki;
        }
        case 'TForall': {
            const t = global_1.namestore.fresh(type.name);
            let k;
            if (type.kind) {
                k = type.kind;
            }
            else {
                const kn = global_1.namestore.fresh(type.name);
                global_1.context.add(elems_1.CKMeta(kn));
                k = kinds_1.KMeta(kn);
            }
            global_1.context.enter(t, elems_1.CTVar(t, k));
            const ki = exports.inferKind(types_1.openTForall(type, types_1.TVar(t)));
            global_1.context.leave(t);
            return global_1.applyKind(ki);
        }
    }
};
exports.checkKindType = (type) => kindUnification_1.unifyKinds(exports.inferKind(type), kinds_1.kType);

},{"./elems":4,"./error":5,"./global":6,"./kindUnification":9,"./kinds":10,"./types":17}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const global_1 = require("./global");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const error_1 = require("./error");
const wellformedness_1 = require("./wellformedness");
const solveKind = (x, kind) => {
    const elem = global_1.context.lookup('CKMeta', x.name);
    if (!elem)
        return error_1.infererr('solve with undefined kmeta');
    const right = global_1.context.split('CKMeta', x.name);
    wellformedness_1.wfKind(kind);
    global_1.context.add(elems_1.CKMeta(x.name, kind));
    global_1.context.addAll(right);
};
const instKind = (x, kind) => {
    global_1.storeContext();
    try {
        solveKind(x, kind);
        global_1.discardContext();
    }
    catch (err) {
        if (!(err instanceof error_1.InferError))
            throw err;
        global_1.restoreContext();
        if (kinds_1.isKMeta(kind))
            return solveKind(kind, x);
        return error_1.infererr(`inst kind failed: ${kinds_1.showKind(x)} := ${kinds_1.showKind(kind)}, ${err}`);
    }
};
exports.unifyKinds = (a_, b_) => {
    // console.log(`unifyKinds ${showKind(a_)} ~ ${showKind(b_)} in ${context}`);
    if (a_ === b_)
        return;
    const a = global_1.applyKind(a_);
    const b = global_1.applyKind(b_);
    if (a === b)
        return;
    if (kinds_1.isKVar(a) && kinds_1.isKVar(b) && names_1.eqName(a.name, b.name))
        return;
    if (kinds_1.isKMeta(a) && kinds_1.isKMeta(b) && names_1.eqName(a.name, b.name))
        return;
    if (kinds_1.isKFun(a) && kinds_1.isKFun(b)) {
        exports.unifyKinds(a.left, b.left);
        return exports.unifyKinds(global_1.applyKind(a.right), global_1.applyKind(b.right));
    }
    if (kinds_1.isKMeta(a)) {
        if (kinds_1.containsKMeta(a.name, b))
            return error_1.infererr(`kind occurs check L failed: ${kinds_1.showKind(a)} in ${kinds_1.showKind(b)}`);
        return instKind(a, b);
    }
    if (kinds_1.isKMeta(b)) {
        if (kinds_1.containsKMeta(b.name, a))
            return error_1.infererr(`kind occurs check R failed: ${kinds_1.showKind(b)} in ${kinds_1.showKind(a)}`);
        return instKind(b, a);
    }
    return error_1.infererr(`kind unify failed: ${kinds_1.showKind(a)} ~ ${kinds_1.showKind(b)}`);
};

},{"./elems":4,"./error":5,"./global":6,"./kinds":10,"./names":11,"./wellformedness":20}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
exports.KVar = (name) => ({ tag: 'KVar', name });
exports.isKVar = (kind) => kind.tag === 'KVar';
exports.KMeta = (name) => ({ tag: 'KMeta', name });
exports.isKMeta = (kind) => kind.tag === 'KMeta';
exports.KFun = (left, right) => ({ tag: 'KFun', left, right });
exports.isKFun = (kind) => kind.tag === 'KFun';
exports.kfunFrom = (ks) => ks.reduceRight((x, y) => exports.KFun(y, x));
exports.kfun = (...ks) => exports.kfunFrom(ks);
exports.nType = names_1.Name('Type');
exports.kType = exports.KVar(exports.nType);
exports.flattenKFun = (kind) => {
    let c = kind;
    const r = [];
    while (exports.isKFun(c)) {
        r.push(c.left);
        c = c.right;
    }
    r.push(c);
    return r;
};
exports.showKind = (kind) => {
    switch (kind.tag) {
        case 'KVar': return names_1.showName(kind.name);
        case 'KMeta': return `?${names_1.showName(kind.name)}`;
        case 'KFun':
            return exports.flattenKFun(kind)
                .map(k => {
                const s = exports.showKind(k);
                return exports.isKFun(k) ? `(${s})` : s;
            })
                .join(' -> ');
    }
};
exports.containsKMeta = (x, kind) => {
    switch (kind.tag) {
        case 'KVar': return false;
        case 'KMeta': return names_1.eqName(x, kind.name);
        case 'KFun': return exports.containsKMeta(x, kind.left) || exports.containsKMeta(x, kind.right);
    }
};

},{"./names":11}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Name = (name) => ({ tag: 'Name', name });
exports.isName = (term) => term.tag === 'Name';
exports.Gen = (name, id) => ({ tag: 'Gen', name, id });
exports.isGen = (term) => term.tag === 'Gen';
exports.showName = (name) => {
    switch (name.tag) {
        case 'Name': return name.name;
        case 'Gen': return `${name.name}\$${name.id}`;
    }
};
exports.simplifyName = (name) => {
    switch (name.tag) {
        case 'Name': return name;
        case 'Gen': return exports.Name(`${name.name}${name.id ? name.id - 1 : ''}`);
    }
};
exports.hashName = (name) => exports.showName(name);
exports.createNameMap = () => new Map();
exports.insertNameMap = (k, v, m) => {
    m.set(exports.hashName(k), v);
    return m;
};
exports.getNameMap = (k, m) => {
    return m.get(exports.hashName(k)) || null;
};
exports.eqName = (a, b) => {
    if (a === b)
        return true;
    if (a.tag === 'Name')
        return b.tag === 'Name' && a.name === b.name;
    if (a.tag === 'Gen')
        return b.tag === 'Gen' && a.name === b.name && a.id === b.id;
    return false;
};
exports.nameContains = (ns, n) => {
    for (let i = 0, l = ns.length; i < l; i++)
        if (exports.eqName(n, ns[i]))
            return true;
    return false;
};

},{}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
class NameStore {
    constructor(map = new Map()) {
        this.map = map;
    }
    toString() {
        const r = [];
        for (let [k, v] of this.map.entries())
            r.push(`${k}: ${v}`);
        return `{${r.join(', ')}}`;
    }
    fresh(name_) {
        const name = typeof name_ === 'string' ? name_ : name_.name;
        const id = this.map.get(name) || 0;
        this.map.set(name, id + 1);
        return names_1.Gen(name, id);
    }
    reset() {
        this.map.clear();
        return this;
    }
}
exports.NameStore = NameStore;

},{"./names":11}],13:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const terms_1 = require("./terms");
const names_1 = require("./names");
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const definitions_1 = require("./definitions");
const err = (msg) => { throw new SyntaxError(msg); };
const VarT = (val) => ({ tag: 'VarT', val });
const matchVarT = (val, t) => t.tag === 'VarT' && t.val === val;
const SymbolT = (val) => ({ tag: 'SymbolT', val });
const matchSymbolT = (val, t) => t.tag === 'SymbolT' && t.val === val;
const ParenT = (val) => ({ tag: 'ParenT', val });
const showToken = (t) => {
    switch (t.tag) {
        case 'SymbolT':
        case 'VarT': return t.val;
        case 'ParenT': return `(${t.val.map(showToken).join(' ')})`;
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
const SYM1 = ['\\', ':', '.', '='];
const SYM2 = ['->'];
const KEYWORDS = ['let', 'in', 'type'];
const KEYWORDS_TYPE = ['forall', 'type', 'let'];
const KEYWORDS_DEF = ['let', 'type'];
const START = 0;
const NAME = 1;
const tokenize = (sc) => {
    let state = START;
    let r = [];
    let t = '';
    let p = [], b = [];
    for (let i = 0, l = sc.length; i <= l; i++) {
        const c = sc[i] || ' ';
        const next = sc[i + 1] || '';
        // console.log(`${i};${c};${next};${state}`, r);
        if (state === START) {
            if (SYM2.indexOf(c + next) >= 0)
                r.push(SymbolT(c + next)), i++;
            else if (SYM1.indexOf(c) >= 0)
                r.push(SymbolT(c));
            else if (/[a-z]/i.test(c))
                t += c, state = NAME;
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
    // console.log(`parseTokenKind ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': return kinds_1.KVar(names_1.Name(ts.val));
        case 'SymbolT': return err(`stuck on ${ts.val}`);
        case 'ParenT': return parseParensKind(ts.val);
    }
};
const parseParensKind = (ts) => {
    // console.log(`parseParensKind ${showTokens(ts)}`);
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
            return err(`empty kind`);
        if (ts.length > 1)
            return err(`kind applications unimplemented`);
        return parseTokenKind(ts[0]);
    }));
};
// types
const parseTokenType = (ts) => {
    // console.log(`parseTokenType ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS_TYPE.indexOf(ts.val) >= 0)
                return err(`stuck on ${ts.val}`);
            return types_1.TVar(names_1.Name(ts.val));
        }
        case 'SymbolT': return err(`stuck on ${ts.val}`);
        case 'ParenT': return parseParensType(ts.val);
    }
};
const parseParensType = (ts) => {
    // console.log(`parseParensType ${showTokens(ts)}`);
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
            if (c.tag === 'ParenT') {
                const parts = splitTokens(c.val, t => matchSymbolT(':', t));
                if (parts.length !== 2)
                    return err(`invalid use of : in forall argument`);
                const as = parts[0].map(t => {
                    if (t.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(t.val) >= 0)
                        return err(`not a valid arg in forall: ${t.val}`);
                    return names_1.Name(t.val);
                });
                const ki = parseParensKind(parts[1]);
                for (let j = 0; j < as.length; j++)
                    args.push([as[j], ki]);
                continue;
            }
            if (c.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(c.val) >= 0)
                return err(`invalid arg to forall: ${c.val}`);
            args.push([names_1.Name(c.val), null]);
        }
        if (args.length === 0)
            return err(`forall without args`);
        const body = parseParensType(ts.slice(i));
        return types_1.tforallK(args, body);
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
    return types_1.tfunFrom(fs.map(ts => types_1.tappFrom(ts.map(parseTokenType))));
};
// terms
const parseToken = (ts) => {
    // console.log(`parseToken ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS.indexOf(ts.val) >= 0)
                return err(`stuck on ${ts.val}`);
            return terms_1.Var(names_1.Name(ts.val));
        }
        case 'SymbolT': return err(`stuck on ${ts.val}`);
        case 'ParenT': return parseParens(ts.val);
    }
};
const parseParens = (ts) => {
    // console.log(`parseParens ${showTokens(ts)}`);
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
            if (c.tag !== 'VarT' || KEYWORDS.indexOf(c.tag) >= 0)
                return err(`invalid arg: ${c.val}`);
            args.push(c.val);
        }
        if (args.length === 0)
            return err(`\\ without args`);
        const body = parseParens(ts.slice(i));
        return terms_1.abs(args.map(names_1.Name), body);
    }
    if (matchVarT('let', ts[0])) {
        const args = [];
        let i = 1;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no = after let`);
            if (matchSymbolT('=', c))
                break;
            if (c.tag !== 'VarT' || KEYWORDS.indexOf(c.tag) >= 0)
                return err(`invalid arg: ${c.val}`);
            args.push(names_1.Name(c.val));
        }
        if (args.length === 0)
            return err(`let without name`);
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
        return terms_1.Let(args[0], args.length > 1 ? terms_1.abs(args.slice(1), body) : body, rest);
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
        if (ts[1].tag !== 'VarT')
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
            if (c.tag === 'ParenT') {
                const parts = splitTokens(c.val, t => matchSymbolT(':', t));
                if (parts.length !== 2)
                    return err(`invalid use of : in type argument`);
                const as = parts[0].map(t => {
                    if (t.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(t.val) >= 0)
                        return err(`not a valid arg in type: ${t.val}`);
                    return names_1.Name(t.val);
                });
                const ki = parseParensKind(parts[1]);
                for (let j = 0; j < as.length; j++)
                    args.push([as[j], ki]);
                continue;
            }
            if (c.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(c.tag) >= 0)
                return err(`invalid arg: ${c.val}`);
            args.push([names_1.Name(c.val), null]);
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
        return [definitions_1.DType(names_1.Name(tname), args, body)].concat(rest);
    }
    if (matchVarT('let', ts[0])) {
        const args = [];
        let i = 1;
        while (true) {
            const c = ts[i++];
            if (!c)
                return err(`no = after let def`);
            if (matchSymbolT('=', c))
                break;
            if (c.tag !== 'VarT' || KEYWORDS.indexOf(c.tag) >= 0)
                return err(`invalid arg: ${c.val}`);
            args.push(names_1.Name(c.val));
        }
        if (args.length === 0)
            return err(`let def without name`);
        const bodyts = [];
        while (true) {
            const c = ts[i++];
            if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0))
                break;
            bodyts.push(c);
        }
        const body = parseParens(bodyts);
        const rest = parseParensDefs(ts.slice(i - 1));
        return [definitions_1.DLet(args[0], args.slice(1), body)].concat(rest);
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

},{"./definitions":3,"./kinds":10,"./names":11,"./terms":16,"./types":17}],14:[function(require,module,exports){
(function (global){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const compiler_1 = require("./compiler");
const inference_1 = require("./inference");
const parser_1 = require("./parser");
const global_1 = require("./global");
const names_1 = require("./names");
const _show = (x) => {
    if (typeof x === 'function')
        return '[Fn]';
    if (typeof x === 'string')
        return JSON.stringify(x);
    if (Array.isArray(x))
        return `[${x.map(_show).join(', ')}]`;
    if (typeof x === 'object' && typeof x._tag === 'string') {
        if (x._tag === 'Pair')
            return `(Pair ${_show(x.val[0])} ${_show(x.val[1])})`;
        return x.val === null ? x._tag : `(${x._tag} ${_show(x.val)})`;
    }
    return '' + x;
};
const _prelude = "type Void = forall t. t\r\n\r\ntype Unit = forall t. t -> t\r\nlet unit = Unit \\x -> x\r\n\r\ntype Pair a b = forall r. (a -> b -> r) -> r\r\nlet pair a b = Pair \\f -> f a b\r\nlet fst p = unPair p \\x y -> x\r\nlet snd p = unPair p \\x y -> y\r\n\r\ntype Sum a b = forall r. (a -> r) -> (b -> r) -> r\r\nlet inl x = Sum \\f g -> f x\r\nlet inr x = Sum \\f g -> g x\r\n\r\ntype Bool = forall r. r -> r -> r\r\nlet true = Bool \\a b -> a\r\nlet false = Bool \\a b -> b\r\nlet cond c a b = unBool c a b\r\nlet if c a b = cond c a b unit\r\n";
const _env = typeof global === 'undefined' ? 'window' : 'global';
exports.run = (_s, _cb) => {
    if (_s === ':c' || _s === ':ctx' || _s === ':context')
        return _cb(`${global_1.context}`);
    if (_s.startsWith(':def ')) {
        try {
            const _rest = _s.slice(4).trim();
            const _ds = parser_1.parseDefs(_rest);
            inference_1.inferDefs(_ds);
            const _c = compiler_1.compileDefs(_ds, n => `${_env}['${n}']`);
            eval(`(() => {${_c}})()`);
            return _cb(`defined ${_ds.map(d => names_1.showName(d.name)).join(' ')}`);
        }
        catch (err) {
            return _cb(`${err}`, true);
        }
    }
    if (_s === ':p' || _s === ':prelude') {
        try {
            const _ds = parser_1.parseDefs(_prelude);
            inference_1.inferDefs(_ds);
            const _c = compiler_1.compileDefs(_ds, n => `${_env}['${n}']`);
            eval(`(() => {${_c}})()`);
            return _cb(`defined ${_ds.map(d => names_1.showName(d.name)).join(' ')}`);
        }
        catch (err) {
            return _cb(`${err}`, true);
        }
    }
    try {
        const _e = parser_1.parse(_s);
        // console.log(showTerm(_e));
        const _t = inference_1.infer(_e);
        // console.log(showType(_t));
        const _c = compiler_1.compile(_e);
        // console.log(_c);
        const _v = eval(_c);
        // console.log(_v);
        return _cb(`${_show(_v)} : ${types_1.showType(_t)}`);
    }
    catch (err) {
        return _cb('' + err, true);
    }
};

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./compiler":1,"./global":6,"./inference":7,"./names":11,"./parser":13,"./types":17}],15:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const names_1 = require("./names");
const global_1 = require("./global");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const wellformedness_1 = require("./wellformedness");
const error_1 = require("./error");
const unification_1 = require("./unification");
const kindUnification_1 = require("./kindUnification");
const kindInference_1 = require("./kindInference");
exports.solve = (x, type) => {
    if (!types_1.isMono(type))
        return error_1.infererr('solve with polytype');
    const elem = global_1.context.lookup('CTMeta', x.name);
    if (!elem)
        return error_1.infererr('solve with undefined tmeta');
    const right = global_1.context.split('CTMeta', x.name);
    wellformedness_1.wfType(type);
    global_1.context.add(elems_1.CTMeta(x.name, elem.kind, type));
    global_1.context.addAll(right);
};
// x := List y
const instL = (x, type) => {
    global_1.storeContext();
    try {
        exports.solve(x, type);
        global_1.discardContext();
    }
    catch (err) {
        if (!(err instanceof error_1.InferError))
            throw err;
        global_1.restoreContext();
        if (types_1.isTMeta(type))
            return exports.solve(type, x);
        const f = types_1.matchTFun(type);
        if (f) {
            const y = x.name;
            const a = global_1.namestore.fresh(y);
            const b = global_1.namestore.fresh(y);
            const ta = types_1.TMeta(a);
            const tb = types_1.TMeta(b);
            global_1.context.replace('CTMeta', y, [
                elems_1.CTMeta(b, kinds_1.kType),
                elems_1.CTMeta(a, kinds_1.kType),
                elems_1.CTMeta(y, kinds_1.kType, types_1.TFun(ta, tb)),
            ]);
            instR(f.left, ta);
            instL(tb, global_1.apply(f.right));
            return;
        }
        if (types_1.isTApp(type))
            return unification_1.inst(x, type);
        if (types_1.isTForall(type)) {
            const y = global_1.namestore.fresh(type.name);
            if (type.kind) {
                global_1.context.enter(y, elems_1.CTVar(y, type.kind));
            }
            else {
                const k = global_1.namestore.fresh(type.name);
                global_1.context.enter(y, elems_1.CKMeta(k), elems_1.CTVar(y, kinds_1.KMeta(k)));
            }
            instL(x, types_1.openTForall(type, types_1.TVar(y)));
            global_1.context.leave(y);
            return;
        }
        return error_1.infererr(`instL failed: ${types_1.showType(x)} := ${types_1.showType(type)}, ${err}`);
    }
};
const instR = (type, x) => {
    global_1.storeContext();
    try {
        exports.solve(x, type);
        global_1.discardContext();
    }
    catch (err) {
        if (!(err instanceof error_1.InferError))
            throw err;
        global_1.restoreContext();
        if (types_1.isTMeta(type))
            return exports.solve(type, x);
        const f = types_1.matchTFun(type);
        if (f) {
            const y = x.name;
            const a = global_1.namestore.fresh(y);
            const b = global_1.namestore.fresh(y);
            const ta = types_1.TMeta(a);
            const tb = types_1.TMeta(b);
            global_1.context.replace('CTMeta', y, [
                elems_1.CTMeta(b, kinds_1.kType),
                elems_1.CTMeta(a, kinds_1.kType),
                elems_1.CTMeta(y, kinds_1.kType, types_1.TFun(ta, tb)),
            ]);
            instL(ta, f.left);
            instR(global_1.apply(f.right), tb);
            return;
        }
        if (types_1.isTApp(type))
            return unification_1.inst(x, type);
        if (types_1.isTForall(type)) {
            const y = global_1.namestore.fresh(type.name);
            if (type.kind) {
                global_1.context.enter(y, elems_1.CTMeta(y, type.kind));
            }
            else {
                const k = global_1.namestore.fresh(type.name);
                global_1.context.enter(y, elems_1.CKMeta(k), elems_1.CTMeta(y, kinds_1.KMeta(k)));
            }
            instR(types_1.openTForall(type, types_1.TMeta(y)), x);
            global_1.context.leave(y);
            return;
        }
        return error_1.infererr(`instR failed: ${types_1.showType(x)} := ${types_1.showType(type)}, ${err}`);
    }
};
exports.subsume = (a_, b_) => {
    // console.log(`subsume ${showType(a_)} <: ${showType(b_)} in ${context}`);
    if (a_ === b_)
        return;
    const a = global_1.apply(a_);
    const b = global_1.apply(b_);
    if (a === b)
        return;
    kindUnification_1.unifyKinds(kindInference_1.inferKind(a), kindInference_1.inferKind(b));
    if (types_1.isTVar(a) && types_1.isTVar(b) && names_1.eqName(a.name, b.name))
        return;
    if (types_1.isTMeta(a) && types_1.isTMeta(b) && names_1.eqName(a.name, b.name))
        return;
    const fa = types_1.matchTFun(a);
    const fb = types_1.matchTFun(b);
    if (fa && fb) {
        exports.subsume(fb.left, fa.left);
        return exports.subsume(global_1.apply(fa.right), global_1.apply(fb.right));
    }
    if (types_1.isTApp(a) && types_1.isTApp(b))
        return unification_1.unify(a, b);
    if (types_1.isTForall(a)) {
        const t = global_1.namestore.fresh(a.name);
        if (a.kind) {
            global_1.context.enter(t, elems_1.CTMeta(t, a.kind));
        }
        else {
            const k = global_1.namestore.fresh(a.name);
            global_1.context.enter(t, elems_1.CKMeta(k), elems_1.CTMeta(t, kinds_1.KMeta(k)));
        }
        exports.subsume(types_1.openTForall(a, types_1.TMeta(t)), b);
        global_1.context.leave(t);
        return;
    }
    if (types_1.isTForall(b)) {
        const t = global_1.namestore.fresh(b.name);
        if (b.kind) {
            global_1.context.enter(t, elems_1.CTVar(t, b.kind));
        }
        else {
            const k = global_1.namestore.fresh(b.name);
            global_1.context.enter(t, elems_1.CKMeta(k), elems_1.CTVar(t, kinds_1.KMeta(k)));
        }
        exports.subsume(a, types_1.openTForall(b, types_1.TVar(t)));
        global_1.context.leave(t);
        return;
    }
    if (types_1.isTMeta(a)) {
        if (types_1.containsTMeta(a.name, b))
            return error_1.infererr(`occurs check L failed: ${types_1.showType(a)} in ${types_1.showType(b)}`);
        return instL(a, b);
    }
    if (types_1.isTMeta(b)) {
        if (types_1.containsTMeta(b.name, a))
            return error_1.infererr(`occurs check R failed: ${types_1.showType(b)} in ${types_1.showType(a)}`);
        return instR(a, b);
    }
    return error_1.infererr(`subsume failed: ${types_1.showType(a)} <: ${types_1.showType(b)}`);
};

},{"./elems":4,"./error":5,"./global":6,"./kindInference":8,"./kindUnification":9,"./kinds":10,"./names":11,"./types":17,"./unification":18,"./wellformedness":20}],16:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const types_1 = require("./types");
exports.Var = (name) => ({ tag: 'Var', name });
exports.isVar = (term) => term.tag === 'Var';
exports.Abs = (name, body) => ({ tag: 'Abs', name, body });
exports.isAbs = (term) => term.tag === 'Abs';
exports.abs = (ns, body) => ns.reduceRight((t, n) => exports.Abs(n, t), body);
exports.App = (left, right) => ({ tag: 'App', left, right });
exports.isApp = (term) => term.tag === 'App';
exports.appFrom = (ts) => ts.reduce(exports.App);
exports.app = (...ts) => exports.appFrom(ts);
exports.Ann = (term, type) => ({ tag: 'Ann', term, type });
exports.isAnn = (term) => term.tag === 'Ann';
exports.Let = (name, term, body) => ({ tag: 'Let', name, term, body });
exports.isLet = (term) => term.tag === 'Let';
exports.flattenApp = (type) => {
    let c = type;
    const r = [];
    while (exports.isApp(c)) {
        r.push(c.right);
        c = c.left;
    }
    r.push(c);
    return r.reverse();
};
exports.flattenAbs = (type) => {
    let c = type;
    const args = [];
    while (exports.isAbs(c)) {
        args.push(c.name);
        c = c.body;
    }
    return { args, body: c };
};
exports.showTerm = (term) => {
    switch (term.tag) {
        case 'Var': return names_1.showName(term.name);
        case 'App':
            return exports.flattenApp(term)
                .map(t => {
                const s = exports.showTerm(t);
                return exports.isApp(t) || exports.isAbs(t) || exports.isAnn(t) ? `(${s})` : s;
            })
                .join(' ');
        case 'Abs': {
            const f = exports.flattenAbs(term);
            const args = f.args.map(names_1.showName).join(' ');
            return `\\${args} -> ${exports.showTerm(f.body)}`;
        }
        case 'Ann': return `${exports.showTerm(term.term)} : ${types_1.showType(term.type)}`;
        case 'Let':
            return `(let ${names_1.showName(term.name)} = ${exports.showTerm(term.term)} in ${exports.showTerm(term.body)})`;
    }
};
const substVar = (x, s, term) => {
    switch (term.tag) {
        case 'Var': return names_1.eqName(x, term.name) ? s : term;
        case 'Abs': {
            if (names_1.eqName(x, term.name))
                return term;
            const body = substVar(x, s, term.body);
            return term.body === body ? term : exports.Abs(term.name, body);
        }
        case 'App': {
            const left = substVar(x, s, term.left);
            const right = substVar(x, s, term.right);
            return term.left === left && term.right === right ? term : exports.App(left, right);
        }
        case 'Ann': {
            const body = substVar(x, s, term.term);
            return term.term === body ? term : exports.Ann(body, term.type);
        }
        case 'Let': {
            const val = substVar(x, s, term.term);
            const body = names_1.eqName(x, term.name) ? term.body : substVar(x, s, term.body);
            return term.term === val && term.body === body ? term : exports.Let(term.name, val, body);
        }
    }
};
exports.openAbs = (a, s) => substVar(a.name, s, a.body);
exports.openLet = (a, s) => substVar(a.name, s, a.body);

},{"./names":11,"./types":17}],17:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const kinds_1 = require("./kinds");
const namestore_1 = require("./namestore");
exports.TVar = (name) => ({ tag: 'TVar', name });
exports.isTVar = (type) => type.tag === 'TVar';
exports.TMeta = (name) => ({ tag: 'TMeta', name });
exports.isTMeta = (type) => type.tag === 'TMeta';
exports.TApp = (left, right) => ({ tag: 'TApp', left, right });
exports.isTApp = (type) => type.tag === 'TApp';
exports.tappFrom = (ts) => ts.reduce(exports.TApp);
exports.tapp = (...ts) => exports.tappFrom(ts);
exports.TForall = (name, type) => ({ tag: 'TForall', name, kind: null, type });
exports.TForallK = (name, kind, type) => ({ tag: 'TForall', name, kind, type });
exports.isTForall = (type) => type.tag === 'TForall';
exports.tforall = (ns, type) => ns.reduceRight((t, n) => exports.TForall(n, t), type);
exports.tforallK = (ns, type) => ns.reduceRight((t, [n, k]) => exports.TForallK(n, k, t), type);
exports.nFun = names_1.Name('->');
exports.tFun = exports.TVar(exports.nFun);
exports.TFun = (left, right) => exports.TApp(exports.TApp(exports.tFun, left), right);
exports.tfunFrom = (ts) => ts.reduceRight((x, y) => exports.TFun(y, x));
exports.tfun = (...ts) => exports.tfunFrom(ts);
exports.isTFun = (type) => exports.isTApp(type) && exports.isTApp(type.left) && exports.isTVar(type.left.left) && names_1.eqName(type.left.left.name, exports.nFun);
exports.matchTFun = (type) => exports.isTFun(type) ? { left: type.left.right, right: type.right } : null;
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
exports.flattenTForall = (type) => {
    let c = type;
    const args = [];
    while (exports.isTForall(c)) {
        args.push([c.name, c.kind || null]);
        c = c.type;
    }
    return { args, type: c };
};
exports.flattenTFun = (type) => {
    let c = type;
    const r = [];
    let f = exports.matchTFun(c);
    while (f) {
        r.push(f.left);
        c = f.right;
        f = exports.matchTFun(c);
    }
    r.push(c);
    return r;
};
exports.showType = (type) => {
    switch (type.tag) {
        case 'TVar': return names_1.showName(type.name);
        case 'TMeta': return `?${names_1.showName(type.name)}`;
        case 'TApp': {
            if (exports.isTFun(type))
                return exports.flattenTFun(type)
                    .map(t => {
                    const s = exports.showType(t);
                    return exports.isTFun(t) || exports.isTForall(t) ? `(${s})` : s;
                })
                    .join(' -> ');
            return exports.flattenTApp(type)
                .map(t => {
                const s = exports.showType(t);
                return exports.isTApp(t) || exports.isTForall(t) ? `(${s})` : s;
            })
                .join(' ');
        }
        case 'TForall': {
            const f = exports.flattenTForall(type);
            const args = f.args
                .map(([n, k]) => k ? `(${names_1.showName(n)} : ${kinds_1.showKind(k)})` : names_1.showName(n))
                .join(' ');
            return `forall ${args}. ${exports.showType(f.type)}`;
        }
    }
};
exports.substTVar = (x, s, type) => {
    switch (type.tag) {
        case 'TVar': return names_1.eqName(x, type.name) ? s : type;
        case 'TMeta': return type;
        case 'TApp': {
            const left = exports.substTVar(x, s, type.left);
            const right = exports.substTVar(x, s, type.right);
            return type.left === left && type.right === right ? type : exports.TApp(left, right);
        }
        case 'TForall': {
            if (names_1.eqName(x, type.name))
                return type;
            const body = exports.substTVar(x, s, type.type);
            return type.type === body ? type : exports.TForallK(type.name, type.kind, body);
        }
    }
};
exports.openTForall = (forall, s) => exports.substTVar(forall.name, s, forall.type);
exports.containsTMeta = (x, type) => {
    switch (type.tag) {
        case 'TVar': return false;
        case 'TMeta': return names_1.eqName(x, type.name);
        case 'TApp': return exports.containsTMeta(x, type.left) || exports.containsTMeta(x, type.right);
        case 'TForall': return exports.containsTMeta(x, type.type);
    }
};
exports.substTMetas = (type, m) => {
    switch (type.tag) {
        case 'TVar': return type;
        case 'TMeta': return names_1.getNameMap(type.name, m) || type;
        case 'TApp': {
            const left = exports.substTMetas(type.left, m);
            const right = exports.substTMetas(type.right, m);
            return type.left === left && type.right === right ? type : exports.TApp(left, right);
        }
        case 'TForall': {
            const body = exports.substTMetas(type.type, m);
            return type.type === body ? type : exports.TForallK(type.name, type.kind, body);
        }
    }
};
exports.isMono = (type) => {
    switch (type.tag) {
        case 'TVar': return true;
        case 'TMeta': return true;
        case 'TApp': return exports.isMono(type.left) && exports.isMono(type.right);
        case 'TForall': return false;
    }
};
exports.simplifyType = (type, ns = new namestore_1.NameStore()) => {
    switch (type.tag) {
        case 'TVar':
        case 'TMeta': return type;
        case 'TApp': {
            const left = exports.simplifyType(type.left, ns);
            const right = exports.simplifyType(type.right, ns);
            return type.left === left && type.right === right ? type : exports.TApp(left, right);
        }
        case 'TForall': {
            const x = names_1.simplifyName(ns.fresh(type.name));
            const body = exports.simplifyType(type.type, ns);
            return exports.TForallK(x, type.kind, exports.substTVar(type.name, exports.TVar(x), body));
        }
    }
};

},{"./kinds":10,"./names":11,"./namestore":12}],18:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const names_1 = require("./names");
const global_1 = require("./global");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const error_1 = require("./error");
const subsumption_1 = require("./subsumption");
const kindInference_1 = require("./kindInference");
const kindUnification_1 = require("./kindUnification");
exports.inst = (x, type) => {
    global_1.storeContext();
    try {
        subsumption_1.solve(x, type);
        global_1.discardContext();
    }
    catch (err) {
        if (!(err instanceof error_1.InferError))
            throw err;
        global_1.restoreContext();
        if (types_1.isTMeta(type))
            return subsumption_1.solve(type, x);
        if (types_1.isTApp(type)) {
            const y = x.name;
            const a = global_1.namestore.fresh(y);
            const b = global_1.namestore.fresh(y);
            const ta = types_1.TMeta(a);
            const tb = types_1.TMeta(b);
            global_1.context.replace('CTMeta', y, [
                elems_1.CTMeta(b, kinds_1.kType),
                elems_1.CTMeta(a, kinds_1.kType),
                elems_1.CTMeta(y, kinds_1.kType, types_1.TApp(ta, tb)),
            ]);
            exports.inst(ta, type.left);
            exports.inst(tb, global_1.apply(type.right));
            return;
        }
        if (types_1.isTForall(type)) {
            const y = global_1.namestore.fresh(type.name);
            if (type.kind) {
                global_1.context.enter(y, elems_1.CTVar(y, type.kind));
            }
            else {
                const k = global_1.namestore.fresh(type.name);
                global_1.context.enter(y, elems_1.CKMeta(k), elems_1.CTVar(y, kinds_1.KMeta(k)));
            }
            exports.inst(x, types_1.openTForall(type, types_1.TVar(y)));
            global_1.context.leave(y);
            return;
        }
        return error_1.infererr(`inst failed: ${types_1.showType(x)} := ${types_1.showType(type)}, ${err}`);
    }
};
exports.unify = (a_, b_) => {
    // console.log(`unify ${showType(a_)} ~ ${showType(b_)} in ${context}`);
    if (a_ === b_)
        return;
    const a = global_1.apply(a_);
    const b = global_1.apply(b_);
    if (a === b)
        return;
    kindUnification_1.unifyKinds(kindInference_1.inferKind(a), kindInference_1.inferKind(b));
    if (a === b)
        return;
    if (types_1.isTVar(a) && types_1.isTVar(b) && names_1.eqName(a.name, b.name))
        return;
    if (types_1.isTMeta(a) && types_1.isTMeta(b) && names_1.eqName(a.name, b.name))
        return;
    if (types_1.isTApp(a) && types_1.isTApp(b)) {
        exports.unify(a.left, b.left);
        return exports.unify(global_1.apply(a.right), global_1.apply(b.right));
    }
    if (types_1.isTForall(a) && types_1.isTForall(b)) {
        const t = global_1.namestore.fresh(a.name);
        const k = global_1.namestore.fresh(a.name);
        global_1.context.enter(t, elems_1.CKMeta(k), elems_1.CTVar(t, kinds_1.KMeta(k)));
        exports.unify(types_1.openTForall(a, types_1.TVar(t)), types_1.openTForall(b, types_1.TVar(t)));
        global_1.context.leave(t);
        return;
    }
    if (types_1.isTMeta(a)) {
        if (types_1.containsTMeta(a.name, b))
            return error_1.infererr(`occurs check L failed: ${types_1.showType(a)} in ${types_1.showType(b)}`);
        return exports.inst(a, b);
    }
    if (types_1.isTMeta(b)) {
        if (types_1.containsTMeta(b.name, a))
            return error_1.infererr(`occurs check R failed: ${types_1.showType(b)} in ${types_1.showType(a)}`);
        return exports.inst(b, a);
    }
    return error_1.infererr(`unify failed: ${types_1.showType(a)} ~ ${types_1.showType(b)}`);
};

},{"./elems":4,"./error":5,"./global":6,"./kindInference":8,"./kindUnification":9,"./kinds":10,"./names":11,"./subsumption":15,"./types":17}],19:[function(require,module,exports){
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

},{"./repl":14}],20:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const elems_1 = require("./elems");
const global_1 = require("./global");
const names_1 = require("./names");
const error_1 = require("./error");
exports.wfKind = (kind) => {
    switch (kind.tag) {
        case 'KVar': {
            if (global_1.context.contains('CKVar', kind.name))
                return;
            return error_1.infererr(`undefined kind ${names_1.showName(kind.name)}`);
        }
        case 'KMeta': {
            if (global_1.context.contains('CKMeta', kind.name))
                return;
            return error_1.infererr(`undefined kind ?${names_1.showName(kind.name)}`);
        }
        case 'KFun': {
            exports.wfKind(kind.left);
            return exports.wfKind(kind.right);
        }
    }
};
exports.wfType = (type) => {
    switch (type.tag) {
        case 'TVar': {
            if (global_1.context.contains('CTVar', type.name))
                return;
            return error_1.infererr(`undefined type ${names_1.showName(type.name)}`);
        }
        case 'TMeta': {
            if (global_1.context.contains('CTMeta', type.name))
                return;
            return error_1.infererr(`undefined type ?${names_1.showName(type.name)}`);
        }
        case 'TApp': {
            exports.wfType(type.left);
            return exports.wfType(type.right);
        }
        case 'TForall': {
            if (type.kind)
                exports.wfKind(type.kind);
            const t = global_1.namestore.fresh(type.name);
            if (type.kind) {
                global_1.context.enter(t, elems_1.CTVar(t, type.kind));
            }
            else {
                const k = global_1.namestore.fresh(type.name);
                global_1.context.enter(t, elems_1.CKMeta(k), elems_1.CTVar(t, kinds_1.KMeta(k)));
            }
            exports.wfType(types_1.openTForall(type, types_1.TVar(t)));
            global_1.context.leave(t);
            return;
        }
    }
};
exports.wfElem = (elem) => {
    // console.log(`wfElem ${showElem(elem)} in ${context}`);
    switch (elem.tag) {
        case 'CKVar': {
            if (!global_1.context.contains('CKVar', elem.name))
                return;
            return error_1.infererr(`duplicate kind ${names_1.showName(elem.name)}`);
        }
        case 'CKMeta': {
            if (global_1.context.contains('CKMeta', elem.name))
                return error_1.infererr(`duplicate kind ?${names_1.showName(elem.name)}`);
            if (elem.kind)
                exports.wfKind(elem.kind);
            return;
        }
        case 'CTVar': {
            if (global_1.context.contains('CTVar', elem.name))
                return error_1.infererr(`duplicate type ${names_1.showName(elem.name)}`);
            return exports.wfKind(elem.kind);
        }
        case 'CTMeta': {
            if (global_1.context.contains('CTMeta', elem.name))
                return error_1.infererr(`duplicate type ?${names_1.showName(elem.name)}`);
            exports.wfKind(elem.kind);
            if (elem.type)
                exports.wfType(elem.type);
            return;
        }
        case 'CVar': {
            if (global_1.context.contains('CVar', elem.name))
                return error_1.infererr(`duplicate var ${names_1.showName(elem.name)}`);
            return exports.wfType(elem.type);
        }
        case 'CMarker': {
            if (!global_1.context.contains('CMarker', elem.name))
                return;
            return error_1.infererr(`duplicate marker ${names_1.showName(elem.name)}`);
        }
    }
};
exports.wfContext = () => {
    global_1.storeContext();
    let elem = global_1.context.pop();
    while (elem) {
        exports.wfElem(elem);
        elem = global_1.context.pop();
    }
    global_1.restoreContext();
};

},{"./elems":4,"./error":5,"./global":6,"./kinds":10,"./names":11,"./types":17}]},{},[19]);

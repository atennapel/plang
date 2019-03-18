(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const terms_1 = require("./terms");
const names_1 = require("./names");
const err = (msg) => { throw new Error(msg); };
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
        case 'If': return `(${exports.compile(term.cond)} ? ${exports.compile(term.then)} : ${exports.compile(term.else_)})`;
        case 'Query': return err(`cannot compile ${terms_1.showTerm(term)}`);
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
        case 'DDeclType': return '';
        case 'DDeclare': return '';
        case 'DForeign': return `${prefix(exports.compileName(def.name))} = ${def.val};`;
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

},{"./names":12,"./terms":17}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.config = {
    showKinds: false,
    logging: false,
};
exports.log = (msg) => {
    if (exports.config.logging)
        console.log(msg);
};

},{}],3:[function(require,module,exports){
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
                ns.push(c);
        }
        return ns;
    }
    first(fn) {
        for (let a = this.elems, l = a.length, i = 0; i < l; i++) {
            const c = fn(a[i]);
            if (c)
                return c;
        }
        return null;
    }
}
exports.Context = Context;

},{"./elems":5,"./names":12}],4:[function(require,module,exports){
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
exports.DDeclType = (name, kind) => ({ tag: 'DDeclType', name, kind });
exports.isDDeclType = (def) => def.tag === 'DDeclType';
exports.DDeclare = (name, type) => ({ tag: 'DDeclare', name, type });
exports.isDDeclare = (def) => def.tag === 'DDeclare';
exports.DForeign = (name, val) => ({ tag: 'DForeign', name, val });
exports.isDForeign = (def) => def.tag === 'DForeign';
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
        case 'DDeclType': return `decltype ${names_1.showName(def.name)} : ${kinds_1.showKind(def.kind)}`;
        case 'DDeclare': return `declare ${names_1.showName(def.name)} : ${types_1.showType(def.type)}`;
        case 'DForeign': return `foreign ${names_1.showName(def.name)} ${JSON.stringify(def.val)}`;
    }
};

},{"./kinds":11,"./names":12,"./terms":17,"./types":18}],5:[function(require,module,exports){
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

},{"./kinds":11,"./names":12,"./types":18}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class InferError extends TypeError {
    constructor(msg) { super(msg); }
}
exports.InferError = InferError;
exports.infererr = (msg) => {
    throw new InferError(msg);
};

},{}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const context_1 = require("./context");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const namestore_1 = require("./namestore");
const initialContext = () => context_1.Context.of(elems_1.CKVar(kinds_1.nType), elems_1.CTVar(types_1.nFun, kinds_1.kfun(kinds_1.kType, kinds_1.kType, kinds_1.kType)), elems_1.CTVar(types_1.nBool, kinds_1.kType));
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

},{"./context":3,"./elems":5,"./kinds":11,"./namestore":13,"./types":18}],8:[function(require,module,exports){
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
const definitions_1 = require("./definitions");
const config_1 = require("./config");
const resolveImplicit = (type) => {
    config_1.log(`resolveImplicit ${types_1.showType(type)}`);
    const x = global_1.context.first(e => {
        if (e.tag !== 'CVar')
            return null;
        global_1.storeContext();
        try {
            subsumption_1.subsume(e.type, type);
            config_1.log(`select ${names_1.showName(e.name)} : ${types_1.showType(e.type)}`);
            return [e.name, e.type];
        }
        catch (err) {
            if (!(err instanceof error_1.InferError))
                throw err;
            return null;
        }
        finally {
            global_1.restoreContext();
        }
    });
    if (!x)
        return error_1.infererr(`no implicit value found for: ${types_1.showType(type)}`);
    return x;
};
const getByName = (cs, x) => {
    for (let i = 0, l = cs.length; i < l; i++)
        if (names_1.eqName(cs[i].name, x))
            return cs[i];
    return null;
};
const unsolvedInType = (unsolved, type, ns = []) => {
    switch (type.tag) {
        case 'TVar': return ns;
        case 'TMeta': {
            const x = type.name;
            const c = getByName(unsolved, x);
            if (c && !getByName(ns, x))
                ns.push(c);
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
        const x = ns[i].name;
        const y = global_1.namestore.fresh(x);
        names_1.insertNameMap(x, types_1.TVar(y), m);
    }
    let c = types_1.substTMetas(type, m);
    for (let i = ns.length - 1; i >= 0; i--) {
        const e = ns[i];
        c = types_1.TForallK(names_1.getNameMap(e.name, m).name, e.kind, c);
    }
    return c;
};
const generalizeFrom = (marker, type) => generalize(global_1.context.leaveWithUnsolved(marker), type);
const typesynth = (term) => {
    config_1.log(`typesynth ${terms_1.showTerm(term)}`);
    if (terms_1.isVar(term)) {
        const x = global_1.context.lookup('CVar', term.name);
        if (!x)
            return error_1.infererr(`undefined var ${names_1.showName(term.name)}`);
        return [x.type, term];
    }
    if (terms_1.isAbs(term)) {
        const x = global_1.namestore.fresh(term.name);
        const a = global_1.namestore.fresh(term.name);
        const b = global_1.namestore.fresh(term.name);
        const ta = types_1.TMeta(a);
        const tb = types_1.TMeta(b);
        global_1.context.enter(x, elems_1.CTMeta(a, kinds_1.kType), elems_1.CTMeta(b, kinds_1.kType), elems_1.CVar(x, ta));
        const body = typecheck(terms_1.openAbs(term, terms_1.Var(x)), tb);
        const ty = global_1.apply(types_1.TFun(ta, tb));
        return [generalizeFrom(x, ty), terms_1.Abs(x, body)];
    }
    if (terms_1.isApp(term)) {
        const [left, nleft] = typesynth(term.left);
        const [right, nright] = typeappsynth(global_1.apply(left), term.right);
        return [right, terms_1.App(nleft, nright)];
    }
    if (terms_1.isAnn(term)) {
        wellformedness_1.wfType(term.type);
        const ty = kindInference_1.elaborateType(term.type);
        kindInference_1.checkKindType(ty);
        const nterm = typecheck(term.term, ty);
        return [ty, terms_1.Ann(nterm, ty)];
    }
    if (terms_1.isLet(term)) {
        const [ty, nterm] = typesynth(term.term);
        const x = global_1.namestore.fresh(term.name);
        global_1.context.enter(x, elems_1.CVar(x, ty));
        const [uty, nbody] = typesynth(terms_1.openLet(term, terms_1.Var(x)));
        const rty = global_1.apply(uty);
        const gty = generalizeFrom(x, rty);
        return [gty, terms_1.Let(x, nterm, nbody)];
    }
    if (terms_1.isIf(term)) {
        const ncond = typecheck(term.cond, types_1.tBool);
        const [ty, nthen] = typesynth(term.then);
        const nelse = typecheck(term.else_, ty);
        return [ty, terms_1.If(ncond, nthen, nelse)];
    }
    return error_1.infererr(`cannot synth: ${terms_1.showTerm(term)}`);
};
const typecheck = (term, type) => {
    config_1.log(`typecheck ${terms_1.showTerm(term)} : ${types_1.showType(type)}`);
    if (types_1.isTForall(type)) {
        if (!type.kind)
            return error_1.infererr(`forall lacks kind: ${types_1.showType(type)}`);
        const x = global_1.namestore.fresh(type.name);
        global_1.context.enter(x, elems_1.CTVar(x, type.kind));
        const nterm = typecheck(term, types_1.openTForall(type, types_1.TVar(x)));
        global_1.context.leave(x);
        return nterm;
    }
    const f = types_1.matchTFun(type);
    if (terms_1.isAbs(term) && f) {
        const x = global_1.namestore.fresh(term.name);
        global_1.context.enter(x, elems_1.CVar(x, f.left));
        const body = typecheck(terms_1.openAbs(term, terms_1.Var(x)), f.right);
        global_1.context.leave(x);
        return terms_1.Abs(x, body);
    }
    if (terms_1.isLet(term)) {
        const [ty, nterm] = typesynth(term.term);
        const x = global_1.namestore.fresh(term.name);
        global_1.context.enter(x, elems_1.CVar(x, ty));
        const nbody = typecheck(terms_1.openLet(term, terms_1.Var(x)), global_1.apply(type));
        global_1.context.leave(x);
        return terms_1.Let(x, nterm, nbody);
    }
    if (terms_1.isIf(term)) {
        const ncond = typecheck(term.cond, types_1.tBool);
        const nthen = typecheck(term.then, type);
        const nelse = typecheck(term.else_, global_1.apply(type));
        return terms_1.If(ncond, nthen, nelse);
    }
    if (terms_1.isQuery(term)) {
        const [y, t] = resolveImplicit(type);
        subsumption_1.subsume(t, type);
        return terms_1.Var(y);
    }
    const [ty, nterm] = typesynth(term);
    subsumption_1.subsume(global_1.apply(ty), global_1.apply(type));
    return nterm;
};
const typeappsynth = (type, term) => {
    config_1.log(`typeappsynth ${types_1.showType(type)} @ ${terms_1.showTerm(term)}`);
    if (types_1.isTForall(type)) {
        if (!type.kind)
            return error_1.infererr(`forall lacks kind: ${types_1.showType(type)}`);
        const x = global_1.namestore.fresh(type.name);
        global_1.context.add(elems_1.CTMeta(x, type.kind));
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
        const nterm = typecheck(term, ta);
        return [tb, nterm];
    }
    const f = types_1.matchTFun(type);
    if (f) {
        const nterm = typecheck(term, f.left);
        return [f.right, nterm];
    }
    // TODO: generalize the below for all type applications
    if (types_1.isTApp(type) && types_1.isTMeta(type.left)) {
        const x = type.left.name;
        const a = global_1.namestore.fresh(x);
        const ta = types_1.TMeta(a);
        global_1.context.replace('CTMeta', x, [
            elems_1.CTMeta(a, kinds_1.kType),
            elems_1.CTMeta(x, kinds_1.kfun(kinds_1.kType, kinds_1.kType), types_1.TApp(types_1.tFun, ta)),
        ]);
        const nterm = typecheck(term, ta);
        return [type.right, nterm];
    }
    if (types_1.isTApp(type) && types_1.isTApp(type.left) && types_1.isTMeta(type.left.left)) {
        const x = type.left.left.name;
        global_1.context.replace('CTMeta', x, [
            elems_1.CTMeta(x, kinds_1.kfun(kinds_1.kType, kinds_1.kType, kinds_1.kType), types_1.tFun),
        ]);
        const nterm = typecheck(term, type.left.right);
        return [type.right, nterm];
    }
    return error_1.infererr(`cannot typeappsynth: ${types_1.showType(type)} @ ${terms_1.showTerm(term)}`);
};
exports.infer = (term) => {
    global_1.namestore.reset();
    wellformedness_1.wfContext();
    const m = global_1.namestore.fresh('m');
    global_1.context.enter(m);
    try {
        const [uty, nterm] = typesynth(term);
        const ty = generalizeFrom(m, global_1.apply(uty));
        kindInference_1.checkKindType(ty);
        if (!global_1.context.isComplete())
            return error_1.infererr(`incomplete context: ${global_1.context}`);
        return [types_1.simplifyType(ty), nterm];
    }
    catch (err) {
        global_1.context.leave(m);
        throw err;
    }
};
exports.inferDef = (def) => {
    config_1.log(`inferDef ${definitions_1.showDef(def)}`);
    switch (def.tag) {
        case 'DType': {
            const tname = def.name;
            const untname = names_1.Name(`un${tname.name}`);
            if (global_1.context.lookup('CTVar', tname))
                throw new TypeError(`type ${names_1.showName(tname)} is already defined`);
            if (global_1.context.lookup('CVar', tname))
                throw new TypeError(`${names_1.showName(tname)} is already defined`);
            if (global_1.context.lookup('CVar', untname))
                throw new TypeError(`${names_1.showName(untname)} is already defined`);
            const ty = types_1.tforallK(def.args, def.type);
            wellformedness_1.wfType(ty);
            const ety = kindInference_1.elaborateType(ty);
            kindInference_1.checkKindType(ety);
            const fl = types_1.flattenTForall(ety);
            const nargs = fl.args.slice(0, def.args.length);
            const ntype = types_1.tforallK(fl.args.slice(def.args.length), fl.type);
            global_1.context.add(elems_1.CTVar(tname, kinds_1.kfunFrom(nargs.map(([_, k]) => k || kinds_1.kType).concat([kinds_1.kType]))), elems_1.CVar(tname, types_1.tforallK(nargs, types_1.tfun(ntype, types_1.tappFrom([types_1.TVar(tname)].concat(nargs.map(([n]) => types_1.TVar(n))))))), elems_1.CVar(untname, types_1.tforallK(nargs, types_1.tfun(types_1.tappFrom([types_1.TVar(tname)].concat(nargs.map(([n]) => types_1.TVar(n)))), ntype))));
            return def;
        }
        case 'DLet': {
            const name = def.name;
            if (global_1.context.lookup('CVar', name))
                throw new TypeError(`${names_1.showName(name)} is already defined`);
            const [ty, term] = exports.infer(terms_1.abs(def.args, def.term));
            global_1.context.add(elems_1.CVar(name, ty));
            return definitions_1.DLet(name, [], term);
        }
        case 'DDeclType': {
            const name = def.name;
            if (global_1.context.lookup('CTVar', name))
                throw new TypeError(`type ${names_1.showName(name)} is already defined`);
            wellformedness_1.wfKind(def.kind);
            global_1.context.add(elems_1.CTVar(name, def.kind));
            return def;
        }
        case 'DDeclare': {
            const name = def.name;
            if (global_1.context.lookup('CVar', name))
                throw new TypeError(`${names_1.showName(name)} is already defined`);
            wellformedness_1.wfType(def.type);
            const type = kindInference_1.elaborateType(def.type);
            kindInference_1.checkKindType(type);
            global_1.context.add(elems_1.CVar(name, type));
            return def;
        }
        case 'DForeign': return def;
    }
};
exports.inferDefs = (ds) => ds.map(exports.inferDef);

},{"./config":2,"./definitions":4,"./elems":5,"./error":6,"./global":7,"./kindInference":9,"./kinds":11,"./names":12,"./subsumption":16,"./terms":17,"./types":18,"./wellformedness":21}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const kindUnification_1 = require("./kindUnification");
const global_1 = require("./global");
const error_1 = require("./error");
const elems_1 = require("./elems");
const names_1 = require("./names");
const config_1 = require("./config");
const elaborateTypeR = (type) => {
    config_1.log(`elaborateTypeR ${types_1.showType(type)}`);
    switch (type.tag) {
        case 'TVar': {
            const e = global_1.context.lookup('CTVar', type.name);
            if (!e)
                return error_1.infererr(`undefined tvar ${types_1.showType(type)}`);
            return [global_1.applyKind(e.kind), type];
        }
        case 'TMeta': {
            const e = global_1.context.lookup('CTMeta', type.name);
            if (!e)
                return error_1.infererr(`undefined tmeta ${types_1.showType(type)}`);
            return [global_1.applyKind(e.kind), type];
        }
        case 'TApp': {
            const [l, tl] = elaborateTypeR(type.left);
            const [r, tr] = elaborateTypeR(type.right);
            const kv = global_1.namestore.fresh('k');
            const km = kinds_1.KMeta(kv);
            global_1.context.add(elems_1.CKMeta(kv));
            kindUnification_1.unifyKinds(l, kinds_1.KFun(r, km));
            return [global_1.applyKind(km), types_1.TApp(tl, tr)];
        }
        case 'TForall': {
            let k;
            if (type.kind) {
                k = type.kind;
            }
            else {
                const kn = global_1.namestore.fresh(type.name);
                global_1.context.add(elems_1.CKMeta(kn));
                k = kinds_1.KMeta(kn);
            }
            global_1.context.enter(type.name, elems_1.CTVar(type.name, k));
            const [ki, tt] = elaborateTypeR(types_1.openTForall(type, types_1.TVar(type.name)));
            global_1.context.leave(type.name);
            return [global_1.applyKind(ki), types_1.TForallK(type.name, global_1.applyKind(k), tt)];
        }
    }
};
const instKMetaInKind = (kind) => {
    switch (kind.tag) {
        case 'KVar': return kind;
        case 'KMeta': return kinds_1.kType;
        case 'KFun':
            return kinds_1.KFun(instKMetaInKind(kind.left), instKMetaInKind(kind.right));
    }
};
const instKMeta = (type) => {
    switch (type.tag) {
        case 'TVar': return type;
        case 'TMeta': return type;
        case 'TApp': return types_1.TApp(instKMeta(type.left), instKMeta(type.right));
        case 'TForall': {
            const k = type.kind ? instKMetaInKind(type.kind) : kinds_1.kType;
            return types_1.TForallK(type.name, k, instKMeta(type.type));
        }
    }
};
exports.elaborateType = (type) => {
    config_1.log(`elaborateType ${types_1.showType(type)}`);
    const m = global_1.namestore.fresh('m');
    global_1.context.enter(m);
    const [_, ty] = elaborateTypeR(type);
    global_1.context.leave(m);
    const ti = instKMeta(ty);
    config_1.log(`result ${types_1.showType(ti)}`);
    return ti;
};
exports.deriveKind = (type) => {
    switch (type.tag) {
        case 'TVar': {
            const c = global_1.context.lookup('CTVar', type.name);
            if (c)
                return c.kind;
            return error_1.infererr(`undefined type ${names_1.showName(type.name)}`);
        }
        case 'TMeta': {
            const c = global_1.context.lookup('CTMeta', type.name);
            if (c)
                return c.kind;
            return error_1.infererr(`undefined type ?${names_1.showName(type.name)}`);
        }
        case 'TApp': {
            const l = exports.deriveKind(type.left);
            const r = exports.deriveKind(type.right);
            if (l.tag !== 'KFun')
                return error_1.infererr(`not a kfun in ${types_1.showType(type)}`);
            if (!kinds_1.eqKind(l.left, r))
                return error_1.infererr(`kind mismatch in ${types_1.showType(type)}`);
            return l.right;
        }
        case 'TForall': {
            if (!type.kind)
                return error_1.infererr(`forall lacks kind: ${types_1.showType(type)}`);
            const t = global_1.namestore.fresh(type.name);
            global_1.context.enter(t, elems_1.CTVar(t, type.kind));
            const k = exports.deriveKind(types_1.openTForall(type, types_1.TVar(t)));
            global_1.context.leave(t);
            return k;
        }
    }
};
exports.checkKindType = (type) => {
    const k = exports.deriveKind(type);
    if (!kinds_1.eqKind(k, kinds_1.kType))
        return error_1.infererr(`expected ${kinds_1.showKind(kinds_1.kType)} but got ${kinds_1.showKind(k)} in ${types_1.showType(type)}`);
};

},{"./config":2,"./elems":5,"./error":6,"./global":7,"./kindUnification":10,"./kinds":11,"./names":12,"./types":18}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const global_1 = require("./global");
const elems_1 = require("./elems");
const kinds_1 = require("./kinds");
const error_1 = require("./error");
const wellformedness_1 = require("./wellformedness");
const config_1 = require("./config");
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
    config_1.log(`instKind ${kinds_1.showKind(x)} ~ ${kinds_1.showKind(kind)} in ${global_1.context}`);
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
        if (kinds_1.isKFun(kind)) {
            const y = x.name;
            const a = global_1.namestore.fresh(y);
            const b = global_1.namestore.fresh(y);
            const ta = kinds_1.KMeta(a);
            const tb = kinds_1.KMeta(b);
            global_1.context.replace('CKMeta', y, [
                elems_1.CKMeta(b),
                elems_1.CKMeta(a),
                elems_1.CKMeta(y, kinds_1.KFun(ta, tb)),
            ]);
            instKind(ta, kind.left);
            instKind(tb, global_1.applyKind(kind.right));
            return;
        }
        return error_1.infererr(`inst kind failed: ${kinds_1.showKind(x)} := ${kinds_1.showKind(kind)}, ${err}`);
    }
};
exports.unifyKinds = (a, b) => {
    config_1.log(`unifyKinds ${kinds_1.showKind(a)} ~ ${kinds_1.showKind(b)} in ${global_1.context}`);
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

},{"./config":2,"./elems":5,"./error":6,"./global":7,"./kinds":11,"./names":12,"./wellformedness":21}],11:[function(require,module,exports){
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
exports.eqKind = (a, b) => {
    switch (a.tag) {
        case 'KVar': return exports.isKVar(b) && names_1.eqName(a.name, b.name);
        case 'KMeta': return exports.isKMeta(b) && names_1.eqName(a.name, b.name);
        case 'KFun': return exports.isKFun(b) && exports.eqKind(a.left, b.left) && exports.eqKind(a.right, b.right);
    }
};
exports.containsKMeta = (x, kind) => {
    switch (kind.tag) {
        case 'KVar': return false;
        case 'KMeta': return names_1.eqName(x, kind.name);
        case 'KFun': return exports.containsKMeta(x, kind.left) || exports.containsKMeta(x, kind.right);
    }
};

},{"./names":12}],12:[function(require,module,exports){
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

},{}],13:[function(require,module,exports){
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

},{"./names":12}],14:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const terms_1 = require("./terms");
const names_1 = require("./names");
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const definitions_1 = require("./definitions");
const config_1 = require("./config");
const err = (msg) => { throw new SyntaxError(msg); };
const VarT = (val) => ({ tag: 'VarT', val });
const matchVarT = (val, t) => t.tag === 'VarT' && t.val === val;
const SymbolT = (val) => ({ tag: 'SymbolT', val });
const matchSymbolT = (val, t) => t.tag === 'SymbolT' && t.val === val;
const StringT = (val) => ({ tag: 'StringT', val });
const ParenT = (val) => ({ tag: 'ParenT', val });
const showToken = (t) => {
    switch (t.tag) {
        case 'SymbolT':
        case 'VarT': return t.val;
        case 'StringT': return JSON.stringify(t.val);
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
const SYM1 = ['\\', ':', '.', '=', '?'];
const SYM2 = ['->', '<|', '|>', '<<', '>>'];
const KEYWORDS = ['let', 'in', 'type', 'if', 'then', 'else'];
const KEYWORDS_TYPE = ['forall', 'type', 'let'];
const KEYWORDS_DEF = ['let', 'type', 'decltype', 'declare', 'foreign'];
const START = 0;
const NAME = 1;
const STRING = 2;
const COMMENT = 3;
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
        case 'VarT': return kinds_1.KVar(names_1.Name(ts.val));
        case 'SymbolT': return err(`stuck on ${ts.val}`);
        case 'ParenT': return parseParensKind(ts.val);
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
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
const parseTokenType = (ts) => {
    config_1.log(`parseTokenType ${showToken(ts)}`);
    switch (ts.tag) {
        case 'VarT': {
            if (KEYWORDS_TYPE.indexOf(ts.val) >= 0)
                return err(`stuck on ${ts.val}`);
            return types_1.TVar(names_1.Name(ts.val));
        }
        case 'SymbolT': {
            if (ts.val === '->')
                return types_1.tFun;
            return err(`stuck on ${ts.val}`);
        }
        case 'ParenT': return parseParensType(ts.val);
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
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
            return terms_1.Var(names_1.Name(ts.val));
        }
        case 'SymbolT': {
            if (ts.val === '?')
                return terms_1.Query;
            if (ts.val === '<|') {
                const f = names_1.Name('f');
                const x = names_1.Name('x');
                return terms_1.abs([f, x], terms_1.appFrom([terms_1.Var(f), terms_1.Var(x)]));
            }
            if (ts.val === '|>') {
                const f = names_1.Name('f');
                const x = names_1.Name('x');
                return terms_1.abs([x, f], terms_1.appFrom([terms_1.Var(f), terms_1.Var(x)]));
            }
            if (ts.val === '<<') {
                const f = names_1.Name('f');
                const g = names_1.Name('g');
                const x = names_1.Name('x');
                return terms_1.abs([f, g, x], terms_1.App(terms_1.Var(f), terms_1.App(terms_1.Var(g), terms_1.Var(x))));
            }
            if (ts.val === '>>') {
                const f = names_1.Name('f');
                const g = names_1.Name('g');
                const x = names_1.Name('x');
                return terms_1.abs([g, f, x], terms_1.App(terms_1.Var(f), terms_1.App(terms_1.Var(g), terms_1.Var(x))));
            }
            return err(`stuck on ${ts.val}`);
        }
        case 'ParenT': return parseParens(ts.val);
        case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
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
                const x = names_1.Name('_x');
                return terms_1.abs([x], terms_1.App(f, terms_1.Var(x)));
                // (<| x) = \f -> f x
            }
            else if (split[0].length === 0) {
                const f = names_1.Name('_f');
                const x = parseParens(split[1]);
                return terms_1.abs([f], terms_1.App(terms_1.Var(f), x));
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
                const f = names_1.Name('_f');
                const x = parseParens(split[0]);
                return terms_1.abs([f], terms_1.App(terms_1.Var(f), x));
                // (|> f) = \x -> f x
            }
            else if (split[0].length === 0) {
                const f = parseParens(split[1]);
                const x = names_1.Name('_x');
                return terms_1.abs([x], terms_1.App(f, terms_1.Var(x)));
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
                const g = names_1.Name('_g');
                const x = names_1.Name('_x');
                return terms_1.abs([g, x], terms_1.App(f, terms_1.App(terms_1.Var(g), terms_1.Var(x))));
                // (<< g) = \f x -> f (g x)
            }
            else if (split[0].length === 0) {
                const f = names_1.Name('_f');
                const g = parseParens(split[1]);
                const x = names_1.Name('_x');
                return terms_1.abs([f, x], terms_1.App(terms_1.Var(f), terms_1.App(g, terms_1.Var(x))));
            }
        }
        const terms = split.map(parseParens);
        const x = names_1.Name('_x');
        return terms_1.abs([x], terms.reduceRight((x, y) => terms_1.App(y, x), terms_1.Var(x)));
    }
    if (contains(ts, t => matchSymbolT('>>', t))) {
        const split = splitTokens(ts, t => matchSymbolT('>>', t));
        // special case
        if (split.length === 2) {
            // (f >>) = \g x -> g (f x)
            if (split[1].length === 0) {
                const f = parseParens(split[0]);
                const g = names_1.Name('_g');
                const x = names_1.Name('_x');
                return terms_1.abs([g, x], terms_1.App(terms_1.Var(g), terms_1.App(f, terms_1.Var(x))));
                // (>> g) = \f x -> g (f x)
            }
            else if (split[0].length === 0) {
                const f = names_1.Name('_f');
                const g = parseParens(split[1]);
                const x = names_1.Name('_x');
                return terms_1.abs([f, x], terms_1.App(g, terms_1.App(terms_1.Var(f), terms_1.Var(x))));
            }
        }
        const terms = split.map(parseParens);
        const x = names_1.Name('_x');
        return terms_1.abs([x], terms.reverse().reduceRight((x, y) => terms_1.App(y, x), terms_1.Var(x)));
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
    if (matchVarT('decltype', ts[0])) {
        if (ts[1].tag !== 'VarT')
            return err(`invalid type name: ${ts[1].val}`);
        const name = ts[1].val;
        if (ts[2].tag !== 'SymbolT' || ts[2].val !== ':')
            return err(`: expected after declare name but got ${ts[2].val}`);
        let i = 3;
        const bodyts = [];
        while (true) {
            const c = ts[i++];
            if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0))
                break;
            bodyts.push(c);
        }
        const body = parseParensKind(bodyts);
        const rest = parseParensDefs(ts.slice(i - 1));
        return [definitions_1.DDeclType(names_1.Name(name), body)].concat(rest);
    }
    if (matchVarT('declare', ts[0])) {
        if (ts[1].tag !== 'VarT')
            return err(`invalid def name: ${ts[1].val}`);
        const name = ts[1].val;
        if (ts[2].tag !== 'SymbolT' || ts[2].val !== ':')
            return err(`: expected after declare name but got ${ts[2].val}`);
        let i = 3;
        const bodyts = [];
        while (true) {
            const c = ts[i++];
            if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0))
                break;
            bodyts.push(c);
        }
        const body = parseParensType(bodyts);
        const rest = parseParensDefs(ts.slice(i - 1));
        return [definitions_1.DDeclare(names_1.Name(name), body)].concat(rest);
    }
    if (matchVarT('foreign', ts[0])) {
        if (ts[1].tag !== 'VarT')
            return err(`invalid foreign name: ${ts[1].val}`);
        const name = ts[1].val;
        if (ts[2].tag !== 'StringT')
            return err(`string literal expected for foreign ${name} but got ${ts[2].val}`);
        const body = ts[2].val;
        const rest = parseParensDefs(ts.slice(3));
        return [definitions_1.DForeign(names_1.Name(name), body)].concat(rest);
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

},{"./config":2,"./definitions":4,"./kinds":11,"./names":12,"./terms":17,"./types":18}],15:[function(require,module,exports){
(function (global){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const compiler_1 = require("./compiler");
const inference_1 = require("./inference");
const parser_1 = require("./parser");
const global_1 = require("./global");
const config_1 = require("./config");
const names_1 = require("./names");
const terms_1 = require("./terms");
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
const _prelude = "; some primitives so we have some concrete values\ndeclare True : Bool\nforeign True \"true\"\ndeclare False : Bool\nforeign False \"false\"\nlet not b = if b then False else True\nlet and a b = if a then b else False\nlet or a b = if a then True else b\n\ndecltype PrimNat : Type\ndeclare primZ : PrimNat\nforeign primZ \"0\"\ndeclare primS : PrimNat -> PrimNat\nforeign primS \"x => x + 1\"\n\ndecltype PrimList : Type -> Type\ndeclare primNil : forall t. PrimList t\nforeign primNil \"[]\"\ndeclare primCons : forall t. t -> PrimList t -> PrimList t\nforeign primCons \"h => t => [h].concat(t)\"\n\ndecltype Pair : Type -> Type -> Type\ndeclare pair : forall a b. a -> b -> Pair a b\nforeign pair \"a => b => [a, b]\"\ndeclare fst : forall a b. Pair a b -> a\nforeign fst \"p => p[0]\"\ndeclare snd : forall a b. Pair a b -> b\nforeign snd \"p => p[1]\"\n\n; our base types\ntype Void = forall t. t\n\ntype Unit = forall t. t -> t\nlet unit = Unit \\x -> x\n\ntype Sum a b = forall r. (a -> r) -> (b -> r) -> r\nlet inl x = Sum \\f g -> f x\nlet inr x = Sum \\f g -> g x\n\ntype List t = forall r. r -> (t -> r -> r) -> r\nlet Nil = List \\n c -> n\nlet Cons h t = List \\n c -> c h (unList t n c)\nlet showList l = unList l primNil primCons\n\nlet foldl f v l = unList l v f\nlet mapList f = foldl (\\h t -> Cons (f h) t) Nil\n\ntype Functor f = forall a b. (a -> b) -> f a -> f b\nlet map = unFunctor\n\nlet ArrFunctor = Functor \\f g x -> f (g x)\nlet ListFunctor = Functor mapList\n";
const _env = typeof global === 'undefined' ? 'window' : 'global';
exports.run = (_s, _cb) => {
    if (_s === ':c' || _s === ':ctx' || _s === ':context')
        return _cb(`${global_1.context}`);
    if (_s === ':showkinds' || _s === ':k') {
        config_1.config.showKinds = !config_1.config.showKinds;
        return _cb(`showKinds: ${config_1.config.showKinds}`);
    }
    if (_s === ':logging' || _s === ':l') {
        config_1.config.logging = !config_1.config.logging;
        return _cb(`logging: ${config_1.config.logging}`);
    }
    if (_s.startsWith(':def ')) {
        try {
            const _rest = _s.slice(4).trim();
            const _ds = parser_1.parseDefs(_rest);
            const _nds = inference_1.inferDefs(_ds);
            const _c = compiler_1.compileDefs(_nds, n => `${_env}['${n}']`);
            eval(`(() => {${_c}})()`);
            return _cb(`defined ${_nds.map(d => names_1.showName(d.name)).join(' ')}`);
        }
        catch (err) {
            return _cb(`${err}`, true);
        }
    }
    if (_s === ':p' || _s === ':prelude') {
        try {
            const _ds = parser_1.parseDefs(_prelude);
            const _nds = inference_1.inferDefs(_ds);
            const _c = compiler_1.compileDefs(_nds, n => `${_env}['${n}']`);
            eval(`(() => {${_c}})()`);
            return _cb(`defined ${_nds.map(d => names_1.showName(d.name)).join(' ')}`);
        }
        catch (err) {
            return _cb(`${err}`, true);
        }
    }
    try {
        const _e = parser_1.parse(_s);
        config_1.log(terms_1.showTerm(_e));
        const [_t, _ne] = inference_1.infer(_e);
        config_1.log(types_1.showType(_t));
        const _c = compiler_1.compile(_ne);
        config_1.log(_c);
        const _v = eval(_c);
        config_1.log(_v);
        return _cb(`${_show(_v)} : ${types_1.showType(_t)}`);
    }
    catch (err) {
        return _cb('' + err, true);
    }
};

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./compiler":1,"./config":2,"./global":7,"./inference":8,"./names":12,"./parser":14,"./terms":17,"./types":18}],16:[function(require,module,exports){
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
const kindInference_1 = require("./kindInference");
const config_1 = require("./config");
exports.solve = (x, type) => {
    config_1.log(`solve ${types_1.showType(x)} := ${types_1.showType(type)}`);
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
const instL = (x, type) => {
    config_1.log(`instL ${types_1.showType(x)} := ${types_1.showType(type)}`);
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
            if (!type.kind)
                return error_1.infererr(`forall lacks kind: ${types_1.showType(type)}`);
            const y = global_1.namestore.fresh(type.name);
            global_1.context.enter(y, elems_1.CTVar(y, type.kind));
            instL(x, types_1.openTForall(type, types_1.TVar(y)));
            global_1.context.leave(y);
            return;
        }
        return error_1.infererr(`instL failed: ${types_1.showType(x)} := ${types_1.showType(type)}, ${err}`);
    }
};
const instR = (type, x) => {
    config_1.log(`instR ${types_1.showType(x)} =: ${types_1.showType(type)}`);
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
            if (!type.kind)
                return error_1.infererr(`forall lacks kind: ${types_1.showType(type)}`);
            const y = global_1.namestore.fresh(type.name);
            global_1.context.enter(y, elems_1.CTMeta(y, type.kind));
            instR(types_1.openTForall(type, types_1.TMeta(y)), x);
            global_1.context.leave(y);
            return;
        }
        return error_1.infererr(`instR failed: ${types_1.showType(x)} := ${types_1.showType(type)}, ${err}`);
    }
};
exports.subsume = (a, b) => {
    config_1.log(`subsume ${types_1.showType(a)} <: ${types_1.showType(b)} in ${global_1.context}`);
    if (a === b)
        return;
    if (!kinds_1.eqKind(kindInference_1.deriveKind(a), kindInference_1.deriveKind(b)))
        return error_1.infererr(`kind mismatch in ${types_1.showType(a)} <: ${types_1.showType(b)}`);
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
        if (!a.kind)
            return error_1.infererr(`forall lacks kind: ${types_1.showType(a)}`);
        const t = global_1.namestore.fresh(a.name);
        global_1.context.enter(t, elems_1.CTMeta(t, a.kind));
        exports.subsume(types_1.openTForall(a, types_1.TMeta(t)), b);
        global_1.context.leave(t);
        return;
    }
    if (types_1.isTForall(b)) {
        if (!b.kind)
            return error_1.infererr(`forall lacks kind: ${types_1.showType(b)}`);
        const t = global_1.namestore.fresh(b.name);
        global_1.context.enter(t, elems_1.CTVar(t, b.kind));
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

},{"./config":2,"./elems":5,"./error":6,"./global":7,"./kindInference":9,"./kinds":11,"./names":12,"./types":18,"./unification":19,"./wellformedness":21}],17:[function(require,module,exports){
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
exports.If = (cond, then, else_) => ({ tag: 'If', cond, then, else_ });
exports.isIf = (term) => term.tag === 'If';
exports.Query = { tag: 'Query' };
exports.isQuery = (term) => term.tag === 'Query';
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
        case 'If':
            return `(if ${exports.showTerm(term.cond)} then ${exports.showTerm(term.then)} else ${exports.showTerm(term.else_)})`;
        case 'Query': return '?';
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
        case 'If': {
            const cond = substVar(x, s, term.cond);
            const then = substVar(x, s, term.then);
            const else_ = substVar(x, s, term.else_);
            return exports.If(cond, then, else_);
        }
        case 'Query': return term;
    }
};
exports.openAbs = (a, s) => substVar(a.name, s, a.body);
exports.openLet = (a, s) => substVar(a.name, s, a.body);

},{"./names":12,"./types":18}],18:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const names_1 = require("./names");
const kinds_1 = require("./kinds");
const namestore_1 = require("./namestore");
const config_1 = require("./config");
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
exports.nBool = names_1.Name('Bool');
exports.tBool = exports.TVar(exports.nBool);
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
                .map(([n, k]) => k && config_1.config.showKinds ? `(${names_1.showName(n)} : ${kinds_1.showKind(k)})` : names_1.showName(n))
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

},{"./config":2,"./kinds":11,"./names":12,"./namestore":13}],19:[function(require,module,exports){
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
const config_1 = require("./config");
exports.inst = (x, type) => {
    config_1.log(`inst ${types_1.showType(x)} := ${types_1.showType(type)}`);
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
            const ka = kindInference_1.deriveKind(type.left);
            const kb = kindInference_1.deriveKind(type.right);
            const kr = ka.right;
            const y = x.name;
            const a = global_1.namestore.fresh(y);
            const b = global_1.namestore.fresh(y);
            const ta = types_1.TMeta(a);
            const tb = types_1.TMeta(b);
            global_1.context.replace('CTMeta', y, [
                elems_1.CTMeta(b, kb),
                elems_1.CTMeta(a, ka),
                elems_1.CTMeta(y, kr, types_1.TApp(ta, tb)),
            ]);
            exports.inst(ta, type.left);
            exports.inst(tb, global_1.apply(type.right));
            return;
        }
        if (types_1.isTForall(type)) {
            if (!type.kind)
                return error_1.infererr(`forall lacks kind: ${types_1.showType(type)}`);
            const y = global_1.namestore.fresh(type.name);
            global_1.context.enter(y, elems_1.CTVar(y, type.kind));
            exports.inst(x, types_1.openTForall(type, types_1.TVar(y)));
            global_1.context.leave(y);
            return;
        }
        return error_1.infererr(`inst failed: ${types_1.showType(x)} := ${types_1.showType(type)}, ${err}`);
    }
};
exports.unify = (a, b) => {
    config_1.log(`unify ${types_1.showType(a)} ~ ${types_1.showType(b)} in ${global_1.context}`);
    if (a === b)
        return;
    if (!kinds_1.eqKind(kindInference_1.deriveKind(a), kindInference_1.deriveKind(b)))
        return error_1.infererr(`kind mismatch in ${types_1.showType(a)} ~ ${types_1.showType(b)}`);
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
        if (!a.kind)
            return error_1.infererr(`forall lacks kind: ${types_1.showType(a)}`);
        if (!b.kind)
            return error_1.infererr(`forall lacks kind: ${types_1.showType(b)}`);
        if (!kinds_1.eqKind(a.kind, b.kind))
            return error_1.infererr(`kind does not match in foralls: ${types_1.showType(a)} ~ ${types_1.showType(b)}`);
        const t = global_1.namestore.fresh(a.name);
        global_1.context.enter(t, elems_1.CTVar(t, a.kind));
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

},{"./config":2,"./elems":5,"./error":6,"./global":7,"./kindInference":9,"./kinds":11,"./names":12,"./subsumption":16,"./types":18}],20:[function(require,module,exports){
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

},{"./repl":15}],21:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const elems_1 = require("./elems");
const global_1 = require("./global");
const names_1 = require("./names");
const error_1 = require("./error");
const config_1 = require("./config");
exports.wfKind = (kind) => {
    config_1.log(`wfKind ${kinds_1.showKind(kind)} in ${global_1.context}`);
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
    config_1.log(`wfType ${types_1.showType(type)} in ${global_1.context}`);
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
    config_1.log(`wfElem ${elems_1.showElem(elem)} in ${global_1.context}`);
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
    config_1.log(`wfContext ${global_1.context}`);
    global_1.storeContext();
    let elem = global_1.context.pop();
    while (elem) {
        exports.wfElem(elem);
        elem = global_1.context.pop();
    }
    global_1.restoreContext();
};

},{"./config":2,"./elems":5,"./error":6,"./global":7,"./kinds":11,"./names":12,"./types":18}]},{},[20]);

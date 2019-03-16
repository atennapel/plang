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

},{"./names":10}],2:[function(require,module,exports){
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

},{"./elems":3,"./names":10}],3:[function(require,module,exports){
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

},{"./kinds":9,"./names":10,"./types":16}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class InferError extends TypeError {
    constructor(msg) { super(msg); }
}
exports.InferError = InferError;
exports.infererr = (msg) => {
    throw new InferError(msg);
};

},{}],5:[function(require,module,exports){
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

},{"./context":2,"./elems":3,"./kinds":9,"./namestore":11,"./types":16}],6:[function(require,module,exports){
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
    global_1.context.enter(m);
    const ty = generalizeFrom(m, global_1.apply(typesynth(term)));
    kindInference_1.checkKindType(ty);
    if (!global_1.context.isComplete())
        return error_1.infererr(`incomplete context: ${global_1.context}`);
    return types_1.simplifyType(ty);
};

},{"./elems":3,"./error":4,"./global":5,"./kindInference":7,"./kinds":9,"./names":10,"./subsumption":14,"./terms":15,"./types":16,"./wellformedness":19}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const kinds_1 = require("./kinds");
const kindUnification_1 = require("./kindUnification");
const global_1 = require("./global");
const error_1 = require("./error");
const elems_1 = require("./elems");
exports.inferKind = (type) => {
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
            if (type.kind) {
                global_1.context.enter(t, elems_1.CTVar(t, type.kind));
            }
            else {
                const k = global_1.namestore.fresh(type.name);
                global_1.context.enter(t, elems_1.CKMeta(k), elems_1.CTVar(t, kinds_1.KMeta(k)));
            }
            const ki = exports.inferKind(types_1.openTForall(type, types_1.TVar(t)));
            global_1.context.leave(t);
            return global_1.applyKind(ki);
        }
    }
};
exports.checkKindType = (type) => kindUnification_1.unifyKinds(exports.inferKind(type), kinds_1.kType);

},{"./elems":3,"./error":4,"./global":5,"./kindUnification":8,"./kinds":9,"./types":16}],8:[function(require,module,exports){
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
        return error_1.infererr(`inst kind failed: ${kinds_1.showKind(x)} := ${kinds_1.showKind(kind)}`);
    }
};
exports.unifyKinds = (a, b) => {
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

},{"./elems":3,"./error":4,"./global":5,"./kinds":9,"./names":10,"./wellformedness":19}],9:[function(require,module,exports){
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

},{"./names":10}],10:[function(require,module,exports){
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

},{}],11:[function(require,module,exports){
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

},{"./names":10}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const terms_1 = require("./terms");
const names_1 = require("./names");
const err = (msg) => { throw new SyntaxError(msg); };
const SymbolT = (val) => ({ tag: 'SymbolT', val });
const VarT = (val) => ({ tag: 'VarT', val });
const KeywordT = (val) => ({ tag: 'KeywordT', val });
const showTokens = (ts) => ts.map(x => `${x.val}`).join(' ');
const SYM1 = ['(', ')', '\\', '=', ':', '.'];
const SYM2 = ['->'];
const KEYWORDS = ['let', 'in'];
const START = 0;
const NAME = 1;
const tokenize = (sc) => {
    let state = START;
    const r = [];
    let t = '';
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
            else if (/\s/.test(c))
                continue;
            else
                return err(`invalid char ${c} in tokenize`);
        }
        else if (state === NAME) {
            if (!/[a-z0-9]/i.test(c)) {
                r.push(KEYWORDS.indexOf(t) >= 0 ? KeywordT(t) :
                    VarT(t));
                t = '', i--, state = START;
            }
            else
                t += c;
        }
    }
    if (state !== START)
        return err('invalid tokenize end state');
    return r;
};
// parser
const match = (ts, tag, val = null) => {
    if (ts.length === 0)
        return false;
    const top = ts[ts.length - 1];
    if (top.tag === tag && (!val || top.val === val)) {
        ts.pop();
        return true;
    }
    return false;
};
const safeMatch = (ts, tag, val = null) => {
    if (ts.length === 0)
        return false;
    const top = ts[ts.length - 1];
    if (top.tag === tag && (!val || top.val === val))
        return true;
    return false;
};
// kinds
const parseKindR = (ts) => {
    if (ts.length === 0)
        return err('empty kind');
    if (safeMatch(ts, 'VarT')) {
        const x = ts.pop();
        return kinds_1.KVar(names_1.Name(x.val));
    }
    return err(`parseKindR stuck on ${ts[ts.length - 1].val}`);
};
exports.parseKind = (sc) => {
    const ts = tokenize(sc);
    const ex = parseKindR(ts.reverse());
    if (ts.length > 0)
        return err(`kind stuck on ${ts[0].val}`);
    return ex;
};
// types
const parseTypeR = (ts) => {
    if (ts.length === 0)
        return err('empty type');
    if (safeMatch(ts, 'VarT')) {
        const x = ts.pop();
        return types_1.TVar(names_1.Name(x.val));
    }
    return err(`parseTypeR stuck on ${ts[ts.length - 1].val}`);
};
exports.parseType = (sc) => {
    const ts = tokenize(sc);
    const ex = parseTypeR(ts.reverse());
    if (ts.length > 0)
        return err(`type stuck on ${ts[0].val}`);
    return ex;
};
// terms
const parseArg = (ts) => {
    if (ts.length === 0)
        return err(`empty in argument`);
    const x = ts.pop();
    if (x.tag === 'VarT')
        return x.val;
    return err(`invalid arg: ${x.val}`);
};
const parseAppTo = (ts, fn) => {
    const es = [];
    while (fn(ts))
        es.push(parseExpr(ts));
    if (es.length === 0)
        return err('empty app');
    if (es.length === 1)
        return es[0];
    return terms_1.appFrom(es);
};
const parseApp = (ts) => parseAppTo(ts, ts => {
    if (match(ts, 'SymbolT', ')'))
        return false;
    if (ts.length === 0)
        return err('app end');
    return true;
});
const parseAppAll = (ts) => parseAppTo(ts, ts => {
    if (ts.length === 0 ||
        safeMatch(ts, 'SymbolT', ')') ||
        safeMatch(ts, 'SymbolT', ':') ||
        safeMatch(ts, 'KeywordT', 'in'))
        return false;
    return true;
});
const parseExpr = (ts) => {
    // console.log(showTokens(ts.slice(0).reverse()));
    if (ts.length === 0)
        return err('empty expr');
    if (match(ts, 'SymbolT', '\\')) {
        const args = [];
        while (!match(ts, 'SymbolT', '->'))
            args.push(parseArg(ts));
        if (args.length === 0)
            return err('empty args after \\');
        const body = parseAppAll(ts);
        return terms_1.abs(args.map(names_1.Name), body);
    }
    else if (match(ts, 'SymbolT', '(')) {
        return parseApp(ts);
    }
    else if (match(ts, 'KeywordT', 'let')) {
        if (!safeMatch(ts, 'VarT'))
            return err('no name after let');
        const x = ts.pop().val;
        if (!match(ts, 'SymbolT', '='))
            return err('no = after name after let');
        const val = parseAppTo(ts, ts => {
            if (match(ts, 'KeywordT', 'in'))
                return false;
            if (ts.length === 0)
                return err('no in after let');
            return true;
        });
        const body = parseAppAll(ts);
        return terms_1.Let(names_1.Name(x), val, body);
    }
    else if (safeMatch(ts, 'VarT')) {
        const x = ts.pop();
        return terms_1.Var(names_1.Name(x.val));
    }
    return err(`parseExpr stuck on ${ts[ts.length - 1].val}`);
};
exports.parseTerm = (sc) => {
    const ts = tokenize(sc);
    const ex = parseAppAll(ts.reverse());
    if (ts.length > 0) {
        if (match(ts, 'SymbolT', ':')) {
            const ty = parseTypeR(ts);
            return terms_1.Ann(ex, ty);
        }
        return err(`stuck on ${ts[0].val}`);
    }
    return ex;
};

},{"./kinds":9,"./names":10,"./terms":15,"./types":16}],13:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const compiler_1 = require("./compiler");
const inference_1 = require("./inference");
const parser_1 = require("./parser");
const global_1 = require("./global");
const elems_1 = require("./elems");
const names_1 = require("./names");
const kinds_1 = require("./kinds");
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
const _Bool = names_1.Name('Bool');
const _Nat = names_1.Name('Nat');
const _List = names_1.Name('List');
const _t = names_1.Name('t');
const _tv = types_1.TVar(_t);
const _r = names_1.Name('r');
const _rv = types_1.TVar(_r);
global_1.context.add(elems_1.CTVar(_Bool, kinds_1.kType), elems_1.CVar(names_1.Name('True'), types_1.TVar(_Bool)), elems_1.CVar(names_1.Name('False'), types_1.TVar(_Bool)), elems_1.CVar(names_1.Name('if'), types_1.tforallK([[names_1.Name('t'), kinds_1.kType]], types_1.tfun(types_1.TVar(_Bool), types_1.TVar(names_1.Name('t')), types_1.TVar(names_1.Name('t')), types_1.TVar(names_1.Name('t'))))), elems_1.CTVar(_Nat, kinds_1.kType), elems_1.CVar(names_1.Name('Z'), types_1.TVar(_Nat)), elems_1.CVar(names_1.Name('S'), types_1.tfun(types_1.TVar(_Nat), types_1.TVar(_Nat))), elems_1.CVar(names_1.Name('caseNat'), types_1.tforallK([[names_1.Name('t'), kinds_1.kType]], types_1.tfun(types_1.TVar(names_1.Name('t')), types_1.tfun(types_1.TVar(_Nat), types_1.TVar(names_1.Name('t'))), types_1.TVar(_Nat), types_1.TVar(names_1.Name('t'))))), elems_1.CVar(names_1.Name('iterNat'), types_1.tforallK([[names_1.Name('t'), kinds_1.kType]], types_1.tfun(types_1.TVar(names_1.Name('t')), types_1.tfun(types_1.TVar(names_1.Name('t')), types_1.TVar(names_1.Name('t'))), types_1.TVar(_Nat), types_1.TVar(names_1.Name('t'))))), elems_1.CVar(names_1.Name('recNat'), types_1.tforallK([[names_1.Name('t'), kinds_1.kType]], types_1.tfun(types_1.TVar(names_1.Name('t')), types_1.tfun(types_1.TVar(_Nat), types_1.TVar(names_1.Name('t')), types_1.TVar(names_1.Name('t'))), types_1.TVar(_Nat), types_1.TVar(names_1.Name('t'))))), elems_1.CTVar(_List, kinds_1.kfun(kinds_1.kType, kinds_1.kType)), elems_1.CVar(names_1.Name('Nil'), types_1.tforallK([[_t, kinds_1.kType]], types_1.tapp(types_1.TVar(_List), _tv))), elems_1.CVar(names_1.Name('Cons'), types_1.tforallK([[_t, kinds_1.kType]], types_1.tfun(_tv, types_1.tapp(types_1.TVar(_List), _tv), types_1.tapp(types_1.TVar(_List), _tv)))), elems_1.CVar(names_1.Name('caseList'), types_1.tforallK([[_t, kinds_1.kType], [_r, kinds_1.kType]], types_1.tfun(_rv, types_1.tfun(_tv, types_1.tapp(types_1.TVar(_List), _tv), _rv), types_1.tapp(types_1.TVar(_List), _tv), _rv))), elems_1.CVar(names_1.Name('iterList'), types_1.tforallK([[_t, kinds_1.kType], [_r, kinds_1.kType]], types_1.tfun(_rv, types_1.tfun(_tv, _rv, _rv), types_1.tapp(types_1.TVar(_List), _tv), _rv))), elems_1.CVar(names_1.Name('recList'), types_1.tforallK([[_t, kinds_1.kType], [_r, kinds_1.kType]], types_1.tfun(_rv, types_1.tfun(_tv, types_1.tapp(types_1.TVar(_List), _tv), _rv, _rv), types_1.tapp(types_1.TVar(_List), _tv), _rv))));
exports.run = (_s, _cb) => {
    if (_s === ':ctx')
        return _cb(`${global_1.context}`);
    try {
        const _e = parser_1.parseTerm(_s);
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

},{"./compiler":1,"./elems":3,"./global":5,"./inference":6,"./kinds":9,"./names":10,"./parser":12,"./types":16}],14:[function(require,module,exports){
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
        return error_1.infererr(`instL failed: ${types_1.showType(x)} := ${types_1.showType(type)}`);
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
        return error_1.infererr(`instR failed: ${types_1.showType(x)} := ${types_1.showType(type)}`);
    }
};
exports.subsume = (a_, b_) => {
    const a = global_1.apply(a_);
    const b = global_1.apply(b_);
    kindUnification_1.unifyKinds(kindInference_1.inferKind(a), kindInference_1.inferKind(b));
    if (a === b)
        return;
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

},{"./elems":3,"./error":4,"./global":5,"./kindInference":7,"./kindUnification":8,"./kinds":9,"./names":10,"./types":16,"./unification":17,"./wellformedness":19}],15:[function(require,module,exports){
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

},{"./names":10,"./types":16}],16:[function(require,module,exports){
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

},{"./kinds":9,"./names":10,"./namestore":11}],17:[function(require,module,exports){
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
        return error_1.infererr(`inst failed: ${types_1.showType(x)} := ${types_1.showType(type)}`);
    }
};
exports.unify = (a_, b_) => {
    const a = global_1.apply(a_);
    const b = global_1.apply(b_);
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

},{"./elems":3,"./error":4,"./global":5,"./kindInference":7,"./kindUnification":8,"./kinds":9,"./names":10,"./subsumption":14,"./types":16}],18:[function(require,module,exports){
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

},{"./repl":13}],19:[function(require,module,exports){
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

},{"./elems":3,"./error":4,"./global":5,"./kinds":9,"./names":10,"./types":16}]},{},[18]);

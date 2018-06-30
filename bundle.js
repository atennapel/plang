(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const exprs_1 = require("./exprs");
const definitions_1 = require("./definitions");
function compile(expr) {
    if (expr instanceof exprs_1.EVar)
        return `${expr.name}`;
    if (expr instanceof exprs_1.EApp)
        return `${compile(expr.left)}(${compile(expr.right)})`;
    if (expr instanceof exprs_1.EAbs)
        return `(${expr.name} => ${compile(expr.expr)})`;
    if (expr instanceof exprs_1.ELit)
        return typeof expr.val === 'string' ? JSON.stringify(expr.val) : `${expr.val}`;
    if (expr instanceof exprs_1.EEmpty)
        return `_recEmpty`;
    if (expr instanceof exprs_1.ESelect)
        return `_recSelect(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.EExtend)
        return `_recExtend(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.ERestrict)
        return `_recRestrict(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.ERecUpdate)
        return `_recUpdate(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.EVarEmpty)
        return `_varEmpty`;
    if (expr instanceof exprs_1.EInject)
        return `_varInject(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.EEmbed)
        return `_varEmbed(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.ECase)
        return `_varCase(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.EVarUpdate)
        return `_varUpdate(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.EReturn)
        return `_return`;
    if (expr instanceof exprs_1.EPure)
        return `_pure`;
    if (expr instanceof exprs_1.EOp)
        return `_op(${JSON.stringify(expr.label)})`;
    if (expr instanceof exprs_1.EDo)
        return `_do`;
    if (expr instanceof exprs_1.EHandler)
        return `_handler({${expr.map.map(([op, e]) => `${op}:${compile(e)}`).join(',')}})`;
    return util_1.impossible();
}
exports.compile = compile;
function varPrefix(name, attachVars) {
    return attachVars ? `(typeof global === 'undefined'? window: global)['${name}']` : `const ${name}`;
}
function compileConstructor(n, l) {
    const a = [];
    for (let i = 0; i < l; i++)
        a.push(`x${i}`);
    return `(${a.join('=>')}${a.length === 0 ? '' : '=>'}({_adt:true,_tag:'${n}',_args:[${a.join(',')}]}))`;
}
exports.compileConstructor = compileConstructor;
function compileCase(n, c) {
    const a = [];
    for (let i = 0; i < c.length; i++)
        a.push(`f${c[i][0]}`);
    return `${a.join('=>')}${a.length === 0 ? '' : '=>'}x=>{switch(x._tag){${c.map(([cn, ts]) => `case '${cn}':return f${cn}${ts.map((_, i) => `(x._args[${i}])`).join('')};break;`).join('')}}throw new Error('case failed for ${n}')}`;
}
exports.compileCase = compileCase;
function compileCata(n, c, rtype) {
    const a = [];
    for (let i = 0; i < c.length; i++)
        a.push(`f${c[i][0]}`);
    return `${a.join('=>')}${a.length === 0 ? '' : '=>'}case${n}${c.map(([cn, ts]) => `(${ts.length === 0 ? `f${cn}` : ts.map((_, i) => `x${i}`).join('=>') + '=>' + `f${cn}` + ts.map((t, i) => t.equals(rtype) ? `(cata${n}${a.map(x => `(${x})`).join('')}(x${i}))` : `(x${i})`).join('')})`).join('')}`;
}
exports.compileCata = compileCata;
function compilePara(n, c, rtype) {
    const a = [];
    for (let i = 0; i < c.length; i++)
        a.push(`f${c[i][0]}`);
    return `${a.join('=>')}${a.length === 0 ? '' : '=>'}case${n}${c.map(([cn, ts]) => `(${ts.length === 0 ? `f${cn}` : ts.map((_, i) => `x${i}`).join('=>') + '=>' + `f${cn}` +
        ts.map((t, i) => t.equals(rtype) ? `(x${i})(para${n}${a.map(x => `(${x})`).join('')}(x${i}))` : `(x${i})`).join('')})`).join('')}`;
}
exports.compilePara = compilePara;
function compileDefinition(d, attachVars) {
    if (d instanceof definitions_1.DValue)
        return `${varPrefix(d.name, attachVars)} = ${compile(d.val)}`;
    if (d instanceof definitions_1.DData)
        return d.constrs.map(([n, ts]) => `${varPrefix(n, attachVars)} = ${compileConstructor(n, ts.length)}`).join(';') + ';' +
            `${varPrefix(`case${d.name}`, attachVars)} = ${compileCase(d.name, d.constrs)};` +
            (d.constrs.length === 1 && d.constrs[0][1].length === 1 ?
                `${varPrefix(`un${d.name}`, attachVars)} = case${d.name}(x => x);` : '') +
            `${varPrefix(`cata${d.name}`, attachVars)} = ${compileCata(d.name, d.constrs, d.getType())};` +
            `${varPrefix(`para${d.name}`, attachVars)} = ${compilePara(d.name, d.constrs, d.getType())};`;
    return util_1.impossible();
}
exports.compileDefinition = compileDefinition;
function compileProgram(p, withMain, lib = '', attachVars) {
    return `;${lib.trim()};${p.map(d => compileDefinition(d, attachVars)).join(';')}${withMain ? `;console.log(show(main))` : ''};`;
}
exports.compileProgram = compileProgram;

},{"./definitions":3,"./exprs":4,"./util":11}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
class ContextElem {
}
exports.ContextElem = ContextElem;
class CKCon extends ContextElem {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `kind ${this.name}`;
    }
}
exports.CKCon = CKCon;
exports.ckcon = (name) => new CKCon(name);
exports.isCKCon = (name) => (e) => e instanceof CKCon && e.name === name;
class CTCon extends ContextElem {
    constructor(name, kind) {
        super();
        this.name = name;
        this.kind = kind;
    }
    toString() {
        return `type ${this.name} : ${this.kind}`;
    }
}
exports.CTCon = CTCon;
exports.ctcon = (name, kind) => new CTCon(name, kind);
exports.isCTCon = (name) => (e) => e instanceof CTCon && e.name === name;
class CTVar extends ContextElem {
    constructor(name, kind) {
        super();
        this.name = name;
        this.kind = kind;
    }
    toString() {
        return `tvar ${this.name} : ${this.kind}`;
    }
}
exports.CTVar = CTVar;
exports.ctvar = (name, kind) => new CTVar(name, kind);
exports.isCTVar = (name) => (e) => e instanceof CTVar && e.name === name;
class CTEx extends ContextElem {
    constructor(name, kind) {
        super();
        this.name = name;
        this.kind = kind;
    }
    toString() {
        return `^${this.name} : ${this.kind}`;
    }
}
exports.CTEx = CTEx;
exports.ctex = (name, kind) => new CTEx(name, kind);
exports.isCTEx = (name) => (e) => e instanceof CTEx && e.name === name;
class CVar extends ContextElem {
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
exports.isCVar = (name) => (e) => e instanceof CVar && e.name === name;
class CSolved extends ContextElem {
    constructor(name, kind, type) {
        super();
        this.name = name;
        this.kind = kind;
        this.type = type;
    }
    toString() {
        return `^${this.name} : ${this.kind} = ${this.type}`;
    }
}
exports.CSolved = CSolved;
exports.csolved = (name, kind, type) => new CSolved(name, kind, type);
class CMarker extends ContextElem {
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
exports.isCMarker = (name) => (e) => e instanceof CMarker && e.name === name;
class Context {
    constructor(elems) {
        this.elems = elems;
    }
    toString() {
        return `[${this.elems.join(', ')}]`;
    }
    findIndex(fn) {
        const a = this.elems;
        const l = a.length;
        for (let i = 0; i < l; i++) {
            if (fn(a[i]))
                return i;
        }
        return -1;
    }
    find(fn) {
        const a = this.elems;
        const l = a.length;
        for (let i = 0; i < l; i++) {
            const r = fn(a[i]);
            if (r !== null)
                return r;
        }
        return null;
    }
    contains(fn) {
        return this.find(e => fn(e) ? true : null) !== null;
    }
    isComplete() {
        return !this.contains(e => e instanceof CTEx);
    }
    findVar(name) {
        return this.find(e => e instanceof CVar && e.name === name ? e.type : null);
    }
    findSolved(name) {
        return this.find(e => e instanceof CSolved && e.name === name ? e.type : null);
    }
    findTCon(name) {
        return this.find(e => e instanceof CTCon && e.name === name ? e.kind : null);
    }
    findTVar(name) {
        return this.find(e => e instanceof CTVar && e.name === name ? e.kind : null);
    }
    findEx(name) {
        return this.find(e => e instanceof CTEx && e.name === name ? e.kind : null);
    }
    findKCon(name) {
        return this.find(e => e instanceof CKCon && e.name === name ? true : null);
    }
    findMarker(name) {
        return this.find(e => e instanceof CMarker && e.name === name ? true : null);
    }
    findExOrSolved(name) {
        return this.findEx(name) || this.find(e => e instanceof CSolved && e.name === name ? e.kind : null);
    }
    add(...es) {
        return new Context(this.elems.concat(es.filter(x => x !== null)));
    }
    append(other) {
        return new Context(this.elems.concat(other.elems));
    }
    split(fn) {
        const i = this.findIndex(fn);
        return i < 0 ?
            { left: this, right: new Context([]) } :
            { left: new Context(this.elems.slice(0, i)), right: new Context(this.elems.slice(i + 1)) };
    }
    replace(fn, other) {
        const i = this.findIndex(fn);
        return i < 0 ? this : new Context(this.elems.slice(0, i).concat(other.elems, this.elems.slice(i + 1)));
    }
    removeAll(fn) {
        const r = [];
        const a = this.elems;
        const l = a.length;
        for (let i = 0; i < l; i++) {
            if (!fn(a[i]))
                r.push(a[i]);
        }
        return new Context(r);
    }
    isOrdered(a, b) {
        const ia = this.findIndex(e => e instanceof CTEx && e.name === a);
        const ib = this.findIndex(e => e instanceof CTEx && e.name === b);
        return ia < 0 || ib < 0 ? false : ia < ib;
    }
    kcons() {
        return this.elems.filter(e => e instanceof CKCon).map((e) => e.name);
    }
    tcons() {
        return this.elems.filter(e => e instanceof CTCon).map((e) => e.name);
    }
    vars() {
        return this.elems.filter(e => e instanceof CVar).map((e) => e.name);
    }
    tvars() {
        return this.elems.filter(e => e instanceof CTVar).map((e) => e.name);
    }
    texs() {
        return this.elems.filter(e => e instanceof CTEx || e instanceof CSolved)
            .map((e) => e.name);
    }
    unsolved() {
        return this.elems.filter(e => e instanceof CTEx).map((e) => [e.name, e.kind]);
    }
    apply(type) {
        if (type instanceof types_1.TEx) {
            const r = this.find(e => (e instanceof CTEx || e instanceof CSolved) && e.name === type.name ? e : null);
            return r === null ? type : r instanceof CSolved ? this.apply(r.type) : type;
        }
        if (type instanceof types_1.TFun)
            return types_1.tfun(this.apply(type.left), this.apply(type.right));
        if (type instanceof types_1.TExtend)
            return types_1.textend(type.label, this.apply(type.type), this.apply(type.rest));
        if (type instanceof types_1.TApp)
            return types_1.tapp(this.apply(type.left), this.apply(type.right));
        if (type instanceof types_1.TForall)
            return types_1.tforall(type.name, type.kind, this.apply(type.type));
        return type;
    }
    applyContextElem(e) {
        if (e instanceof CVar)
            return exports.cvar(e.name, this.apply(e.type));
        if (e instanceof CSolved)
            return exports.csolved(e.name, e.kind, this.apply(e.type));
        return e;
    }
    applyContext(context) {
        return new Context(context.elems.map(e => this.applyContextElem(e)));
    }
}
exports.Context = Context;

},{"./types":10}],3:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const typechecker_1 = require("./typechecker");
class Definition {
}
exports.Definition = Definition;
class DValue extends Definition {
    constructor(name, val, type) {
        super();
        this.name = name;
        this.val = val;
        this.type = type;
    }
    toString() {
        return `${this.name}${this.type ? ` : ${this.type}` : ''} = ${this.val}`;
    }
}
exports.DValue = DValue;
class DData extends Definition {
    constructor(name, params, constrs) {
        super();
        this.name = name;
        this.params = params;
        this.constrs = constrs;
    }
    toString() {
        return `data ${this.name} ${this.params.length === 0 ? '' : this.params.map(([x, k]) => `(${x} : ${k})`).join(' ')}${this.constrs.length === 0 ? '' : ` = ${this.constrs.map(([x, ts]) => `${x}${ts.length === 0 ? '' : ts.join(' ')}`).join(' | ')}`}`;
    }
    getKind() {
        return kinds_1.kfuns.apply(null, this.params.map(([n, k]) => k).concat([typechecker_1.ktype]));
    }
    getType() {
        return types_1.tapps.apply(null, [types_1.tcon(this.name)].concat(this.params.map(([x, _]) => types_1.tvar(x))));
    }
}
exports.DData = DData;

},{"./kinds":5,"./typechecker":9,"./types":10}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Expr {
}
exports.Expr = Expr;
class ELit extends Expr {
    constructor(val) {
        super();
        this.val = val;
    }
    toString() {
        return typeof this.val === 'string' ? JSON.stringify(this.val) : `${this.val}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.ELit = ELit;
exports.elit = (val) => new ELit(val);
class EVar extends Expr {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `${this.name}`;
    }
    subst(name, expr) {
        return this.name === name ? expr : this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EVar = EVar;
exports.evar = (name) => new EVar(name);
class EApp extends Expr {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} ${this.right})`;
    }
    subst(name, expr) {
        return new EApp(this.left.subst(name, expr), this.right.subst(name, expr));
    }
    substType(name, type) {
        return new EApp(this.left.substType(name, type), this.right.substType(name, type));
    }
}
exports.EApp = EApp;
exports.eapp = (left, right) => new EApp(left, right);
exports.eapps = (...es) => es.reduce(exports.eapp);
class EAbs extends Expr {
    constructor(name, type, expr) {
        super();
        this.name = name;
        this.type = type;
        this.expr = expr;
    }
    isAnnotated() {
        return !!this.type;
    }
    toString() {
        return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.expr})` : `(\\${this.name} -> ${this.expr})`;
    }
    subst(name, expr) {
        return this.name === name ? this : new EAbs(this.name, this.type, this.expr.subst(name, expr));
    }
    open(expr) {
        return this.expr.subst(this.name, expr);
    }
    substType(name, type) {
        return new EAbs(this.name, this.type && this.type.subst(name, type), this.expr.substType(name, type));
    }
}
exports.EAbs = EAbs;
exports.eabs = (name, expr) => new EAbs(name, null, expr);
exports.eabst = (name, type, expr) => new EAbs(name, type, expr);
exports.eabss = (ns, expr) => ns.reduceRight((a, b) => typeof b === 'string' ? exports.eabs(b, a) : exports.eabst(b[0], b[1], a), expr);
class EAnno extends Expr {
    constructor(expr, type) {
        super();
        this.expr = expr;
        this.type = type;
    }
    toString() {
        return `(${this.expr} : ${this.type})`;
    }
    subst(name, expr) {
        return new EAnno(this.expr.subst(name, expr), this.type);
    }
    substType(name, type) {
        return new EAnno(this.expr.substType(name, type), this.type.subst(name, type));
    }
}
exports.EAnno = EAnno;
exports.eanno = (expr, type) => new EAnno(expr, type);
class ETAbs extends Expr {
    constructor(name, kind, expr) {
        super();
        this.name = name;
        this.kind = kind;
        this.expr = expr;
    }
    toString() {
        return `(/\\(${this.name} : ${this.kind}) -> ${this.expr})`;
    }
    subst(name, expr) {
        return new ETAbs(this.name, this.kind, this.expr.subst(name, expr));
    }
    substType(name, type) {
        return this.name === name ? this : new ETAbs(this.name, this.kind, this.expr.substType(name, type));
    }
}
exports.ETAbs = ETAbs;
exports.etabs = (name, kind, expr) => new ETAbs(name, kind, expr);
exports.etabss = (ns, expr) => ns.reduceRight((a, b) => exports.etabs(b[0], b[1], a), expr);
class ETApp extends Expr {
    constructor(expr, type) {
        super();
        this.expr = expr;
        this.type = type;
    }
    toString() {
        return `(${this.expr} @${this.type})`;
    }
    subst(name, expr) {
        return new ETApp(this.expr.subst(name, expr), this.type);
    }
    substType(name, type) {
        return new ETApp(this.expr.substType(name, type), this.type.subst(name, type));
    }
}
exports.ETApp = ETApp;
exports.etapp = (expr, type) => new ETApp(expr, type);
exports.etapps = (expr, ...ts) => ts.reduce(exports.etapp, expr);
class EEmpty extends Expr {
    toString() {
        return `{}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EEmpty = EEmpty;
exports.eempty = new EEmpty;
class EExtend extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `.+${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EExtend = EExtend;
exports.eextend = (label) => new EExtend(label);
class ESelect extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `.${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.ESelect = ESelect;
exports.eselect = (label) => new ESelect(label);
class ERestrict extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `.-${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.ERestrict = ERestrict;
exports.erestrict = (label) => new ERestrict(label);
class ERecUpdate extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `.:${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.ERecUpdate = ERecUpdate;
exports.erecupdate = (label) => new ERecUpdate(label);
class EVarEmpty extends Expr {
    toString() {
        return `varEmpty`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EVarEmpty = EVarEmpty;
exports.evarempty = new EVarEmpty;
class EInject extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `!${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EInject = EInject;
exports.einject = (label) => new EInject(label);
class EEmbed extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `!+${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EEmbed = EEmbed;
exports.eembed = (label) => new EEmbed(label);
class ECase extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `?${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.ECase = ECase;
exports.ecase = (label) => new ECase(label);
class EVarUpdate extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `!:${this.label}`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EVarUpdate = EVarUpdate;
exports.evarupdate = (label) => new EVarUpdate(label);
class EReturn extends Expr {
    toString() {
        return `return`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EReturn = EReturn;
exports.ereturn = new EReturn();
class EPure extends Expr {
    toString() {
        return `pure`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EPure = EPure;
exports.epure = new EPure();
class EOp extends Expr {
    constructor(label) {
        super();
        this.label = label;
    }
    toString() {
        return `(perform ${this.label})`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EOp = EOp;
exports.eop = (label) => new EOp(label);
class EDo extends Expr {
    toString() {
        return `do`;
    }
    subst(name, expr) {
        return this;
    }
    substType(name, type) {
        return this;
    }
}
exports.EDo = EDo;
exports.edo = new EDo();
class EHandler extends Expr {
    constructor(map) {
        super();
        this.map = map;
    }
    toString() {
        return `(handler { ${this.map.map(([op, h]) => `${op} -> ${h}`).join(', ')} })`;
    }
    subst(name, expr) {
        return new EHandler(this.map.map(([op, e]) => [op, e.subst(name, expr)]));
    }
    substType(name, type) {
        return new EHandler(this.map.map(([op, e]) => [op, e.substType(name, type)]));
    }
}
exports.EHandler = EHandler;
exports.ehandler = (map) => new EHandler(map);

},{}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Kind {
}
exports.Kind = Kind;
class KCon extends Kind {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `${this.name}`;
    }
    equals(other) {
        return other instanceof KCon && this.name === other.name;
    }
}
exports.KCon = KCon;
exports.kcon = (name) => new KCon(name);
class KFun extends Kind {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} -> ${this.right})`;
    }
    equals(other) {
        return other instanceof KFun && this.left.equals(other.left) && this.right.equals(other.right);
    }
}
exports.KFun = KFun;
exports.kfun = (left, right) => new KFun(left, right);
exports.kfuns = (...ts) => ts.reduceRight((a, b) => exports.kfun(b, a));

},{}],6:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const typechecker_1 = require("./typechecker");
const definitions_1 = require("./definitions");
const prettyprinter_1 = require("./prettyprinter");
function matchingBracket(c) {
    if (c === '(')
        return ')';
    if (c === ')')
        return '(';
    return '';
}
const token = (val) => ({ tag: 'token', val });
const paren = (val) => ({ tag: 'paren', val });
function tokenize(s) {
    const START = 0, NAME = 1, STR = 2, NUM = 3;
    let state = START;
    let r = [], p = [], b = [];
    let t = '';
    let escape = false;
    for (let i = 0; i <= s.length; i++) {
        const c = s[i] || ' ';
        if (state === START) {
            if (/[a-z]/i.test(c))
                t += c, state = NAME;
            else if (/[0-9]/.test(c))
                t += c, state = NUM;
            else if (c === '"')
                state = STR;
            else if (c === '-' && s[i + 1] === '>')
                r.push(token('->')), i++;
            else if (c === '/' && s[i + 1] === '\\')
                r.push(token('/\\')), i++;
            else if (c === '@')
                r.push(token('@'));
            else if (c === '$')
                r.push(token('$'));
            else if (c === ':')
                r.push(token(':'));
            else if (c === prettyprinter_1.FORALL)
                r.push(token('forall'));
            else if (c === '.')
                r.push(token('.'));
            else if (c === '=')
                r.push(token('='));
            else if (c === '|')
                r.push(token('|'));
            else if (c === '\\')
                r.push(token('\\'));
            else if (c === '(')
                b.push(c), p.push(r), r = [];
            else if (c === ')') {
                if (b.length === 0)
                    throw new SyntaxError(`unmatched bracket: ${c}`);
                const br = b.pop();
                if (matchingBracket(br) !== c)
                    throw new SyntaxError(`unmatched bracket: ${br} and ${c}`);
                const a = p.pop();
                a.push(paren(r));
                r = a;
            }
            else if (/\s+/.test(c))
                continue;
            else
                throw new SyntaxError(`invalid char: ${c}`);
        }
        else if (state === NAME) {
            if (!/[a-z0-9]/i.test(c))
                r.push(token(t)), t = '', i--, state = START;
            else
                t += c;
        }
        else if (state === NUM) {
            if (!/[0-9\.]/i.test(c))
                r.push(token(t)), t = '', i--, state = START;
            else
                t += c;
        }
        else if (state === STR) {
            if (escape)
                t += c, escape = false;
            else if (c === '\\')
                escape = true;
            else if (c === '"')
                r.push(token(`"${t}`)), t = '', state = START;
            else
                t += c;
        }
    }
    if (state !== START)
        throw new SyntaxError(`invalid parsing state: ${state}`);
    return r;
}
function parse(s) {
    return exprs(tokenize(s));
}
exports.parse = parse;
function isToken(x, n) {
    return x.tag === 'token' && x.val === n;
}
function containsToken(x, n) {
    return x.filter(x => x.tag === 'token').map(x => x.val).indexOf(n) >= 0;
}
function splitOn(x, f) {
    const r = [];
    let c = [];
    for (let i = 0; i < x.length; i++) {
        if (f(x[i])) {
            r.push(c);
            c = [];
        }
        else
            c.push(x[i]);
    }
    r.push(c);
    return r;
}
function exprs(x) {
    if (x.length === 0)
        return exprs_1.evar('Unit');
    if (x.length === 1)
        return expr(x[0]);
    if (containsToken(x, ':')) {
        const s = splitOn(x, x => isToken(x, ':'));
        if (s.length !== 2)
            throw new SyntaxError('nested anno :');
        s.forEach(x => x.length === 0 ? (() => { throw new SyntaxError('invalid anno :'); })() : null);
        const l = exprs(s[0]);
        const r = types(s[1]);
        return exprs_1.eanno(l, r);
    }
    if (isToken(x[0], 'handler')) {
        const args = [];
        for (let i = 1; i < x.length; i += 2) {
            const op = x[i];
            if (op.tag !== 'token')
                throw new SyntaxError(`invalid op in handler`);
            const e = expr(x[i + 1]);
            args.push([op.val, e]);
        }
        return exprs_1.ehandler(args);
    }
    if (isToken(x[0], '\\')) {
        const args = [];
        let found = -1;
        for (let i = 1; i < x.length; i++) {
            const c = x[i];
            if (isToken(c, '->')) {
                found = i;
                break;
            }
            else if (c.tag === 'token')
                args.push(c.val);
            else if (c.tag === 'paren' && c.val.length === 0)
                args.push(['_', type(c)]);
            else if (c.tag === 'paren' && containsToken(c.val, ':')) {
                const s = splitOn(c.val, x => isToken(x, ':'));
                if (s.length !== 2)
                    throw new SyntaxError('nested anno arg :');
                const l = s[0].map(x => {
                    if (x.tag === 'token')
                        return x.val;
                    throw new SyntaxError(`invalid arg: ${x}`);
                });
                const r = types(s[1]);
                l.forEach(n => args.push([n, r]));
            }
            else
                throw new SyntaxError(`invalid arg: ${c}`);
        }
        if (found < 0)
            throw new SyntaxError(`missing -> after \\`);
        const rest = x.slice(found + 1);
        if (rest.length === 0)
            throw new SyntaxError(`missing body in function`);
        return exprs_1.eabss(args, exprs(rest));
    }
    if (isToken(x[0], '/\\')) {
        const args = [];
        let found = -1;
        for (let i = 1; i < x.length; i++) {
            const c = x[i];
            if (isToken(c, '->')) {
                found = i;
                break;
            }
            else if (c.tag === 'token')
                args.push(c.val);
            else if (c.tag === 'paren' && containsToken(c.val, ':')) {
                const s = splitOn(c.val, x => isToken(x, ':'));
                if (s.length !== 2)
                    throw new SyntaxError('nested anno arg :');
                const l = s[0].map(x => {
                    if (x.tag === 'token')
                        return x.val;
                    throw new SyntaxError(`invalid arg: ${x}`);
                });
                const r = kinds(s[1]);
                l.forEach(n => args.push([n, r]));
            }
            else
                throw new SyntaxError(`invalid arg to tabs: ${c}`);
        }
        if (found < 0)
            throw new SyntaxError(`missing -> after /\\`);
        const rest = x.slice(found + 1);
        if (rest.length === 0)
            throw new SyntaxError(`missing body in tabs`);
        return exprs_1.etabss(args.map(x => typeof x === 'string' ? [x, typechecker_1.ktype] : x), exprs(rest));
    }
    if (containsToken(x, '$')) {
        const s = splitOn(x, x => isToken(x, '$'));
        s.forEach(x => x.length === 0 ? (() => { throw new SyntaxError('invalid application with $'); })() : null);
        if (s.length < 2)
            throw new SyntaxError('$ is missing an argument');
        return s.map(exprs).reduceRight((a, b) => exprs_1.eapp(b, a));
    }
    if (isToken(x[0], '@'))
        throw new SyntaxError('beginning @');
    let r = expr(x[0]);
    let next = false;
    for (let i = 1; i < x.length; i++) {
        const c = x[i];
        if (isToken(c, '@'))
            next = true;
        else if (next) {
            next = false;
            r = exprs_1.etapp(r, type(c));
        }
        else {
            r = exprs_1.eapp(r, expr(c));
        }
    }
    if (next)
        throw new SyntaxError('trailing @');
    return r;
}
function expr(x) {
    if (x.tag === 'token') {
        if (x.val === 'empty')
            return exprs_1.eempty;
        if (x.val === 'varempty')
            return exprs_1.evarempty;
        if (x.val === 'return')
            return exprs_1.ereturn;
        if (x.val === 'pure')
            return exprs_1.epure;
        if (x.val === 'do')
            return exprs_1.edo;
        if (x.val.startsWith('ext') && x.val.length > 3)
            return exprs_1.eextend(x.val.slice(3));
        if (x.val.startsWith('sel') && x.val.length > 3)
            return exprs_1.eselect(x.val.slice(3));
        if (x.val.startsWith('res') && x.val.length > 3)
            return exprs_1.erestrict(x.val.slice(3));
        if (x.val.startsWith('rupd') && x.val.length > 4)
            return exprs_1.erecupdate(x.val.slice(4));
        if (x.val.startsWith('inj') && x.val.length > 3)
            return exprs_1.einject(x.val.slice(3));
        if (x.val.startsWith('emb') && x.val.length > 3)
            return exprs_1.eembed(x.val.slice(3));
        if (x.val.startsWith('cs') && x.val.length > 2)
            return exprs_1.ecase(x.val.slice(2));
        if (x.val.startsWith('vupd') && x.val.length > 4)
            return exprs_1.evarupdate(x.val.slice(4));
        if (x.val.startsWith('op') && x.val.length > 2)
            return exprs_1.eop(x.val.slice(2));
        if (x.val[0] === '"')
            return exprs_1.elit(x.val.slice(1));
        const n = +x.val;
        if (!isNaN(n)) {
            if (x.val.indexOf('.') >= 0)
                return exprs_1.elit(n);
            if (n < 0)
                throw new SyntaxError(`invalid nat: ${n}`);
            let t = exprs_1.evar('Z');
            for (let i = 0; i < n; i++) {
                t = exprs_1.eapp(exprs_1.evar('S'), t);
            }
            return t;
        }
        else
            return exprs_1.evar(x.val);
    }
    return exprs(x.val);
}
function types(x) {
    if (x.length === 0)
        return types_1.tcon('Unit');
    if (x.length === 1)
        return type(x[0]);
    if (isToken(x[0], 'forall')) {
        const args = [];
        let found = -1;
        for (let i = 1; i < x.length; i++) {
            const c = x[i];
            if (isToken(c, '.')) {
                found = i;
                break;
            }
            else if (c.tag === 'token')
                args.push(c.val);
            else if (c.tag === 'paren' && containsToken(c.val, ':')) {
                const s = splitOn(c.val, x => isToken(x, ':'));
                if (s.length !== 2)
                    throw new SyntaxError('nested anno arg :');
                const l = s[0].map(x => {
                    if (x.tag === 'token')
                        return x.val;
                    throw new SyntaxError(`invalid arg: ${x}`);
                });
                const r = kinds(s[1]);
                l.forEach(n => args.push([n, r]));
            }
            else
                throw new SyntaxError(`invalid arg to forall: ${c}`);
        }
        if (found < 0)
            throw new SyntaxError(`missing . after forall`);
        const rest = x.slice(found + 1);
        if (rest.length === 0)
            throw new SyntaxError(`missing body in forall`);
        return types_1.tforalls(args.map(x => typeof x === 'string' ? [x, typechecker_1.ktype] : x), types(rest));
    }
    if (containsToken(x, '->'))
        return types_1.tfuns.apply(null, splitOn(x, x => isToken(x, '->')).map(types));
    return types_1.tapps.apply(null, x.map(type));
}
function type(x) {
    if (x.tag === 'token')
        return /[a-z]/.test(x.val[0]) ? types_1.tvar(x.val) : types_1.tcon(x.val);
    return types(x.val);
}
function kinds(x) {
    if (x.length === 0)
        throw new SyntaxError('kind () is not allowed');
    if (x.length === 1)
        return kind(x[0]);
    const t = splitOn(x, x => isToken(x, '->')).map(kinds);
    if (t.length === 0)
        throw new SyntaxError(`invalid kind syntax`);
    return kinds_1.kfuns.apply(null, t);
}
function kind(x) {
    if (x.tag === 'token')
        return kinds_1.kcon(x.val);
    return kinds(x.val);
}
// definitions
function parseDataName(s) {
    if (s.length === 0)
        throw new SyntaxError('missing data name');
    if (s[0].tag !== 'token')
        throw new SyntaxError('invalid data name');
    const name = s[0].val;
    const args = [];
    for (let i = 1; i < s.length; i++) {
        const c = s[i];
        if (c.tag === 'token')
            args.push([c.val, typechecker_1.ktype]);
        else if (c.tag === 'paren' && containsToken(c.val, ':')) {
            const s = splitOn(c.val, x => isToken(x, ':'));
            if (s.length !== 2)
                throw new SyntaxError('nested anno arg :');
            const l = s[0].map(x => {
                if (x.tag === 'token')
                    return x.val;
                throw new SyntaxError(`invalid arg to data: ${x}`);
            });
            const r = kinds(s[1]);
            l.forEach(n => args.push([n, r]));
        }
        else
            throw new SyntaxError(`invalid arg to data: ${c}`);
    }
    return [name, args];
}
function parseConstr(s) {
    if (s.length === 0)
        throw new SyntaxError('missing constructor name in data');
    if (s[0].tag !== 'token')
        throw new SyntaxError('invalid data constructor name');
    const name = s[0].val;
    return [name, s.slice(1).map(type)];
}
function parseDefinition(s) {
    if (s.startsWith('data ')) {
        const ts = tokenize(s.slice(5));
        if (ts.length === 0)
            throw new SyntaxError('data name missing');
        if (ts[0].tag !== 'token')
            throw new SyntaxError('invalid data name');
        const name = ts[0].val;
        if (ts.length === 1)
            return new definitions_1.DData(name, [], []);
        if (containsToken(ts, '=')) {
            const spl = splitOn(ts, x => isToken(x, '='));
            if (spl.length !== 2)
                throw new SyntaxError('missing right side of = in data');
            const dataName = parseDataName(spl[0]);
            const constr = splitOn(spl[1], x => isToken(x, '|')).map(parseConstr);
            return new definitions_1.DData(dataName[0], dataName[1], constr);
        }
        else
            throw new SyntaxError('= is missing in data');
    }
    else {
        const spl = s.split('=');
        if (!spl || spl.length !== 2)
            throw new SyntaxError('error on =');
        const name = spl[0].trim();
        if (!/[a-z][A-Z0-9a-z]*/.test(name))
            throw new SyntaxError(`invalid name: ${name}`);
        const rest = spl[1].trim();
        return new definitions_1.DValue(name, parse(rest));
    }
}
exports.parseDefinition = parseDefinition;
function parseProgram(s) {
    return s.split(';').filter(x => x.trim().length > 0).map(x => parseDefinition(x.trim()));
}
exports.parseProgram = parseProgram;

},{"./definitions":3,"./exprs":4,"./kinds":5,"./prettyprinter":7,"./typechecker":9,"./types":10}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const context_1 = require("./context");
const util_1 = require("./util");
const typechecker_1 = require("./typechecker");
const RARROW = ' -> ';
exports.FORALL = '\u2200';
// Kinds
function flattenKFun(f) {
    const r = [];
    let c = f;
    while (c instanceof kinds_1.KFun) {
        r.push(c.left);
        c = c.right;
    }
    r.push(c);
    return r;
}
function ppKind(k) {
    if (k instanceof kinds_1.KCon)
        return `${k.name}`;
    if (k instanceof kinds_1.KFun)
        return flattenKFun(k).map(k => k instanceof kinds_1.KFun ? `(${ppKind(k)})` : ppKind(k)).join(`${RARROW}`);
    return util_1.impossible();
}
exports.ppKind = ppKind;
// Types
function flattenTFun(f) {
    const r = [];
    let c = f;
    while (c instanceof types_1.TFun) {
        r.push(c.left);
        c = c.right;
    }
    r.push(c);
    return r;
}
function flattenTForall(f) {
    const r = [];
    let c = f;
    while (c instanceof types_1.TForall) {
        r.push([c.name, c.kind]);
        c = c.type;
    }
    return { args: r, ty: c };
}
function flattenTApp(a) {
    const r = [];
    let c = a;
    while (c instanceof types_1.TApp) {
        r.push(c.right);
        c = c.left;
    }
    r.push(c);
    return r.reverse();
}
function flattenTExtend(e) {
    const props = [];
    let c = e;
    while (c instanceof types_1.TExtend) {
        props.push([c.label, c.type]);
        c = c.rest;
    }
    return { props, rest: c instanceof types_1.TEmpty ? null : c };
}
function isSymbol(n) {
    return !/[a-z]/i.test(n[0]);
}
function ppType(t) {
    if (t instanceof types_1.TEmpty)
        return `{}`;
    if (t instanceof types_1.TCon)
        return `${t.name}`;
    if (t instanceof types_1.TVar)
        return `${t.name}`;
    if (t instanceof types_1.TEx)
        return `^${t.name}`;
    if (t instanceof types_1.TApp) {
        const f = flattenTApp(t);
        const first = f[0];
        if (first instanceof types_1.TCon && isSymbol(first.name) && f.length === 3) {
            const args = f.slice(1).map(t => t instanceof types_1.TApp || t instanceof types_1.TFun || t instanceof types_1.TForall ? `(${ppType(t)})` : ppType(t));
            return `${args[0]} ${first.name} ${args[1]}`;
        }
        return f.map(t => t instanceof types_1.TApp || t instanceof types_1.TFun || t instanceof types_1.TForall ? `(${ppType(t)})` : ppType(t)).join(` `);
    }
    if (t instanceof types_1.TFun)
        return flattenTFun(t).map(t => t instanceof types_1.TFun || t instanceof types_1.TForall ? `(${ppType(t)})` : ppType(t)).join(`${RARROW}`);
    if (t instanceof types_1.TForall) {
        const f = flattenTForall(t);
        const args = f.args.map(([x, k]) => k.equals(typechecker_1.ktype) ? x : `(${x} : ${ppKind(k)})`);
        return `${exports.FORALL}${args.join(' ')}. ${ppType(f.ty)}`;
    }
    if (t instanceof types_1.TExtend) {
        const f = flattenTExtend(t);
        return `{ ${f.props.map(([l, t]) => `${l} : ${ppType(t)}`).join(', ')} ${f.rest ? `| ${ppType(f.rest)} ` : ''}}`;
    }
    return util_1.impossible();
}
exports.ppType = ppType;
// ContextElem
function ppContextElem(e) {
    if (e instanceof context_1.CKCon)
        return `kind ${e.name}`;
    if (e instanceof context_1.CTCon)
        return `type ${e.name} : ${ppKind(e.kind)}`;
    if (e instanceof context_1.CTVar)
        return `tvar ${e.name} : ${ppKind(e.kind)}`;
    if (e instanceof context_1.CTEx)
        return `tex ^${e.name} : ${ppKind(e.kind)}`;
    if (e instanceof context_1.CVar)
        return `${e.name} : ${ppType(e.type)}`;
    if (e instanceof context_1.CSolved)
        return `^${e.name} : ${ppKind(e.kind)} = ${ppType(e.type)}`;
    if (e instanceof context_1.CMarker)
        return `|>^${e.name}`;
    return util_1.impossible();
}
exports.ppContextElem = ppContextElem;
function ppContext(c) {
    return `[${c.elems.map(ppContextElem).join(', ')}]`;
}
exports.ppContext = ppContext;

},{"./context":2,"./kinds":5,"./typechecker":9,"./types":10,"./util":11}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const compilerJS_1 = require("./compilerJS");
const typechecker_1 = require("./typechecker");
const context_1 = require("./context");
const parser_1 = require("./parser");
const prettyprinter_1 = require("./prettyprinter");
const definitions_1 = require("./definitions");
exports._context = typechecker_1.initialContext.add(context_1.cvar('show', types_1.tforalls([['t', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('t'), typechecker_1.tstr))), context_1.cvar('emptyStr', typechecker_1.tstr), context_1.cvar('appendStr', types_1.tfuns(typechecker_1.tstr, typechecker_1.tstr, typechecker_1.tstr)), context_1.cvar('zeroFloat', typechecker_1.tfloat), context_1.cvar('oneFloat', typechecker_1.tfloat), context_1.cvar('negFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('incFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('decFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('addFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('subFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('mulFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('divFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat, typechecker_1.tfloat)), context_1.cvar('modFloat', types_1.tfuns(typechecker_1.tfloat, typechecker_1.tfloat, typechecker_1.tfloat)));
function _show(x) {
    if (x._rec) {
        if (x.length === 0)
            return '{}';
        const r = [];
        for (let i = x.length - 1; i >= 0; i--)
            r.push(`${x[i][0]} = ${_show(x[i][1])}`);
        return `{ ${r.join(', ')} }`;
    }
    if (x._eff) {
        if (x.tag === 'ret')
            return `!(${_show(x.val)})`;
        if (x.tag === 'cont')
            return `!(${x.op} ${_show(x.val)})`;
    }
    if (x._var) {
        let l = '';
        for (let i = 0; i < x.level; i++)
            l += '^';
        return `(${l}${x.label} ${_show(x.val)})`;
    }
    if (x._adt) {
        if (x._tag === 'Z')
            return '0';
        if (x._tag === 'S') {
            let c = x;
            let n = 0;
            while (c._tag === 'S') {
                n++;
                c = c._args[0];
            }
            return `${n}`;
        }
        if (x._tag === 'Nil')
            return '[]';
        if (x._tag === 'Cons') {
            let c = x;
            let r = [];
            while (c._tag === 'Cons') {
                r.push(c._args[0]);
                c = c._args[1];
            }
            return '[' + r.map(_show).join(', ') + ']';
        }
        return x._args.length === 0 ? `${x._tag}` : `(${x._tag}${x._args.length > 0 ? ` ${x._args.map(_show).join(' ')}` : ''})`;
    }
    if (typeof x === 'function')
        return `[Function]`;
    if (typeof x === 'string')
        return JSON.stringify(x);
    return `${x}`;
}
let _ctx = exports._context;
function _run(i, cb) {
    const cmd = i.trim().toLowerCase();
    if (cmd === ':help' || cmd === ':h') {
        cb('commands :help :context :def :prelude');
    }
    else if (cmd === ':prelude' || cmd === ':p') {
        try {
            const ds = parser_1.parseProgram(eval('_prelude'));
            const t = typechecker_1.inferProgram(_ctx, ds);
            const c = compilerJS_1.compileProgram(t.defs, false, '', true);
            console.log(c);
            eval(c);
            _ctx = t.ctx;
            cb('prelude loaded');
        }
        catch (err) {
            return cb('' + err, true);
        }
    }
    else if (cmd === ':context' || cmd === ':c') {
        cb(_ctx.elems.map(prettyprinter_1.ppContextElem).join('\n'));
    }
    else if (cmd.slice(0, 4) === ':def') {
        const rest = i.slice(4).trim();
        try {
            const d_ = parser_1.parseDefinition(rest);
            const t = typechecker_1.inferDefinition(_ctx, d_);
            _ctx = t.ctx;
            const d = t.def;
            const res = eval(compilerJS_1.compileDefinition(d, true));
            if (d instanceof definitions_1.DValue)
                cb(`${d.name} : ${prettyprinter_1.ppType(_ctx.apply(_ctx.findVar(d.name)))} = ${_show(res)}`);
            else if (d instanceof definitions_1.DData)
                cb(`defined ${d.name}`);
            else
                return cb('unknown definition', true);
        }
        catch (err) {
            return cb('' + err, true);
        }
    }
    else {
        try {
            const p = parser_1.parse(i);
            console.log('' + p);
            const tr = typechecker_1.infer(_ctx, p);
            console.log(prettyprinter_1.ppType(tr.ty));
            console.log('' + tr.expr);
            const c = compilerJS_1.compile(tr.expr);
            console.log(c);
            const res = eval(c);
            cb(`${_show(res)} : ${prettyprinter_1.ppType(tr.ty)}`);
        }
        catch (e) {
            cb('' + e, true);
        }
    }
}
exports.default = _run;

},{"./compilerJS":1,"./context":2,"./definitions":3,"./parser":6,"./prettyprinter":7,"./typechecker":9,"./types":10}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const types_1 = require("./types");
const context_1 = require("./context");
const exprs_1 = require("./exprs");
const kinds_1 = require("./kinds");
const definitions_1 = require("./definitions");
// errors
const err = (msg) => { throw new TypeError(msg); };
const check = (b, m) => b ? null : err(m);
function noDups(d) {
    const o = {};
    for (let i = 0; i < d.length; i++) {
        if (o[d[i]])
            return err(`duplicate ${d[i]}`);
        o[d[i]] = true;
    }
    return null;
}
// fresh
const fresh = (ns, n) => {
    const l = ns.length;
    for (let i = 0; i < l; i++) {
        if (ns[i] === n) {
            const m = n.split('').reverse().join('').match(/^([0-9]+).+$/i);
            if (!m) {
                return fresh(ns, `${n}0`);
            }
            else {
                const jr = m[1];
                const jl = jr.length;
                const j = +(jr.split('').reverse().join(''));
                return fresh(ns, `${n.slice(0, -jl)}${j + 1}`);
            }
        }
    }
    return n;
};
// util
const findVar = (ctx, name) => {
    const r = ctx.findVar(name);
    return r === null ? err(`var ${name} not found in ${ctx}`) : (r);
};
const findSolved = (ctx, name) => {
    const r = ctx.findSolved(name);
    return r === null ? err(`var ${name} not found in ${ctx}`) : (r);
};
const findKCon = (ctx, name) => {
    return ctx.findKCon(name) === null ? err(`tcon ${name} not found in ${ctx}`) : (null);
};
const findTCon = (ctx, name) => {
    const r = ctx.findTCon(name);
    return r === null ? err(`tcon ${name} not found in ${ctx}`) : (r);
};
const findTVar = (ctx, name) => {
    const r = ctx.findTVar(name);
    return r === null ? err(`tvar ${name} not found in ${ctx}`) : (r);
};
const findEx = (ctx, name) => {
    const r = ctx.findEx(name);
    return r === null ? err(`ex ^${name} not found in ${ctx}`) : (r);
};
const findMarker = (ctx, name) => {
    return ctx.findMarker(name) === null ? err(`marker |>${name} not found in ${ctx}`) : (null);
};
const findExOrSolved = (ctx, name) => {
    const r = ctx.findExOrSolved(name);
    return r === null ? err(`ex or solved ^${name} not found in ${ctx}`) : (r);
};
const orderedTExs = (texs, ty) => {
    const o = ty.texs();
    const r = [];
    const texsn = texs.map(([n, _]) => n);
    for (let i = 0; i < o.length; i++) {
        const c = o[i];
        if (texsn.indexOf(c) >= 0 && r.indexOf(c) < 0)
            r.push(c);
    }
    return r.map(n => texs[texsn.indexOf(n)]);
};
// initial context
exports.ktype = kinds_1.kcon('Type');
exports.krow = kinds_1.kcon('Row');
exports.tstr = types_1.tcon('Str');
exports.tfloat = types_1.tcon('Float');
exports.tsrec = types_1.tcon('SRec');
exports.tsvar = types_1.tcon('SVar');
exports.tseff = types_1.tcon('SEff');
exports.initialContext = new context_1.Context([
    context_1.ckcon('Type'),
    context_1.ckcon('Row'),
    context_1.ctcon('Str', exports.ktype),
    context_1.ctcon('Float', exports.ktype),
    context_1.ctcon('SRec', kinds_1.kfuns(exports.krow, exports.ktype)),
    context_1.ctcon('SVar', kinds_1.kfuns(exports.krow, exports.ktype)),
    context_1.ctcon('SEff', kinds_1.kfuns(exports.krow, exports.ktype, exports.ktype)),
]);
// wf
function checkKindType(kind) {
    return exports.ktype.equals(kind) ? (null) : err(`kind is not ${exports.ktype}: ${kind}`);
}
function checkKindRow(kind) {
    return exports.krow.equals(kind) ? (null) : err(`kind is not ${exports.krow}: ${kind}`);
}
function kindWF(ctx, kind) {
    // console.log(`kindWF ${kind} in ${ctx}`);
    if (kind instanceof kinds_1.KCon)
        return findKCon(ctx, kind.name);
    if (kind instanceof kinds_1.KFun) {
        kindWF(ctx, kind.left);
        return kindWF(ctx, kind.right);
    }
    return util_1.impossible();
}
function typeWF(ctx, ty) {
    //console.log(`typeWF ${ty} in ${ctx}`);
    if (ty instanceof types_1.TEmpty) {
        return exports.krow;
    }
    if (ty instanceof types_1.TCon) {
        const k = findTCon(ctx, ty.name);
        kindWF(ctx, k);
        return k;
    }
    if (ty instanceof types_1.TVar) {
        const k = findTVar(ctx, ty.name);
        kindWF(ctx, k);
        return k;
    }
    if (ty instanceof types_1.TEx) {
        const k = findExOrSolved(ctx, ty.name);
        kindWF(ctx, k);
        return k;
    }
    if (ty instanceof types_1.TFun) {
        const k1 = typeWF(ctx, ty.left);
        checkKindType(k1);
        const k2 = typeWF(ctx, ty.right);
        checkKindType(k2);
        return exports.ktype;
    }
    if (ty instanceof types_1.TExtend) {
        const k1 = typeWF(ctx, ty.type);
        checkKindType(k1);
        const k2 = typeWF(ctx, ty.rest);
        checkKindRow(k2);
        return exports.krow;
    }
    if (ty instanceof types_1.TForall) {
        kindWF(ctx, ty.kind);
        return typeWF(ctx.add(context_1.ctvar(ty.name, ty.kind)), ty.type);
    }
    if (ty instanceof types_1.TApp) {
        const kleft = typeWF(ctx, ty.left);
        if (kleft instanceof kinds_1.KFun) {
            const kright = typeWF(ctx, ty.right);
            if (!kright.equals(kleft.left))
                return err(`kind mismatch in type constructor: ${ty} in ${ctx}`);
            return (kleft.right);
        }
        else
            return err(`not a type constructor: ${ty} in ${ctx}`);
    }
    return util_1.impossible();
}
function contextWF(ctx) {
    // console.log(`contextWF ${ctx}`);
    const a = ctx.elems;
    const l = a.length;
    for (let i = 0; i < l; i++) {
        const e = a[i];
        const p = new context_1.Context(a.slice(0, i));
        if (e instanceof context_1.CKCon) {
            if (p.findKCon(e.name) !== null)
                return err(`duplicate kcon ^${e.name}`);
        }
        else if (e instanceof context_1.CTCon) {
            if (p.findTCon(e.name) !== null)
                return err(`duplicate tcon ${e.name}`);
            kindWF(p, e.kind);
        }
        else if (e instanceof context_1.CTVar) {
            if (p.findTVar(e.name) !== null)
                return err(`duplicate tvar ${e.name}`);
            kindWF(p, e.kind);
        }
        else if (e instanceof context_1.CTEx || e instanceof context_1.CSolved) {
            if (p.findExOrSolved(e.name) !== null)
                return err(`duplicate tex ^${e.name}`);
            kindWF(p, e.kind);
            if (e instanceof context_1.CSolved) {
                const k = typeWF(p, e.type);
                if (!k.equals(e.kind))
                    err(`solved with invalid kind: ${e} in ${p}`);
            }
        }
        else if (e instanceof context_1.CVar) {
            if (p.findVar(e.name) !== null)
                return err(`duplicate var ${e.name}`);
            const k = typeWF(p, e.type);
            checkKindType(k);
        }
        else if (e instanceof context_1.CMarker) {
            if (p.findMarker(e.name) !== null || p.findExOrSolved(e.name) !== null)
                return err(`duplicate marker ^${e.name}`);
        }
        else
            return util_1.impossible();
    }
    return null;
}
// subtype
function rowTail(t) {
    if (t instanceof types_1.TEx)
        return t.name;
    if (t instanceof types_1.TEmpty)
        return null;
    if (t instanceof types_1.TExtend)
        return rowTail(t.rest);
    return err(`not a row type in rowTail: ${t}`);
}
function rewriteRow(ctx, l, ty) {
    if (ty instanceof types_1.TEmpty)
        err(`${l} cannot be inserted in ${ctx}`);
    if (ty instanceof types_1.TExtend) {
        const rest = ty.rest;
        if (l === ty.label)
            return { ctx, ty: ty.type, rest, ex: null };
        if (rest instanceof types_1.TEx) {
            const texs = ctx.texs();
            const tt = fresh(texs, 't');
            const tr = fresh(texs.concat([tt]), 'r');
            return {
                ctx: ctx.replace(context_1.isCTEx(rest.name), new context_1.Context([
                    context_1.ctex(tt, exports.ktype),
                    context_1.ctex(tr, exports.krow),
                    context_1.csolved(rest.name, exports.krow, types_1.textend(l, types_1.tex(tt), types_1.tex(tr))),
                ])),
                ty: types_1.tex(tt),
                rest: types_1.textend(ty.label, ty.type, types_1.tex(tr)),
                ex: rest.name
            };
        }
        const r = rewriteRow(ctx, l, rest);
        return { ctx: r.ctx, ty: r.ty, rest: types_1.textend(ty.label, ty.type, r.rest), ex: r.ex };
    }
    return util_1.impossible();
}
function subtype(ctx, a, b) {
    // console.log(`subtype ${a} and ${b} in ${ctx}`);
    const k = typeWF(ctx, a);
    const k2 = typeWF(ctx, b);
    if (!k.equals(k2))
        return err(`kind mismatch ${a} and ${b}, ${k} and ${k2} in ${ctx}`);
    if (a instanceof types_1.TEmpty && b instanceof types_1.TEmpty)
        return ctx;
    if (((a instanceof types_1.TVar && b instanceof types_1.TVar) ||
        (a instanceof types_1.TEx && b instanceof types_1.TEx) ||
        (a instanceof types_1.TCon && b instanceof types_1.TCon)) && a.name === b.name)
        return ctx;
    if (a instanceof types_1.TApp && b instanceof types_1.TApp) {
        const ctx_ = subtype(ctx, a.left, b.left);
        return subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right));
    }
    if (a instanceof types_1.TFun && b instanceof types_1.TFun) {
        const ctx_ = subtype(ctx, b.left, a.left);
        return subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right));
    }
    if (a instanceof types_1.TForall) {
        const x = fresh(ctx.texs(), a.name);
        const ctx_ = subtype(ctx.add(context_1.cmarker(x), context_1.ctex(x, a.kind)), a.open(types_1.tex(x)), b);
        return (ctx_.split(context_1.isCMarker(x)).left);
    }
    if (b instanceof types_1.TForall) {
        const x = fresh(ctx.tvars(), b.name);
        const ctx_ = subtype(ctx.add(context_1.ctvar(x, b.kind)), a, b.open(types_1.tvar(x)));
        return (ctx_.split(context_1.isCTVar(x)).left);
    }
    if (a instanceof types_1.TEx) {
        const r1 = findEx(ctx, a.name);
        const r2 = check(!b.containsEx(a.name), `occurs check failed L: ${a} in ${b}`);
        return instL(ctx, a.name, b);
    }
    if (b instanceof types_1.TEx) {
        const r1 = findEx(ctx, b.name);
        const r2 = check(!a.containsEx(b.name), `occurs check failed R: ${b} in ${a}`);
        return instR(ctx, a, b.name);
    }
    if (a instanceof types_1.TExtend && b instanceof types_1.TExtend) {
        const r = rewriteRow(ctx, a.label, b);
        const tail = rowTail(a.rest);
        if (tail && r.ex && tail === r.ex)
            return err(`recursive row type: ${a} <: ${b}`);
        const ctx_ = subtype(r.ctx, r.ctx.apply(a.type), r.ctx.apply(r.ty));
        return subtype(ctx_, ctx_.apply(a.rest), ctx_.apply(r.rest));
    }
    return err(`subtype failed: ${a} <: ${b} in ${ctx}`);
}
// inst
function solve(ctx, name, ty) {
    // console.log(`solve ${name} and ${ty} in ${ctx}`);
    if (ty.isMono()) {
        const s = ctx.split(context_1.isCTEx(name));
        const k = typeWF(s.left, ty);
        return s.left.add(context_1.csolved(name, k, ty)).append(s.right);
    }
    else
        return err(`polymorphic type in solve: ${name} := ${ty} in ${ctx}`);
}
function instL(ctx, a, b) {
    // console.log(`instL ${a} and ${b} in ${ctx}`);
    if (b instanceof types_1.TEx && ctx.isOrdered(a, b.name))
        return solve(ctx, b.name, types_1.tex(a));
    if (b instanceof types_1.TEx && ctx.isOrdered(b.name, a))
        return solve(ctx, a, b);
    try {
        return solve(ctx, a, b);
    }
    catch (e) {
        if (!(e instanceof TypeError))
            throw e;
    }
    if (b instanceof types_1.TApp) {
        const kf = typeWF(ctx, b.left);
        const texs = ctx.texs();
        const a1 = fresh(texs, a);
        const a2 = fresh(texs.concat([a1]), a);
        const ctx_ = instL(ctx.replace(context_1.isCTEx(a), new context_1.Context([context_1.ctex(a2, kf.left), context_1.ctex(a1, kf), context_1.csolved(a, kf.right, types_1.tapp(types_1.tex(a1), types_1.tex(a2)))])), a1, b.left);
        return instL(ctx_, a2, ctx_.apply(b.right));
    }
    if (b instanceof types_1.TFun) {
        const texs = ctx.texs();
        const a1 = fresh(texs, a);
        const a2 = fresh(texs.concat([a1]), a);
        const ctx_ = instR(ctx.replace(context_1.isCTEx(a), new context_1.Context([context_1.ctex(a2, exports.ktype), context_1.ctex(a1, exports.ktype), context_1.csolved(a, exports.ktype, types_1.tfun(types_1.tex(a1), types_1.tex(a2)))])), b.left, a1);
        return instL(ctx_, a2, ctx_.apply(b.right));
    }
    if (b instanceof types_1.TForall) {
        const x = fresh(ctx.tvars(), b.name);
        const ctx_ = instL(ctx.add(context_1.ctvar(x, b.kind)), a, b.open(types_1.tvar(x)));
        return (ctx_.split(context_1.isCTVar(x)).left);
    }
    if (b instanceof types_1.TExtend) {
        const texs = ctx.texs();
        const at = fresh(texs, 't');
        const ar = fresh(texs.concat([at]), 'r');
        const ctx_ = instL(ctx.replace(context_1.isCTEx(a), new context_1.Context([
            context_1.ctex(at, exports.ktype),
            context_1.ctex(ar, exports.krow),
            context_1.csolved(a, exports.krow, types_1.textend(b.label, types_1.tex(at), types_1.tex(ar))),
        ])), at, b.type);
        return instL(ctx_, ar, ctx_.apply(b.rest));
    }
    return err(`instL failed: ${a} and ${b} in ${ctx}`);
}
function instR(ctx, a, b) {
    // console.log(`instR ${a} and ${b} in ${ctx}`);
    if (a instanceof types_1.TEx && ctx.isOrdered(b, a.name))
        return solve(ctx, a.name, types_1.tex(b));
    if (a instanceof types_1.TEx && ctx.isOrdered(a.name, b))
        return solve(ctx, b, a);
    try {
        return solve(ctx, b, a);
    }
    catch (e) {
        if (!(e instanceof TypeError))
            throw e;
    }
    if (a instanceof types_1.TApp) {
        const kf = typeWF(ctx, a.left);
        const texs = ctx.texs();
        const b1 = fresh(texs, b);
        const b2 = fresh(texs.concat([b1]), b);
        const ctx_ = instR(ctx.replace(context_1.isCTEx(b), new context_1.Context([context_1.ctex(b2, kf.left), context_1.ctex(b1, kf), context_1.csolved(b, kf.right, types_1.tapp(types_1.tex(b1), types_1.tex(b2)))])), a.left, b1);
        return instR(ctx_, ctx_.apply(a.right), b2);
    }
    if (a instanceof types_1.TFun) {
        const texs = ctx.texs();
        const b1 = fresh(texs, b);
        const b2 = fresh(texs.concat([b1]), b);
        const ctx_ = instL(ctx.replace(context_1.isCTEx(b), new context_1.Context([context_1.ctex(b2, exports.ktype), context_1.ctex(b1, exports.ktype), context_1.csolved(b, exports.ktype, types_1.tfun(types_1.tex(b1), types_1.tex(b2)))])), b1, a.left);
        return instR(ctx_, ctx_.apply(a.right), b2);
    }
    if (a instanceof types_1.TForall) {
        const x = fresh(ctx.texs(), a.name);
        const ctx_ = instR(ctx.add(context_1.cmarker(x), context_1.ctex(x, a.kind)), a.open(types_1.tex(x)), b);
        return (ctx_.split(context_1.isCMarker(x)).left);
    }
    if (a instanceof types_1.TExtend) {
        const texs = ctx.texs();
        const at = fresh(texs, 't');
        const ar = fresh(texs.concat([at]), 'r');
        const ctx_ = instR(ctx.replace(context_1.isCTEx(b), new context_1.Context([
            context_1.ctex(at, exports.ktype),
            context_1.ctex(ar, exports.krow),
            context_1.csolved(b, exports.krow, types_1.textend(a.label, types_1.tex(at), types_1.tex(ar))),
        ])), a.type, at);
        return instR(ctx_, ctx_.apply(a.rest), ar);
    }
    return err(`instR failed: ${a} and ${b} in ${ctx}`);
}
// synth/check
function generalize(ctx, marker, ty) {
    const s = ctx.split(marker);
    const t = s.right.apply(ty);
    const u = orderedTExs(s.right.unsolved(), t);
    return {
        ctx: s.left,
        ty: types_1.tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, types_1.tvar(n)), t)),
    };
}
function synth(ctx, e) {
    // console.log(`synth ${e} in ${ctx}`);
    contextWF(ctx);
    if (e instanceof exprs_1.EEmpty) {
        return { ctx, ty: types_1.tapp(exports.tsrec, types_1.tempty), expr: e };
    }
    if (e instanceof exprs_1.EVarEmpty) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype]], types_1.tfuns(types_1.tapp(exports.tsvar, types_1.tempty), types_1.tvar('t'))),
            expr: e
        };
    }
    if (e instanceof exprs_1.ESelect) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tapp(exports.tsrec, types_1.textend(e.label, types_1.tvar('t'), types_1.tvar('r'))), types_1.tvar('t'))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EExtend) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tvar('t'), types_1.tapp(exports.tsrec, types_1.tvar('r')), types_1.tapp(exports.tsrec, types_1.textend(e.label, types_1.tvar('t'), types_1.tvar('r'))))),
            expr: e
        };
    }
    if (e instanceof exprs_1.ERestrict) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tapp(exports.tsrec, types_1.textend(e.label, types_1.tvar('t'), types_1.tvar('r'))), types_1.tapp(exports.tsrec, types_1.tvar('r')))),
            expr: e
        };
    }
    if (e instanceof exprs_1.ERecUpdate) {
        return {
            ctx,
            ty: types_1.tforalls([['a', exports.ktype], ['b', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tfuns(types_1.tvar('a'), types_1.tvar('b')), types_1.tapp(exports.tsrec, types_1.textend(e.label, types_1.tvar('a'), types_1.tvar('r'))), types_1.tapp(exports.tsrec, types_1.textend(e.label, types_1.tvar('b'), types_1.tvar('r'))))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EInject) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tvar('t'), types_1.tapp(exports.tsvar, types_1.textend(e.label, types_1.tvar('t'), types_1.tvar('r'))))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EEmbed) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tapp(exports.tsvar, types_1.tvar('r')), types_1.tapp(exports.tsvar, types_1.textend(e.label, types_1.tvar('t'), types_1.tvar('r'))))),
            expr: e
        };
    }
    if (e instanceof exprs_1.ECase) {
        return {
            ctx,
            ty: types_1.tforalls([['a', exports.ktype], ['b', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tapp(exports.tsvar, types_1.textend(e.label, types_1.tvar('a'), types_1.tvar('r'))), types_1.tfuns(types_1.tvar('a'), types_1.tvar('b')), types_1.tfuns(types_1.tapp(exports.tsvar, types_1.tvar('r')), types_1.tvar('b')), types_1.tvar('b'))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EVarUpdate) {
        return {
            ctx,
            ty: types_1.tforalls([['a', exports.ktype], ['b', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tfuns(types_1.tvar('a'), types_1.tvar('b')), types_1.tapp(exports.tsvar, types_1.textend(e.label, types_1.tvar('a'), types_1.tvar('r'))), types_1.tapp(exports.tsvar, types_1.textend(e.label, types_1.tvar('b'), types_1.tvar('r'))))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EReturn) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tvar('t'), types_1.tapps(exports.tseff, types_1.tvar('r'), types_1.tvar('t')))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EPure) {
        return {
            ctx,
            ty: types_1.tforalls([['t', exports.ktype]], types_1.tfuns(types_1.tapps(exports.tseff, types_1.tempty, types_1.tvar('t')), types_1.tvar('t'))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EOp) {
        return {
            ctx,
            ty: types_1.tforalls([['a', exports.ktype], ['b', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tvar('a'), types_1.tapps(exports.tseff, types_1.textend(e.label, types_1.tfuns(types_1.tvar('a'), types_1.tvar('b')), types_1.tvar('r')), types_1.tvar('b')))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EDo) {
        return {
            ctx,
            ty: types_1.tforalls([['a', exports.ktype], ['b', exports.ktype], ['r', exports.krow]], types_1.tfuns(types_1.tapps(exports.tseff, types_1.tvar('r'), types_1.tvar('a')), types_1.tfuns(types_1.tvar('a'), types_1.tapps(exports.tseff, types_1.tvar('r'), types_1.tvar('b'))), types_1.tapps(exports.tseff, types_1.tvar('r'), types_1.tvar('b')))),
            expr: e
        };
    }
    if (e instanceof exprs_1.EHandler) {
        const map = e.map;
        const l = map.length;
        const texs = ctx.texs();
        const tm = fresh(texs, 'm');
        const tc = fresh(texs, 'c');
        const td = fresh(texs, 'd');
        const tr = fresh(texs, 'r');
        let ctx_ = ctx.add(context_1.cmarker(tm), context_1.ctex(tc, exports.ktype), context_1.ctex(td, exports.ktype), context_1.ctex(tr, exports.krow));
        const row = [];
        const rexpr = [];
        for (let i = 0; i < l; i++) {
            const c = map[i];
            const op = c[0];
            const ex = c[1];
            if (op === 'return') {
                // c -> SEff r d
                const rr = checkTy(ctx_, ex, types_1.tfuns(types_1.tex(tc), types_1.tapps(exports.tseff, types_1.tex(tr), types_1.tex(td))));
                rexpr.push([op, rr.expr]);
                ctx_ = rr.ctx;
            }
            else {
                const ta = fresh(texs, 'a');
                const tb = fresh(texs, 'b');
                // a -> (b -> SEff r d) -> SEff r d
                const rr = checkTy(ctx_.add(context_1.ctex(ta, exports.ktype), context_1.ctex(tb, exports.ktype)), ex, types_1.tfuns(types_1.tex(ta), types_1.tfuns(types_1.tex(tb), types_1.tapps(exports.tseff, types_1.tex(tr), types_1.tex(td))), types_1.tapps(exports.tseff, types_1.tex(tr), types_1.tex(td))));
                row.push([op, types_1.tfuns(types_1.tex(ta), types_1.tex(tb))]);
                rexpr.push([op, rr.expr]);
                ctx_ = rr.ctx;
            }
        }
        const rg = generalize(ctx_, context_1.isCMarker(tm), types_1.tfuns(types_1.tapps(exports.tseff, types_1.trow(row, types_1.tex(tr)), types_1.tex(tc)), types_1.tapps(exports.tseff, types_1.tex(tr), types_1.tex(td))));
        return {
            ctx: rg.ctx,
            ty: rg.ty,
            expr: exprs_1.ehandler(rexpr),
        };
    }
    if (e instanceof exprs_1.ELit) {
        return { ctx, ty: typeof e.val === 'string' ? exports.tstr : exports.tfloat, expr: e };
    }
    if (e instanceof exprs_1.EVar) {
        const ty = findVar(ctx, e.name);
        return { ctx, ty, expr: e };
    }
    if (e instanceof exprs_1.EAbs) {
        if (e.isAnnotated()) {
            const ty = e.type;
            const k = typeWF(ctx, ty);
            checkKindType(k);
            const x = fresh(ctx.vars(), e.name);
            const b = fresh(ctx.texs(), e.name);
            const r = checkTy(ctx.add(context_1.cmarker(b), context_1.ctex(b, exports.ktype), context_1.cvar(x, ty)), e.open(exprs_1.evar(x)), types_1.tex(b));
            const { ctx: ctx__, ty: ty__ } = generalize(r.ctx, context_1.isCMarker(b), types_1.tfun(ty, types_1.tex(b)));
            return { ctx: ctx__, ty: ty__, expr: exprs_1.eabs(e.name, r.expr) };
        }
        else {
            const x = fresh(ctx.vars(), e.name);
            const texs = ctx.texs();
            const a = fresh(texs, e.name);
            const b = fresh(texs.concat([a]), e.name);
            const r = checkTy(ctx.add(context_1.cmarker(a), context_1.ctex(a, exports.ktype), context_1.ctex(b, exports.ktype), context_1.cvar(x, types_1.tex(a))), e.open(exprs_1.evar(x)), types_1.tex(b));
            const { ctx: ctx__, ty: ty__ } = generalize(r.ctx, context_1.isCMarker(a), types_1.tfun(types_1.tex(a), types_1.tex(b)));
            return { ctx: ctx__, ty: ty__, expr: exprs_1.eabs(e.name, r.expr) };
        }
    }
    if (e instanceof exprs_1.EApp) {
        const r = synth(ctx, e.left);
        const { ctx: ctx_, ty: ty_, expr: right } = synthapp(r.ctx, r.ctx.apply(r.ty), e.right);
        return { ctx: ctx_, ty: ty_, expr: exprs_1.eapp(r.expr, right) };
    }
    if (e instanceof exprs_1.EAnno) {
        typeWF(ctx, e.type);
        const r = checkTy(ctx, e.expr, e.type);
        return { ctx: r.ctx, ty: e.type, expr: r.expr };
    }
    if (e instanceof exprs_1.ETAbs) {
        kindWF(ctx, e.kind);
        const x = fresh(ctx.vars(), e.name);
        const r = synth(ctx.add(context_1.ctvar(x, e.kind)), e.expr.substType(e.name, types_1.tvar(x)));
        return { ctx: r.ctx, ty: types_1.tforall(x, e.kind, r.ctx.apply(r.ty)), expr: e.expr };
    }
    if (e instanceof exprs_1.ETApp) {
        typeWF(ctx, e.type);
        const r = synth(ctx, e.expr);
        const ty_ = r.ctx.apply(r.ty);
        return ty_ instanceof types_1.TForall ? { ctx: r.ctx, ty: ty_.open(e.type), expr: e.expr } :
            err(`type application on non-polymorphic type: ${e} with ${ty_} in ${r.ctx}`);
    }
    return err(`cannot synth ${e} in ${ctx}`);
}
function checkTy(ctx, e, ty) {
    // console.log(`checkTy ${e} and ${ty} in ${ctx}`);
    contextWF(ctx);
    if (ty instanceof types_1.TForall) {
        const x = fresh(ctx.tvars(), ty.name);
        const r = checkTy(ctx.add(context_1.ctvar(x, ty.kind)), e, ty.open(types_1.tvar(x)));
        return { ctx: r.ctx.split(context_1.isCTVar(x)).left, expr: r.expr };
    }
    if (e instanceof exprs_1.EAbs && !e.isAnnotated() && ty instanceof types_1.TFun) {
        const x = fresh(ctx.vars(), e.name);
        const r = checkTy(ctx.add(context_1.cvar(x, ty.left)), e.open(exprs_1.evar(x)), ty.right);
        return { ctx: r.ctx.split(context_1.isCVar(x)).left, expr: exprs_1.eabs(e.name, r.expr) };
    }
    const rr = synth(ctx, e);
    return { ctx: subtype(rr.ctx, rr.ctx.apply(rr.ty), rr.ctx.apply(ty)), expr: rr.expr };
}
function synthapp(ctx, ty, e) {
    // console.log(`synthapp ${ty} and ${e} in ${ctx}`);
    contextWF(ctx);
    if (ty instanceof types_1.TForall) {
        const x = fresh(ctx.texs(), ty.name);
        return synthapp(ctx.add(context_1.ctex(x, ty.kind)), ty.open(types_1.tex(x)), e);
    }
    if (ty instanceof types_1.TEx) {
        findEx(ctx, ty.name);
        const texs = ctx.texs();
        const a1 = fresh(texs, ty.name);
        const a2 = fresh(texs.concat([a1]), ty.name);
        const r = checkTy(ctx.replace(context_1.isCTEx(ty.name), new context_1.Context([context_1.ctex(a2, exports.ktype), context_1.ctex(a1, exports.ktype), context_1.csolved(ty.name, exports.ktype, types_1.tfun(types_1.tex(a1), types_1.tex(a2)))])), e, types_1.tex(a1));
        return ({ ctx: r.ctx, ty: types_1.tex(a2), expr: r.expr });
    }
    if (ty instanceof types_1.TFun) {
        const r = checkTy(ctx, e, ty.left);
        return { ctx: r.ctx, ty: ty.right, expr: r.expr };
    }
    return err(`cannot synthapp ${ty} with ${e} in ${ctx}`);
}
function infer(ctx, e) {
    const m = fresh(ctx.texs(), 'i');
    const r = synth(ctx.add(context_1.cmarker(m)), e);
    contextWF(r.ctx);
    const ctx_ = r.ctx.applyContext(r.ctx);
    const ty_ = ctx_.apply(r.ty);
    const k = typeWF(ctx_, ty_);
    checkKindType(k);
    const { ctx: ctx__, ty: ty__ } = generalize(ctx_, context_1.isCMarker(m), ty_);
    return { ctx: ctx__, ty: ty__, expr: r.expr };
}
exports.infer = infer;
function inferDefinition(ctx, d) {
    if (d instanceof definitions_1.DValue) {
        console.log('' + d);
        const r = infer(ctx, (d.type ? exprs_1.eanno(d.val, d.type) : d.val));
        const ty_ = r.ty;
        const ctx_ = r.ctx.add(context_1.cvar(d.name, ty_));
        contextWF(ctx_);
        return { ctx: ctx_, def: new definitions_1.DValue(d.name, r.expr) };
    }
    else if (d instanceof definitions_1.DData) {
        console.log('' + d);
        const name = d.name;
        const params = d.params;
        const constrs = d.constrs;
        noDups(params.map(([n, _]) => n));
        noDups(constrs.map(([n, _]) => n));
        for (let i = 0; i < params.length; i++) {
            kindWF(ctx, params[i][1]);
        }
        for (let i = 0; i < constrs.length; i++) {
            const c = constrs[i];
            const n = c[0];
            const ts = c[1];
            for (let j = 0; j < ts.length; j++) {
                if (ts[j].occursNegatively(n, false))
                    return err(`${n} occurs in a negative position in ${ts[j]}`);
            }
        }
        const r = fresh(params.map(([n, _]) => n), 'r');
        const ctx_ = (ctx.add(context_1.ctcon(name, d.getKind())).append(new context_1.Context(constrs.map(([n, ts]) => context_1.cvar(n, types_1.tforalls(params, types_1.tfuns.apply(null, ts.concat([d.getType()]))))))).add(context_1.cvar(`case${d.name}`, types_1.tforalls(params, types_1.tforalls([[r, exports.ktype]], types_1.tfuns.apply(null, constrs.map(([n, ts]) => types_1.tfuns.apply(null, ts.concat([types_1.tvar(r)]))).concat([d.getType(), types_1.tvar(r)]))))), constrs.length === 1 && constrs[0][1].length === 1 ?
            context_1.cvar(`un${d.name}`, types_1.tforalls(params, types_1.tfuns(d.getType(), constrs[0][1][0]))) : null, context_1.cvar(`cata${d.name}`, types_1.tforalls(params, types_1.tforalls([[r, exports.ktype]], types_1.tfuns.apply(null, constrs.map(([n, ts]) => types_1.tfuns.apply(null, ts.map(t => t.equals(d.getType()) ? types_1.tvar(r) : t).concat([types_1.tvar(r)]))).concat([d.getType(), types_1.tvar(r)]))))), context_1.cvar(`para${d.name}`, types_1.tforalls(params, types_1.tforalls([[r, exports.ktype]], types_1.tfuns.apply(null, constrs.map(([n, ts]) => types_1.tfuns.apply(null, ts.map(t => t.equals(d.getType()) ? [t, types_1.tvar(r)] : [t]).reduce((a, b) => a.concat(b), []).concat([types_1.tvar(r)]))).concat([d.getType(), types_1.tvar(r)])))))));
        contextWF(ctx_);
        return { ctx: ctx_, def: d };
    }
    return util_1.impossible();
}
exports.inferDefinition = inferDefinition;
function inferProgram(ctx, ds) {
    let c = ctx;
    const defs = [];
    for (let i = 0; i < ds.length; i++) {
        const d = ds[i];
        const r = inferDefinition(c, d);
        c = r.ctx;
        defs.push(r.def);
    }
    return { ctx: c, defs };
}
exports.inferProgram = inferProgram;

},{"./context":2,"./definitions":3,"./exprs":4,"./kinds":5,"./types":10,"./util":11}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Type {
}
exports.Type = Type;
class TCon extends Type {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `${this.name}`;
    }
    equals(other) {
        return other instanceof TCon && this.name === other.name;
    }
    isMono() {
        return true;
    }
    subst(name, type) {
        return this;
    }
    substEx(name, type) {
        return this;
    }
    containsEx(name) {
        return false;
    }
    containsTCon(name) {
        return this.name === name;
    }
    texs() {
        return [];
    }
    tvars() {
        return [];
    }
    occursNegatively(name, negative) {
        return this.name === name && negative;
    }
}
exports.TCon = TCon;
exports.tcon = (name) => new TCon(name);
class TVar extends Type {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `${this.name}`;
    }
    equals(other) {
        return other instanceof TVar && this.name === other.name;
    }
    isMono() {
        return true;
    }
    subst(name, type) {
        return this.name === name ? type : this;
    }
    substEx(name, type) {
        return this;
    }
    containsEx(name) {
        return false;
    }
    containsTCon(name) {
        return false;
    }
    texs() {
        return [];
    }
    tvars() {
        return [this.name];
    }
    occursNegatively(name, negative) {
        return false;
    }
}
exports.TVar = TVar;
exports.tvar = (name) => new TVar(name);
class TEx extends Type {
    constructor(name) {
        super();
        this.name = name;
    }
    toString() {
        return `^${this.name}`;
    }
    equals(other) {
        return other instanceof TEx && this.name === other.name;
    }
    isMono() {
        return true;
    }
    subst(name, type) {
        return this;
    }
    substEx(name, type) {
        return this.name === name ? type : this;
    }
    containsEx(name) {
        return this.name === name;
    }
    containsTCon(name) {
        return false;
    }
    texs() {
        return [this.name];
    }
    tvars() {
        return [];
    }
    occursNegatively(name, negative) {
        return false;
    }
}
exports.TEx = TEx;
exports.tex = (name) => new TEx(name);
class TApp extends Type {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} ${this.right})`;
    }
    equals(other) {
        return other instanceof TApp && this.left.equals(other.left) && this.right.equals(other.right);
    }
    isMono() {
        return this.left.isMono() && this.right.isMono();
    }
    subst(name, type) {
        return new TApp(this.left.subst(name, type), this.right.subst(name, type));
    }
    substEx(name, type) {
        return new TApp(this.left.substEx(name, type), this.right.substEx(name, type));
    }
    containsEx(name) {
        return this.left.containsEx(name) || this.right.containsEx(name);
    }
    containsTCon(name) {
        return this.left.containsTCon(name) || this.right.containsTCon(name);
    }
    texs() {
        return this.left.texs().concat(this.right.texs());
    }
    tvars() {
        return this.left.tvars().concat(this.right.tvars());
    }
    occursNegatively(name, negative) {
        return this.left.occursNegatively(name, negative) || this.right.occursNegatively(name, negative);
    }
}
exports.TApp = TApp;
exports.tapp = (left, right) => new TApp(left, right);
exports.tapps = (...ts) => ts.reduce(exports.tapp);
class TFun extends Type {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return `(${this.left} -> ${this.right})`;
    }
    equals(other) {
        return other instanceof TFun && this.left.equals(other.left) && this.right.equals(other.right);
    }
    isMono() {
        return this.left.isMono() && this.right.isMono();
    }
    subst(name, type) {
        return new TFun(this.left.subst(name, type), this.right.subst(name, type));
    }
    substEx(name, type) {
        return new TFun(this.left.substEx(name, type), this.right.substEx(name, type));
    }
    containsEx(name) {
        return this.left.containsEx(name) || this.right.containsEx(name);
    }
    containsTCon(name) {
        return this.left.containsTCon(name) || this.right.containsTCon(name);
    }
    texs() {
        return this.left.texs().concat(this.right.texs());
    }
    tvars() {
        return this.left.tvars().concat(this.right.tvars());
    }
    occursNegatively(name, negative) {
        return this.left.occursNegatively(name, !negative) || this.right.occursNegatively(name, negative);
    }
}
exports.TFun = TFun;
exports.tfun = (left, right) => new TFun(left, right);
exports.tfuns = (...ts) => ts.reduceRight((a, b) => exports.tfun(b, a));
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
    equals(other) {
        return other instanceof TForall &&
            this.name === other.name &&
            this.kind.equals(other.kind) &&
            this.type.equals(other.type);
    }
    isMono() {
        return false;
    }
    subst(name, type) {
        return this.name === name ? this : new TForall(this.name, this.kind, this.type.subst(name, type));
    }
    substEx(name, type) {
        return new TForall(this.name, this.kind, this.type.substEx(name, type));
    }
    open(type) {
        return this.type.subst(this.name, type);
    }
    containsEx(name) {
        return this.type.containsEx(name);
    }
    containsTCon(name) {
        return this.type.containsTCon(name);
    }
    texs() {
        return this.type.texs();
    }
    tvars() {
        return [this.name].concat(this.type.tvars());
    }
    occursNegatively(name, negative) {
        return this.type.occursNegatively(name, negative);
    }
}
exports.TForall = TForall;
exports.tforall = (name, kind, type) => new TForall(name, kind, type);
exports.tforalls = (ns, type) => ns.reduceRight((a, b) => exports.tforall(b[0], b[1], a), type);
class TEmpty extends Type {
    toString() {
        return `{}`;
    }
    equals(other) {
        return other instanceof TEmpty;
    }
    isMono() {
        return true;
    }
    subst(name, type) {
        return this;
    }
    substEx(name, type) {
        return this;
    }
    containsEx(name) {
        return false;
    }
    containsTCon(name) {
        return false;
    }
    texs() {
        return [];
    }
    tvars() {
        return [];
    }
    occursNegatively(name, negative) {
        return false;
    }
}
exports.TEmpty = TEmpty;
exports.tempty = new TEmpty();
class TExtend extends Type {
    constructor(label, type, rest) {
        super();
        this.label = label;
        this.type = type;
        this.rest = rest;
    }
    toString() {
        return `{ ${this.label} : ${this.type} | ${this.rest} }`;
    }
    equals(other) {
        return other instanceof TExtend && this.label === other.label &&
            this.type.equals(other.type) && this.rest.equals(other.rest);
    }
    isMono() {
        return this.type.isMono() && this.rest.isMono();
    }
    subst(name, type) {
        return new TExtend(this.label, this.type.subst(name, type), this.rest.subst(name, type));
    }
    substEx(name, type) {
        return new TExtend(this.label, this.type.substEx(name, type), this.rest.substEx(name, type));
    }
    containsEx(name) {
        return this.type.containsEx(name) || this.rest.containsEx(name);
    }
    containsTCon(name) {
        return this.type.containsTCon(name) || this.rest.containsTCon(name);
    }
    texs() {
        return this.type.texs().concat(this.rest.texs());
    }
    tvars() {
        return this.type.tvars().concat(this.rest.tvars());
    }
    occursNegatively(name, negative) {
        return this.type.occursNegatively(name, !negative) || this.rest.occursNegatively(name, negative);
    }
}
exports.TExtend = TExtend;
exports.textend = (label, type, rest) => new TExtend(label, type, rest);
exports.trow = (props, rest) => props.reduceRight((r, [l, t]) => exports.textend(l, t, r), rest || exports.tempty);

},{}],11:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function err(msg) { throw new Error(msg); }
exports.err = err;
exports.impossible = () => err('impossible');

},{}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const repl_1 = require("./repl");
function getOutput(s, cb) {
    repl_1.default(s, cb);
}
var hist = [], index = -1;
var input = document.getElementById('input');
var content = document.getElementById('content');
var res = {};
function onresize() {
    content.style.height = window.innerHeight;
}
window.addEventListener('resize', onresize);
onresize();
addResult("REPL (try :help)");
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

},{"./repl":8}]},{},[12]);

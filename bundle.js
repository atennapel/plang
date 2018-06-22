(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Result {
    constructor() { }
    static ok(t) { return new Ok(t); }
    static err(e) { return new Err(e); }
}
exports.Result = Result;
class Ok extends Result {
    constructor(val) {
        super();
        this.val = val;
    }
    toString() { return 'Ok(' + this.val + ')'; }
    map(fn) { return Result.ok(fn(this.val)); }
    then(fn) { return fn(this.val); }
    catch(fn) { return this; }
    not(fn) { return fn(this.val); }
}
exports.Ok = Ok;
function isOk(x) { return x instanceof Ok; }
exports.isOk = isOk;
class Err extends Result {
    constructor(err) {
        super();
        this.err = err;
    }
    toString() { return 'Err(' + this.err + ')'; }
    map(fn) { return Result.err(this.err); }
    then(fn) { return Result.err(this.err); }
    catch(fn) { return fn(this.err); }
}
exports.Err = Err;
function isErr(x) { return x instanceof Err; }
exports.isErr = isErr;

},{}],2:[function(require,module,exports){
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
    if (expr instanceof exprs_1.EAnno)
        return compile(expr.expr);
    if (expr instanceof exprs_1.ETApp)
        return compile(expr.expr);
    if (expr instanceof exprs_1.ETAbs)
        return compile(expr.expr);
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
function compileDefinition(d, attachVars) {
    if (d instanceof definitions_1.DValue)
        return `${varPrefix(d.name, attachVars)} = ${compile(d.val)}`;
    if (d instanceof definitions_1.DData)
        return d.constrs.map(([n, ts]) => `${varPrefix(n, attachVars)} = ${compileConstructor(n, ts.length)}`).join(';') + ';' + `${varPrefix(`case${d.name}`, attachVars)} = ${compileCase(d.name, d.constrs)};`;
    return util_1.impossible();
}
function compileProgram(p, withMain, lib = '', attachVars) {
    return `;${lib.trim()};${p.map(d => compileDefinition(d, attachVars)).join(';')}${withMain ? `;console.log(show(main))` : ''};`;
}
exports.compileProgram = compileProgram;

},{"./definitions":4,"./exprs":5,"./util":12}],3:[function(require,module,exports){
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
        return new Context(this.elems.concat(es));
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

},{"./types":11}],4:[function(require,module,exports){
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

},{"./kinds":6,"./typechecker":10,"./types":11}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
class Expr {
}
exports.Expr = Expr;
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

},{}],6:[function(require,module,exports){
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

},{}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const exprs_1 = require("./exprs");
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const typechecker_1 = require("./typechecker");
const definitions_1 = require("./definitions");
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
    const START = 0, NAME = 1;
    let state = START;
    let r = [], p = [], b = [];
    let t = '';
    for (let i = 0; i <= s.length; i++) {
        const c = s[i] || ' ';
        if (state === START) {
            if (/[a-z0-9]/i.test(c))
                t += c, state = NAME;
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
            if (!/[a-z0-9\']/i.test(c))
                r.push(token(t)), t = '', i--, state = START;
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
        const n = +x.val;
        if (!isNaN(n) && n >= 0) {
            let t = exprs_1.evar('z');
            for (let i = 0; i < n; i++) {
                t = exprs_1.eapp(exprs_1.evar('s'), t);
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

},{"./definitions":4,"./exprs":5,"./kinds":6,"./typechecker":10,"./types":11}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const kinds_1 = require("./kinds");
const types_1 = require("./types");
const context_1 = require("./context");
const util_1 = require("./util");
const typechecker_1 = require("./typechecker");
const RARROW = ' -> ';
const FORALL = '\u2200';
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
function isSymbol(n) {
    return !/[a-z]/i.test(n[0]);
}
function ppType(t) {
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
        return `${FORALL}${args.join(' ')}. ${ppType(f.ty)}`;
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

},{"./context":3,"./kinds":6,"./typechecker":10,"./types":11,"./util":12}],9:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const compilerJS_1 = require("./compilerJS");
const typechecker_1 = require("./typechecker");
const Result_1 = require("./Result");
const parser_1 = require("./parser");
const prettyprinter_1 = require("./prettyprinter");
const definitions_1 = require("./definitions");
exports.context = typechecker_1.initialContext;
function show(x) {
    if (x === null)
        return `()`;
    if (x._adt)
        return x._args.length === 0 ? `${x._tag}` : `(${x._tag}${x._args.length > 0 ? ` ${x._args.map(show).join(' ')}` : ''})`;
    if (Array.isArray(x))
        return `[${x.map(show).join(', ')}]`;
    if (typeof x === 'function')
        return `[Function]`;
    if (x._tag === 'inl')
        return `(Inl ${show(x._val)})`;
    if (x._tag === 'inr')
        return `(Inr ${show(x._val)})`;
    if (x._tag === 'pair')
        return `(${show(x._fst)}, ${show(x._snd)})`;
    return `${x}`;
}
let ctx = exports.context;
function run(i, cb) {
    const cmd = i.trim().toLowerCase();
    if (cmd === ':help') {
        cb('commands :help :context :def :prelude');
    }
    else if (cmd === ':prelude') {
        try {
            const ds = parser_1.parseProgram(eval('_prelude'));
            const t = typechecker_1.inferProgram(ctx, ds);
            if (Result_1.isErr(t))
                throw t.err;
            else if (Result_1.isOk(t)) {
                eval(compilerJS_1.compileProgram(ds, false, '', true));
                ctx = t.val;
                cb('prelude loaded');
            }
        }
        catch (err) {
            return cb('' + err, true);
        }
    }
    else if (cmd === ':context') {
        cb(ctx.elems.map(prettyprinter_1.ppContextElem).join('\n'));
    }
    else if (cmd.slice(0, 4) === ':def') {
        const rest = i.slice(4).trim();
        try {
            const d = parser_1.parseDefinition(rest);
            const t = typechecker_1.inferDefinition(ctx, d);
            if (Result_1.isErr(t))
                throw t.err;
            else if (Result_1.isOk(t)) {
                ctx = t.val;
                if (d instanceof definitions_1.DValue) {
                    const c = compilerJS_1.compile(d.val);
                    console.log(c);
                    const res = eval(`(typeof global === 'undefined'? window: global)['${d.name}'] = ${c}`);
                    cb(`${d.name} : ${prettyprinter_1.ppType(ctx.apply(ctx.findVar(d.name)))} = ${show(res)}`);
                }
                else if (d instanceof definitions_1.DData) {
                    d.constrs.forEach(([n, ts]) => eval(`(typeof global === 'undefined'? window: global)['${n}'] = ${compilerJS_1.compileConstructor(n, ts.length)}`));
                    eval(`(typeof global === 'undefined'? window: global)['case${d.name}'] = ${compilerJS_1.compileCase(d.name, d.constrs)}`);
                    cb(`defined ${d.name}`);
                }
                else
                    return cb('unknown definition', true);
            }
        }
        catch (err) {
            return cb('' + err, true);
        }
    }
    else {
        try {
            const p = parser_1.parse(i);
            console.log('' + p);
            const tr = typechecker_1.infer(ctx, p);
            if (Result_1.isErr(tr))
                throw tr.err;
            else if (Result_1.isOk(tr)) {
                const c = compilerJS_1.compile(p);
                console.log(c);
                const res = eval(c);
                cb(`${show(res)} : ${prettyprinter_1.ppType(tr.val.ty)}`);
            }
        }
        catch (e) {
            cb('' + e, true);
        }
    }
}
exports.default = run;

},{"./Result":1,"./compilerJS":2,"./definitions":4,"./parser":7,"./prettyprinter":8,"./typechecker":10}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const Result_1 = require("./Result");
const util_1 = require("./util");
const types_1 = require("./types");
const context_1 = require("./context");
const exprs_1 = require("./exprs");
const kinds_1 = require("./kinds");
const definitions_1 = require("./definitions");
const err = (msg) => Result_1.Result.err(new TypeError(msg));
const ok = (val) => Result_1.Result.ok(val);
const not = (r, m) => {
    if (Result_1.isOk(r))
        return err(m);
    if (Result_1.isErr(r))
        return ok(null);
    return util_1.impossible();
};
const check = (b, m) => b ? ok(null) : err(m);
function noDups(d) {
    const o = {};
    for (let i = 0; i < d.length; i++) {
        if (o[d[i]])
            return err(`duplicate ${d[i]}`);
        o[d[i]] = true;
    }
    return ok(null);
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
    return r === null ? err(`var ${name} not found in ${ctx}`) : ok(r);
};
const findSolved = (ctx, name) => {
    const r = ctx.findSolved(name);
    return r === null ? err(`var ${name} not found in ${ctx}`) : ok(r);
};
const findKCon = (ctx, name) => {
    return ctx.findKCon(name) === null ? err(`tcon ${name} not found in ${ctx}`) : ok(null);
};
const findTCon = (ctx, name) => {
    const r = ctx.findTCon(name);
    return r === null ? err(`tcon ${name} not found in ${ctx}`) : ok(r);
};
const findTVar = (ctx, name) => {
    const r = ctx.findTVar(name);
    return r === null ? err(`tvar ${name} not found in ${ctx}`) : ok(r);
};
const findEx = (ctx, name) => {
    const r = ctx.findEx(name);
    return r === null ? err(`ex ^${name} not found in ${ctx}`) : ok(r);
};
const findMarker = (ctx, name) => {
    return ctx.findMarker(name) === null ? err(`marker |>${name} not found in ${ctx}`) : ok(null);
};
const findExOrSolved = (ctx, name) => {
    const r = ctx.findExOrSolved(name);
    return r === null ? err(`ex or solved ^${name} not found in ${ctx}`) : ok(r);
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
exports.initialContext = new context_1.Context([
    context_1.ckcon('Type'),
]);
// wf
function checkKindType(kind) {
    return exports.ktype.equals(kind) ? ok(null) : err(`kind is not ${exports.ktype}: ${kind}`);
}
function kindWF(ctx, kind) {
    // console.log(`kindWF ${kind} in ${ctx}`);
    if (kind instanceof kinds_1.KCon)
        return findKCon(ctx, kind.name);
    if (kind instanceof kinds_1.KFun)
        return kindWF(ctx, kind.left).then(() => kindWF(ctx, kind.right));
    return util_1.impossible();
}
function typeWF(ctx, ty) {
    //console.log(`typeWF ${ty} in ${ctx}`);
    if (ty instanceof types_1.TCon)
        return findTCon(ctx, ty.name).then(k => kindWF(ctx, k).map(() => k));
    if (ty instanceof types_1.TVar)
        return findTVar(ctx, ty.name).then(k => kindWF(ctx, k).map(() => k));
    if (ty instanceof types_1.TEx)
        return findExOrSolved(ctx, ty.name).then(k => kindWF(ctx, k).map(() => k));
    if (ty instanceof types_1.TFun)
        return typeWF(ctx, ty.left).then(k1 => checkKindType(k1).then(() => typeWF(ctx, ty.right).then(k2 => checkKindType(k2).map(() => exports.ktype))));
    if (ty instanceof types_1.TForall)
        return kindWF(ctx, ty.kind).then(() => typeWF(ctx.add(context_1.ctvar(ty.name, ty.kind)), ty.type));
    if (ty instanceof types_1.TApp)
        return typeWF(ctx, ty.left).then(kleft => {
            if (kleft instanceof kinds_1.KFun)
                return typeWF(ctx, ty.right)
                    .then(kright => {
                    if (!kright.equals(kleft.left))
                        return err(`kind mismatch in type constructor: ${ty} in ${ctx}`);
                    return ok(kleft.right);
                });
            else
                return err(`not a type constructor: ${ty} in ${ctx}`);
        });
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
            const m = not(findKCon(p, e.name), `duplicate kcon ^${e.name}`);
            if (Result_1.isErr(m))
                return m;
        }
        else if (e instanceof context_1.CTCon) {
            const m = not(findTCon(p, e.name), `duplicate tcon ${e.name}`)
                .then(() => kindWF(p, e.kind));
            if (Result_1.isErr(m))
                return m;
        }
        else if (e instanceof context_1.CTVar) {
            const m = not(findTVar(p, e.name), `duplicate tvar ${e.name}`)
                .then(() => kindWF(p, e.kind));
            if (Result_1.isErr(m))
                return m;
        }
        else if (e instanceof context_1.CTEx || e instanceof context_1.CSolved) {
            const m = not(findExOrSolved(p, e.name), `duplicate tex ^${e.name}`)
                .then(() => kindWF(p, e.kind));
            if (Result_1.isErr(m))
                return m;
        }
        else if (e instanceof context_1.CVar) {
            const m = not(findVar(p, e.name), `duplicate var ${e.name}`)
                .then(() => typeWF(p, e.type).then(k => checkKindType(k)));
            if (Result_1.isErr(m))
                return m;
        }
        else if (e instanceof context_1.CMarker) {
            const m = not(findMarker(p, e.name), `duplicate marker ^${e.name}`)
                .then(() => not(findExOrSolved(p, e.name), `duplicate marker ^${e.name}`));
            if (Result_1.isErr(m))
                return m;
        }
        else
            return util_1.impossible();
    }
    return ok(null);
}
// subtype
function subtype(ctx, a, b) {
    // console.log(`subtype ${a} and ${b} in ${ctx}`);
    const wf = typeWF(ctx, a).then(k1 => typeWF(ctx, b).then(k2 => ok({ k1, k2 })));
    if (Result_1.isErr(wf))
        return new Result_1.Err(wf.err);
    const wfok = wf;
    const k = wfok.val.k1;
    if (!k.equals(wfok.val.k2))
        return err(`kind mismatch ${a} and ${b}, ${k} and ${wfok.val.k2} in ${ctx}`);
    if (((a instanceof types_1.TVar && b instanceof types_1.TVar) ||
        (a instanceof types_1.TEx && b instanceof types_1.TEx) ||
        (a instanceof types_1.TCon && b instanceof types_1.TCon)) && a.name === b.name)
        return ok(ctx);
    if (a instanceof types_1.TApp && b instanceof types_1.TApp)
        return subtype(ctx, a.left, b.left)
            .then(ctx_ => subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right)));
    if (a instanceof types_1.TFun && b instanceof types_1.TFun)
        return subtype(ctx, b.left, a.left)
            .then(ctx_ => subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right)));
    if (a instanceof types_1.TForall) {
        const x = fresh(ctx.texs(), a.name);
        return subtype(ctx.add(context_1.cmarker(x), context_1.ctex(x, a.kind)), a.open(types_1.tex(x)), b)
            .then(ctx_ => ok(ctx_.split(context_1.isCMarker(x)).left));
    }
    if (b instanceof types_1.TForall) {
        const x = fresh(ctx.tvars(), b.name);
        return subtype(ctx.add(context_1.ctvar(x, b.kind)), a, b.open(types_1.tvar(x)))
            .then(ctx_ => ok(ctx_.split(context_1.isCTVar(x)).left));
    }
    if (a instanceof types_1.TEx)
        return findEx(ctx, a.name)
            .then(() => check(!b.containsEx(a.name), `occurs check failed L: ${a} in ${b}`))
            .then(() => instL(ctx, a.name, b));
    if (b instanceof types_1.TEx)
        return findEx(ctx, b.name)
            .then(() => check(!a.containsEx(b.name), `occurs check failed R: ${b} in ${a}`))
            .then(() => instR(ctx, a, b.name));
    return err(`subtype failed: ${a} <: ${b} in ${ctx}`);
}
// inst
function solve(ctx, name, ty) {
    // console.log(`solve ${name} and ${ty} in ${ctx}`);
    if (ty.isMono()) {
        const s = ctx.split(context_1.isCTEx(name));
        return typeWF(s.left, ty)
            .then(k => ok(s.left.add(context_1.csolved(name, k, ty)).append(s.right)));
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
    const r = solve(ctx, a, b);
    if (Result_1.isOk(r))
        return r;
    if (b instanceof types_1.TApp) {
        return typeWF(ctx, b.left).then((kf) => {
            const texs = ctx.texs();
            const a1 = fresh(texs, a);
            const a2 = fresh(texs.concat([a1]), a);
            return instL(ctx.replace(context_1.isCTEx(a), new context_1.Context([context_1.ctex(a2, kf.left), context_1.ctex(a1, kf), context_1.csolved(a, kf.right, types_1.tapp(types_1.tex(a1), types_1.tex(a2)))])), a1, b.left)
                .then(ctx_ => instL(ctx_, a2, ctx_.apply(b.right)));
        });
    }
    if (b instanceof types_1.TFun) {
        const texs = ctx.texs();
        const a1 = fresh(texs, a);
        const a2 = fresh(texs.concat([a1]), a);
        return instR(ctx.replace(context_1.isCTEx(a), new context_1.Context([context_1.ctex(a2, exports.ktype), context_1.ctex(a1, exports.ktype), context_1.csolved(a, exports.ktype, types_1.tfun(types_1.tex(a1), types_1.tex(a2)))])), b.left, a1)
            .then(ctx_ => instL(ctx_, a2, ctx_.apply(b.right)));
    }
    if (b instanceof types_1.TForall) {
        const x = fresh(ctx.tvars(), b.name);
        return instL(ctx.add(context_1.ctvar(x, b.kind)), a, b.open(types_1.tvar(x)))
            .then(ctx_ => ok(ctx_.split(context_1.isCTVar(x)).left));
    }
    return err(`instL failed: ${a} and ${b} in ${ctx}`);
}
function instR(ctx, a, b) {
    // console.log(`instR ${a} and ${b} in ${ctx}`);
    if (a instanceof types_1.TEx && ctx.isOrdered(b, a.name))
        return solve(ctx, a.name, types_1.tex(b));
    if (a instanceof types_1.TEx && ctx.isOrdered(a.name, b))
        return solve(ctx, b, a);
    const r = solve(ctx, b, a);
    if (Result_1.isOk(r))
        return r;
    if (a instanceof types_1.TApp) {
        return typeWF(ctx, a.left).then((kf) => {
            const texs = ctx.texs();
            const b1 = fresh(texs, b);
            const b2 = fresh(texs.concat([b1]), b);
            return instR(ctx.replace(context_1.isCTEx(b), new context_1.Context([context_1.ctex(b2, kf.left), context_1.ctex(b1, kf), context_1.csolved(b, kf.right, types_1.tapp(types_1.tex(b1), types_1.tex(b2)))])), a.left, b1)
                .then(ctx_ => instR(ctx_, ctx_.apply(a.right), b2));
        });
    }
    if (a instanceof types_1.TFun) {
        const texs = ctx.texs();
        const b1 = fresh(texs, b);
        const b2 = fresh(texs.concat([b1]), b);
        return instL(ctx.replace(context_1.isCTEx(b), new context_1.Context([context_1.ctex(b2, exports.ktype), context_1.ctex(b1, exports.ktype), context_1.csolved(b, exports.ktype, types_1.tfun(types_1.tex(b1), types_1.tex(b2)))])), b1, a.left)
            .then(ctx_ => instR(ctx_, ctx_.apply(a.right), b2));
    }
    if (a instanceof types_1.TForall) {
        const x = fresh(ctx.texs(), a.name);
        return instR(ctx.add(context_1.cmarker(x), context_1.ctex(x, a.kind)), a.open(types_1.tex(x)), b)
            .then(ctx_ => ok(ctx_.split(context_1.isCMarker(x)).left));
    }
    return err(`instR failed: ${a} and ${b} in ${ctx}`);
}
// synth/check
function synth(ctx, e) {
    // console.log(`synth ${e} in ${ctx}`);
    const r = contextWF(ctx);
    if (Result_1.isErr(r))
        return new Result_1.Err(r.err);
    if (e instanceof exprs_1.EVar)
        return findVar(ctx, e.name).then(ty => ok({ ctx, ty }));
    if (e instanceof exprs_1.EAbs) {
        if (e.isAnnotated()) {
            const ty = e.type;
            return typeWF(ctx, ty).then(k => checkKindType(k).then(() => {
                const x = fresh(ctx.vars(), e.name);
                const b = fresh(ctx.texs(), e.name);
                return checkTy(ctx.add(context_1.cmarker(b), context_1.ctex(b, exports.ktype), context_1.cvar(x, ty)), e.open(exprs_1.evar(x)), types_1.tex(b))
                    .then(ctx_ => {
                    const s = ctx_.split(context_1.isCMarker(b));
                    const t = s.right.apply(types_1.tfun(ty, types_1.tex(b)));
                    const u = orderedTExs(s.right.unsolved(), t);
                    return ok({
                        ctx: s.left,
                        ty: types_1.tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, types_1.tvar(n)), t)),
                    });
                });
            }));
        }
        else {
            const x = fresh(ctx.vars(), e.name);
            const texs = ctx.texs();
            const a = fresh(texs, e.name);
            const b = fresh(texs.concat([a]), e.name);
            return checkTy(ctx.add(context_1.cmarker(a), context_1.ctex(a, exports.ktype), context_1.ctex(b, exports.ktype), context_1.cvar(x, types_1.tex(a))), e.open(exprs_1.evar(x)), types_1.tex(b))
                .then(ctx_ => {
                const s = ctx_.split(context_1.isCMarker(a));
                const t = s.right.apply(types_1.tfun(types_1.tex(a), types_1.tex(b)));
                const u = orderedTExs(s.right.unsolved(), t);
                return ok({
                    ctx: s.left,
                    ty: types_1.tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, types_1.tvar(n)), t)),
                });
            });
        }
    }
    if (e instanceof exprs_1.EApp)
        return synth(ctx, e.left)
            .then(({ ctx: ctx_, ty }) => synthapp(ctx_, ctx_.apply(ty), e.right));
    if (e instanceof exprs_1.EAnno)
        return typeWF(ctx, e.type)
            .then(() => checkTy(ctx, e.expr, e.type).then(ctx_ => ok({ ctx: ctx_, ty: e.type })));
    if (e instanceof exprs_1.ETAbs)
        return kindWF(ctx, e.kind).then(() => {
            const x = fresh(ctx.vars(), e.name);
            return synth(ctx.add(context_1.ctvar(x, e.kind)), e.expr.substType(e.name, types_1.tvar(x))).then(({ ctx: ctx_, ty }) => ok({ ctx: ctx_, ty: types_1.tforall(x, e.kind, ctx_.apply(ty)) }));
        });
    if (e instanceof exprs_1.ETApp)
        return typeWF(ctx, e.type)
            .then(() => synth(ctx, e.expr)
            .then(({ ctx: ctx_, ty }) => {
            const ty_ = ctx_.apply(ty);
            return ty_ instanceof types_1.TForall ? ok({ ctx: ctx_, ty: ty_.open(e.type) }) :
                err(`type application on non-polymorphic type: ${e} with ${ty_} in ${ctx_}`);
        }));
    return err(`cannot synth ${e} in ${ctx}`);
}
function checkTy(ctx, e, ty) {
    // console.log(`checkTy ${e} and ${ty} in ${ctx}`);
    const r = contextWF(ctx);
    if (Result_1.isErr(r))
        return new Result_1.Err(r.err);
    if (ty instanceof types_1.TForall) {
        const x = fresh(ctx.tvars(), ty.name);
        return checkTy(ctx.add(context_1.ctvar(x, ty.kind)), e, ty.open(types_1.tvar(x)))
            .then(ctx_ => ok(ctx_.split(context_1.isCTVar(x)).left));
    }
    if (e instanceof exprs_1.EAbs && !e.isAnnotated() && ty instanceof types_1.TFun) {
        const x = fresh(ctx.vars(), e.name);
        return checkTy(ctx.add(context_1.cvar(x, ty.left)), e.open(exprs_1.evar(x)), ty.right)
            .then(ctx_ => ok(ctx_.split(context_1.isCVar(x)).left));
    }
    return synth(ctx, e)
        .then(({ ctx: ctx_, ty: ty_ }) => subtype(ctx_, ctx_.apply(ty_), ctx_.apply(ty)));
}
function synthapp(ctx, ty, e) {
    // console.log(`synthapp ${ty} and ${e} in ${ctx}`);
    const r = contextWF(ctx);
    if (Result_1.isErr(r))
        return new Result_1.Err(r.err);
    if (ty instanceof types_1.TForall) {
        const x = fresh(ctx.texs(), ty.name);
        return synthapp(ctx.add(context_1.ctex(x, ty.kind)), ty.open(types_1.tex(x)), e);
    }
    if (ty instanceof types_1.TEx)
        return findEx(ctx, ty.name)
            .then(() => {
            const texs = ctx.texs();
            const a1 = fresh(texs, ty.name);
            const a2 = fresh(texs.concat([a1]), ty.name);
            return checkTy(ctx.replace(context_1.isCTEx(ty.name), new context_1.Context([context_1.ctex(a2, exports.ktype), context_1.ctex(a1, exports.ktype), context_1.csolved(ty.name, exports.ktype, types_1.tfun(types_1.tex(a1), types_1.tex(a2)))])), e, types_1.tex(a1))
                .then(ctx_ => ok({ ctx: ctx_, ty: types_1.tex(a2) }));
        });
    if (ty instanceof types_1.TFun)
        return checkTy(ctx, e, ty.left)
            .then(ctx_ => ok({ ctx: ctx_, ty: ty.right }));
    return err(`cannot synthapp ${ty} with ${e} in ${ctx}`);
}
function infer(ctx, e) {
    return synth(ctx, e)
        .then(({ ctx: ctx__, ty }) => contextWF(ctx__)
        .then(() => {
        const ctx_ = ctx__.applyContext(ctx__);
        const ty_ = ctx_.apply(ty);
        return typeWF(ctx_, ty_).then(k => checkKindType(k).then(() => {
            if (ctx_.isComplete())
                return ok({ ctx: ctx_, ty: ty_ });
            const unsolved = ctx_.unsolved();
            const unsolvedNames = unsolved.map(([n, _]) => n);
            const u = orderedTExs(unsolved, ty_);
            return ok({
                ctx: ctx_.removeAll(e => (e instanceof context_1.CSolved) || ((e instanceof context_1.CTEx || e instanceof context_1.CMarker) && unsolvedNames.indexOf(e.name) >= 0)),
                ty: types_1.tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, types_1.tvar(n)), ty_)),
            });
        }));
    }));
}
exports.infer = infer;
function inferDefinition(ctx, d) {
    if (d instanceof definitions_1.DValue) {
        console.log('' + d);
        return infer(ctx, (d.type ? exprs_1.eanno(d.val, d.type) : d.val))
            .map(x => ({ ty: x.ty, ctx: x.ctx.removeAll(e => e instanceof context_1.CSolved) }))
            .then(({ ctx, ty }) => contextWF(ctx.add(context_1.cvar(d.name, ty)))
            .map(() => ctx.add(context_1.cvar(d.name, ty))));
    }
    else if (d instanceof definitions_1.DData) {
        console.log('' + d);
        const name = d.name;
        const params = d.params;
        const constrs = d.constrs;
        return noDups(params.map(([n, _]) => n))
            .then(() => noDups(constrs.map(([n, _]) => n)))
            .then(() => {
            for (let i = 0; i < params.length; i++) {
                const r = kindWF(ctx, params[i][1]);
                if (Result_1.isErr(r))
                    return new Result_1.Err(r.err);
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
            return ok(ctx.add(context_1.ctcon(name, d.getKind())).append(new context_1.Context(constrs.map(([n, ts]) => context_1.cvar(n, types_1.tforalls(params, types_1.tfuns.apply(null, ts.concat([d.getType()]))))))).add(context_1.cvar(`case${d.name}`, types_1.tforalls(params, types_1.tforalls([[r, exports.ktype]], types_1.tfuns.apply(null, constrs.map(([n, ts]) => types_1.tfuns.apply(null, ts.concat([types_1.tvar(r)]))).concat([d.getType(), types_1.tvar(r)])))))));
        })
            .then((ctx) => contextWF(ctx).map(() => ctx));
    }
    return util_1.impossible();
}
exports.inferDefinition = inferDefinition;
function inferProgram(ctx, ds) {
    let c = ctx;
    for (let i = 0; i < ds.length; i++) {
        const d = ds[i];
        const r = inferDefinition(c, d);
        if (Result_1.isErr(r))
            return new Result_1.Err(r.err);
        else if (Result_1.isOk(r)) {
            console.log('' + d, '' + r.val);
            c = r.val;
        }
        else
            util_1.impossible();
    }
    return ok(c);
}
exports.inferProgram = inferProgram;

},{"./Result":1,"./context":3,"./definitions":4,"./exprs":5,"./kinds":6,"./types":11,"./util":12}],11:[function(require,module,exports){
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

},{}],12:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function err(msg) { throw new Error(msg); }
exports.err = err;
exports.impossible = () => err('impossible');

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

},{"./repl":9}]},{},[13]);

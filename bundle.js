(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
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
exports.default = compile;

},{"./exprs":4,"./util":10}],3:[function(require,module,exports){
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
}
exports.Context = Context;

},{"./types":9}],4:[function(require,module,exports){
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
            else if (c === ':')
                r.push(token(':'));
            else if (c === '.')
                r.push(token('.'));
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
        return exprs_1.evar('unit');
    if (x.length === 1)
        return expr(x[0]);
    if (containsToken(x, ':')) {
        const s = splitOn(x, x => isToken(x, ':'));
        if (s.length !== 2)
            throw new SyntaxError('nested anno :');
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
            throw new SyntaxError(`missing -> after forall`);
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

},{"./exprs":4,"./kinds":5,"./typechecker":8,"./types":9}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("./types");
const compilerJS_1 = require("./compilerJS");
const typechecker_1 = require("./typechecker");
const context_1 = require("./context");
const kinds_1 = require("./kinds");
const Result_1 = require("./Result");
const parser_1 = require("./parser");
exports.context = typechecker_1.initialContext.add(context_1.ctcon('Unit', typechecker_1.ktype), context_1.ctcon('Void', typechecker_1.ktype), context_1.cvar('unit', types_1.tcon('Unit')), context_1.cvar('impossible', types_1.tforalls([['t', typechecker_1.ktype]], types_1.tfuns(types_1.tcon('Void'), types_1.tvar('t')))), context_1.ctcon('Nat', typechecker_1.ktype), context_1.cvar('Z', types_1.tcon('Nat')), context_1.cvar('S', types_1.tfuns(types_1.tcon('Nat'), types_1.tcon('Nat'))), context_1.cvar('rec', types_1.tforalls([['r', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('r'), types_1.tfuns(types_1.tvar('r'), types_1.tcon('Nat'), types_1.tvar('r')), types_1.tcon('Nat'), types_1.tvar('r')))), context_1.ctcon('Bool', typechecker_1.ktype), context_1.cvar('true', types_1.tcon('Bool')), context_1.cvar('false', types_1.tcon('Bool')), context_1.cvar('iff', types_1.tforalls([['t', typechecker_1.ktype]], types_1.tfuns(types_1.tcon('Bool'), types_1.tvar('t'), types_1.tvar('t'), types_1.tvar('t')))), context_1.ctcon('Pair', kinds_1.kfuns(typechecker_1.ktype, typechecker_1.ktype, typechecker_1.ktype)), context_1.cvar('pair', types_1.tforalls([['a', typechecker_1.ktype], ['b', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('a'), types_1.tvar('b'), types_1.tapps(types_1.tcon('Pair'), types_1.tvar('a'), types_1.tvar('b'))))), context_1.cvar('fst', types_1.tforalls([['a', typechecker_1.ktype], ['b', typechecker_1.ktype]], types_1.tfuns(types_1.tapps(types_1.tcon('Pair'), types_1.tvar('a'), types_1.tvar('b')), types_1.tvar('a')))), context_1.cvar('snd', types_1.tforalls([['a', typechecker_1.ktype], ['b', typechecker_1.ktype]], types_1.tfuns(types_1.tapps(types_1.tcon('Pair'), types_1.tvar('a'), types_1.tvar('b')), types_1.tvar('b')))), context_1.ctcon('Sum', kinds_1.kfuns(typechecker_1.ktype, typechecker_1.ktype, typechecker_1.ktype)), context_1.cvar('inl', types_1.tforalls([['a', typechecker_1.ktype], ['b', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('a'), types_1.tapps(types_1.tcon('Sum'), types_1.tvar('a'), types_1.tvar('b'))))), context_1.cvar('inr', types_1.tforalls([['a', typechecker_1.ktype], ['b', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('b'), types_1.tapps(types_1.tcon('Sum'), types_1.tvar('a'), types_1.tvar('b'))))), context_1.cvar('match', types_1.tforalls([['a', typechecker_1.ktype], ['b', typechecker_1.ktype], ['c', typechecker_1.ktype]], types_1.tfuns(types_1.tfuns(types_1.tvar('a'), types_1.tvar('c')), types_1.tfuns(types_1.tvar('b'), types_1.tvar('c')), types_1.tapps(types_1.tcon('Sum'), types_1.tvar('a'), types_1.tvar('b')), types_1.tvar('c')))), context_1.ctcon('List', kinds_1.kfuns(typechecker_1.ktype, typechecker_1.ktype)), context_1.cvar('nil', types_1.tforalls([['a', typechecker_1.ktype]], types_1.tapps(types_1.tcon('List'), types_1.tvar('a')))), context_1.cvar('cons', types_1.tforalls([['a', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('a'), types_1.tapps(types_1.tcon('List'), types_1.tvar('a')), types_1.tapps(types_1.tcon('List'), types_1.tvar('a'))))), context_1.cvar('fold', types_1.tforalls([['t', typechecker_1.ktype], ['r', typechecker_1.ktype]], types_1.tfuns(types_1.tvar('r'), types_1.tfuns(types_1.tvar('r'), types_1.tvar('t'), types_1.tvar('r')), types_1.tapps(types_1.tcon('List'), types_1.tvar('t')), types_1.tvar('r')))));
function show(x) {
    if (x === null)
        return `()`;
    if (Array.isArray(x))
        return `[${x.map(show).join(', ')}]`;
    if (typeof x === 'function')
        return `[Function]`;
    if (x._tag === 'inl')
        return `Inl ${show(x._val)}`;
    if (x._tag === 'inr')
        return `Inr ${show(x._val)}`;
    if (x._tag === 'pair')
        return `(${show(x._fst)}, ${x._snd})`;
    return `${x}`;
}
let ctx = exports.context;
function run(i, cb) {
    const cmd = i.trim().toLowerCase();
    if (cmd === ':help') {
        cb('commands :help :context :let');
    }
    else if (cmd === ':context') {
        cb(ctx.elems.join('\n'));
    }
    else if (cmd.slice(0, 4) === ':let') {
        const rest = i.slice(4).trim();
        const j = rest.indexOf('=');
        if (j < 0)
            return cb('= not found', true);
        const spl = rest.split('=');
        const name = spl[0].trim();
        if (name.length === 0 || !/[a-z][a-zA-Z0-9]*/.test(name))
            return cb('invalid name', true);
        const expr = spl[1].trim();
        if (expr.length === 0)
            return cb('invalid expression', true);
        try {
            const p = parser_1.parse(expr);
            console.log('' + p);
            const tr = typechecker_1.infer(ctx, p);
            if (Result_1.isErr(tr))
                throw tr.err;
            else if (Result_1.isOk(tr)) {
                const c = compilerJS_1.default(p);
                console.log(c);
                const res = eval(`(typeof global === 'undefined'? window: global)['${name}'] = ${c}`);
                ctx = ctx.add(context_1.cvar(name, tr.val.ty));
                cb(`${name} : ${tr.val.ty} = ${show(res)}`);
            }
        }
        catch (e) {
            cb('' + e, true);
        }
        ;
    }
    else {
        try {
            const p = parser_1.parse(i);
            console.log('' + p);
            const tr = typechecker_1.infer(ctx, p);
            if (Result_1.isErr(tr))
                throw tr.err;
            else if (Result_1.isOk(tr)) {
                const c = compilerJS_1.default(p);
                console.log(c);
                const res = eval(c);
                cb(`${show(res)} : ${tr.val.ty}`);
            }
        }
        catch (e) {
            cb('' + e, true);
        }
    }
}
exports.default = run;

},{"./Result":1,"./compilerJS":2,"./context":3,"./kinds":5,"./parser":6,"./typechecker":8,"./types":9}],8:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const Result_1 = require("./Result");
const util_1 = require("./util");
const types_1 = require("./types");
const context_1 = require("./context");
const exprs_1 = require("./exprs");
const kinds_1 = require("./kinds");
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
// fresh
const fresh = (ns, n) => {
    const l = ns.length;
    for (let i = 0; i < l; i++) {
        if (ns[i] === n) {
            const j = n.indexOf('$');
            if (j < 0)
                return fresh(ns, `${n}$0`);
            return fresh(ns, `${n.slice(0, j)}\$${(+n.slice(j + 1)) + 1}`);
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
    //console.log(`kindWF ${kind} in ${ctx}`);
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
    //console.log(`contextWF ${ctx}`);
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
    console.log(`subtype ${a} and ${b} in ${ctx}`);
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
    console.log(`solve ${name} and ${ty} in ${ctx}`);
    if (ty.isMono()) {
        const s = ctx.split(context_1.isCTEx(name));
        return typeWF(s.left, ty)
            .then(k => ok(s.left.add(context_1.csolved(name, k, ty)).append(s.right)));
    }
    else
        return err(`polymorphic type in solve: ${name} := ${ty} in ${ctx}`);
}
function instL(ctx, a, b) {
    console.log(`instL ${a} and ${b} in ${ctx}`);
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
    console.log(`instR ${a} and ${b} in ${ctx}`);
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
    console.log(`synth ${e} in ${ctx}`);
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
    console.log(`checkTy ${e} and ${ty} in ${ctx}`);
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
    console.log(`synthapp ${ty} and ${e} in ${ctx}`);
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
        .then(({ ctx: ctx_, ty }) => contextWF(ctx_)
        .then(() => {
        const ty_ = ctx_.apply(ty);
        return typeWF(ctx_, ty_).then(k => checkKindType(k).then(() => {
            if (ctx_.isComplete())
                return ok({ ctx: ctx_, ty: ty_ });
            const u = orderedTExs(ctx_.unsolved(), ty_);
            return ok({
                ctx: ctx_,
                ty: types_1.tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, types_1.tvar(n)), ty_)),
            });
        }));
    }));
}
exports.infer = infer;

},{"./Result":1,"./context":3,"./exprs":4,"./kinds":5,"./types":9,"./util":10}],9:[function(require,module,exports){
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
    texs() {
        return [];
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
    texs() {
        return [];
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
    texs() {
        return [this.name];
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
    texs() {
        return this.left.texs().concat(this.right.texs());
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
    texs() {
        return this.left.texs().concat(this.right.texs());
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
    texs() {
        return this.type.texs();
    }
}
exports.TForall = TForall;
exports.tforall = (name, kind, type) => new TForall(name, kind, type);
exports.tforalls = (ns, type) => ns.reduceRight((a, b) => exports.tforall(b[0], b[1], a), type);

},{}],10:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function err(msg) { throw new Error(msg); }
exports.err = err;
exports.impossible = () => err('impossible');

},{}],11:[function(require,module,exports){
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

},{"./repl":7}]},{},[11]);

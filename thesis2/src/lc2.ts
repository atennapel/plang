import { Result, Ok, Err } from './Result';

type Name = string;

// result
type InferResult<T> = Result<TypeError, T>;
function ok<T>(t: T): InferResult<T> { return Result.ok(t) }
function err<T>(m: string): InferResult<T> { return Result.err(new TypeError(m)) }

// contexts
abstract class Elem {
  abstract toString(): string;
  abstract equals(e: Elem): boolean;
}

class CVar extends Elem {
  readonly name: Name;
  readonly type: Type;

  constructor(name: Name, type: Type) {
    super();
    this.name = name;
    this.type = type;
  }

  toString(): string {
    return `${this.name} : ${this.type}`;
  }
  equals(e: Elem): boolean {
    return e instanceof CVar && this.name === e.name && this.type.equals(e.type);
  }
}
function cvar(name: Name, type: Type) { return new CVar(name, type) }

class CTCon extends Elem {
  readonly name: Name;
  readonly kind: Kind;

  constructor(name: Name, kind: Kind) {
    super();
    this.name = name;
    this.kind = kind;
  }

  toString(): string {
    return `${this.name} :: ${this.kind}`;
  }
  equals(e: Elem): boolean {
    return e instanceof CTCon && this.name === e.name && this.kind.equals(e.kind);
  }
}
function ctcon(name: Name, kind: Kind) { return new CTCon(name, kind) }

class CTVar extends Elem {
  readonly name: Name;
  readonly kind: Kind;

  constructor(name: Name, kind: Kind) {
    super();
    this.name = name;
    this.kind = kind;
  }

  toString(): string {
    return `${this.name} :: ${this.kind}`;
  }
  equals(e: Elem): boolean {
    return e instanceof CTVar && this.name === e.name && this.kind.equals(e.kind);
  }
}
function ctvar(name: Name, kind: Kind) { return new CTVar(name, kind) }

class Context {
  readonly elems: Elem[];

  constructor(elems: Elem[]) {
    this.elems = elems;
  }

  static empty() {
    return new Context([]);
  }

  toString(): string {
    return `[${this.elems.join(', ')}]`;
  }

  add(e: Elem): Context {
    return new Context(this.elems.concat([e]));
  }

  findIndex(fn: (x: Elem) => boolean): number {
    const a = this.elems;
    for(let i = a.length - 1; i >= 0; i--) {
      if(fn(a[i])) return i;
    }
    return -1;
  }

  findVar(x: Name): InferResult<Type> {
    const i = this.findIndex(e => e instanceof CVar && e.name === x);
    if(i < 0) return err(`Var ${x} not found in context: ${this}`);
    return ok((this.elems[i] as CVar).type);
  }
  findTVar(x: Name): InferResult<Kind> {
    const i = this.findIndex(e => e instanceof CTVar && e.name === x);
    if(i < 0) return err(`TVar ${x} not found in context: ${this}`);
    return ok((this.elems[i] as CTVar).kind);
  }
  findTCon(x: Name): InferResult<Kind> {
    const i = this.findIndex(e => e instanceof CTCon && e.name === x);
    if(i < 0) return err(`TCon ${x} not found in context: ${this}`);
    return ok((this.elems[i] as CTCon).kind);
  }
}

// kinds
abstract class Kind {
  abstract toString(): string;
  abstract equals(e: Kind): boolean;
}

class KCon extends Kind {
  readonly name: Name;

  constructor(name: Name) {
    super();
    this.name = name;
  }

  toString(): string {
    return this.name;
  }
  equals(k: Kind): boolean {
    return k instanceof KCon && this.name === k.name;
  }
}
function kcon(name: Name) { return new KCon(name) }

const ktype = kcon('Type');
const krow = kcon('Row');

class KArr extends Kind {
  readonly left: Kind;
  readonly right: Kind;

  constructor(left: Kind, right: Kind) {
    super();
    this.left = left;
    this.right = right;
  }

  toString(): string {
    return `(${this.left} -> ${this.right})`;
  }
  equals(k: Kind): boolean {
    return k instanceof KArr &&
      this.left.equals(k.left) &&
      this.right.equals(k.right);
  }
}
function karr(left: Kind, right: Kind) {
  return new KArr(left, right);
}
function karrs(...ts: Kind[]) {
  return ts.reduceRight((x, y) => karr(y, x));
}

// types
abstract class Type {
  abstract toString(): string;
  abstract equals(e: Type): boolean;
  abstract equivalent(e: Type): boolean;

  abstract open(e: Type, k?: number): Type;
  abstract close(x: Name, k?: number): Type;

  abstract normalize(): Type;

  abstract wellformed(ctx: Context): InferResult<Type>;
  abstract infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}>;

  openVar(name: Name, k: number = 0): Type {
    return this.open(tfree(name), k);
  }
  subst(x: Name, e: Type): Type {
    return this.close(x).open(e);
  }

  flatten(m: {[key: string]: Type[]} = {}): { map: {[key: string]: Type[]}, rest: Type } {
    return { map: m, rest: this };
  }

  betaEquivalent(o: Type): boolean {
    return this.normalize().equivalent(o.normalize());
  }
}

class TBound extends Type {
  readonly index: number;
  
  constructor(index: number) {
    super();
    this.index = index;
  }

  toString() {
    return `'${this.index}`;
  }
  equals(e: Type): boolean {
    return e instanceof TBound && this.index === e.index;
  }
  equivalent(e: Type): boolean {
    return e instanceof TBound && this.index === e.index;
  }

  open(e: Type, k: number = 0): Type {
    return this.index === k? e: this;
  }
  close(x: Name, k: number = 0): Type {
    return this;
  }

  normalize(): Type {
    return this;
  }

  wellformed(ctx: Context): InferResult<Type> {
    return ok(this);
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return err(`tbound in infer: ${this} in ${ctx}`);
  }
}
function tbound(index: number) { return new TBound(index) }

class TFree extends Type {
  readonly name: Name;
  
  constructor(name: Name) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }
  equals(e: Type): boolean {
    return e instanceof EFree && this.name === e.name;
  }
  equivalent(e: Type): boolean {
    return e instanceof EFree && this.name === e.name;
  }

  open(e: Type, k: number = 0): Type {
    return this;
  }
  close(x: Name, k: number = 0): Type {
    return this.name === x? tbound(k): this;
  }

  normalize(): Type {
    return this;
  }

  wellformed(ctx: Context): InferResult<Type> {
    return ctx.findTVar(this.name).map(_ => this);
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return ctx.findTVar(this.name).map(kind => ({kind, ctx}));
  }
}
function tfree(name: Name) { return new TFree(name) }

class TCon extends Type {
  readonly name: Name;

  constructor(name: Name) {
    super();
    this.name = name;
  }

  toString(): string {
    return this.name;
  }
  equals(t: Type): boolean {
    return t instanceof TCon && this.name === t.name;
  }
  equivalent(t: Type): boolean {
    return t instanceof TCon && this.name === t.name;
  }

  open(e: Type, k: number = 0): Type {
    return this;
  }
  close(x: Name, k: number = 0): Type {
    return this;
  }

  normalize(): Type {
    return this;
  }

  wellformed(ctx: Context): InferResult<Type> {
    return ctx.findTCon(this.name).map(_ => this);
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return ctx.findTCon(this.name).map(kind => ({kind, ctx}));
  }
}
function tcon(name: Name) { return new TCon(name) }

const trecord = tcon('Rec');
const tvariant = tcon('Var');

class TArr extends Type {
  readonly left: Type;
  readonly right: Type;

  constructor(left: Type, right: Type) {
    super();
    this.left = left;
    this.right = right;
  }

  toString(): string {
    return `(${this.left} -> ${this.right})`;
  }
  equals(t: Type): boolean {
    return t instanceof TArr &&
      this.left.equals(t.left) &&
      this.right.equals(t.right);
  }
  equivalent(t: Type): boolean {
    return t instanceof TArr &&
      this.left.equivalent(t.left) &&
      this.right.equivalent(t.right);
  }

  open(e: Type, k: number = 0): Type {
    return tarr(
      this.left.open(e, k),
      this.right.open(e, k)
    );
  }
  close(x: Name, k: number = 0): Type {
    return tarr(
      this.left.close(x, k),
      this.right.close(x, k)
    );
  }

  normalize(): Type {
    return tarr(this.left.normalize(), this.right.normalize());
  }

  wellformed(ctx: Context): InferResult<Type> {
    return this.left.wellformed(ctx).then(l =>  this.right.wellformed(ctx).map(r => tarr(l, r)));
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return this.left.infer(ctx)
      .then(({kind: kleft, ctx}) => this.right.infer(ctx)
      .then(({kind: kright, ctx}) => {
        if(!kleft.equals(ktype))
          return err(`left side of arrow has invalid kind in ${this}, ${ctx}`);
        if(!kright.equals(ktype))
          return err(`right side of arrow has invalid kind in ${this}, ${ctx}`);
        return ok({ctx, kind: ktype});
      }));
  }
}
function tarr(left: Type, right: Type) {
  return new TArr(left, right);
}
function tarrs(...ts: Type[]) {
  return ts.reduceRight((x, y) => tarr(y, x));
}

class TAll extends Type {
  readonly kind: Kind;
  readonly type: Type;

  constructor(kind: Kind, type: Type) {
    super();
    this.kind = kind;
    this.type = type;
  }

  toString(): string {
    return `(forall:${this.kind} . ${this.type})`;
  }
  equals(t: Type): boolean {
    return t instanceof TAll &&
      this.kind.equals(t.kind) &&
      this.type.equals(t.type);
  }
  equivalent(t: Type): boolean {
    return t instanceof TAll &&
      this.kind.equals(t.kind) &&
      this.type.equivalent(t.type);
  }

  open(e: Type, k: number = 0): Type {
    return tall(this.kind, this.type.open(e, k + 1));
  }
  close(x: Name, k: number = 0): Type {
    return tall(this.kind, this.type.close(x, k + 1));
  }

  normalize(): Type {
    const x = fresh();
    return tall(this.kind, this.type.openVar(x).normalize().close(x));
  }

  wellformed(ctx: Context): InferResult<Type> {
    return this.type.wellformed(ctx).map(t => tall(this.kind, t));
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    const x = fresh();
    return this.type.openVar(x).infer(ctx.add(ctvar(x, this.kind)));
  }
}
function tall(kind: Kind, type: Type) {
  return new TAll(kind, type);
}

class TExist extends Type {
  readonly kind: Kind;
  readonly type: Type;

  constructor(kind: Kind, type: Type) {
    super();
    this.kind = kind;
    this.type = type;
  }

  toString(): string {
    return `(exists:${this.kind} . ${this.type})`;
  }
  equals(t: Type): boolean {
    return t instanceof TExist &&
      this.kind.equals(t.kind) &&
      this.type.equals(t.type);
  }
  equivalent(t: Type): boolean {
    return t instanceof TExist &&
      this.kind.equals(t.kind) &&
      this.type.equivalent(t.type);
  }

  open(e: Type, k: number = 0): Type {
    return texist(this.kind, this.type.open(e, k + 1));
  }
  close(x: Name, k: number = 0): Type {
    return texist(this.kind, this.type.close(x, k + 1));
  }

  normalize(): Type {
    const x = fresh();
    return texist(this.kind, this.type.openVar(x).normalize().close(x));
  }

  wellformed(ctx: Context): InferResult<Type> {
    return this.type.wellformed(ctx).map(t => texist(this.kind, t));
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    const x = fresh();
    return this.type.openVar(x).infer(ctx.add(ctvar(x, this.kind)));
  }
}
function texist(kind: Kind, type: Type) {
  return new TExist(kind, type);
}

class TAbs extends Type {
  readonly kind: Kind;
  readonly type: Type;

  constructor(kind: Kind, type: Type) {
    super();
    this.kind = kind;
    this.type = type;
  }

  toString(): string {
    return `(\\:${this.kind}.${this.type})`;
  }
  equals(t: Type): boolean {
    return t instanceof TAbs &&
      this.kind.equals(t.kind) &&
      this.type.equals(t.type);
  }
  equivalent(t: Type): boolean {
    return t instanceof TAbs &&
      this.kind.equals(t.kind) &&
      this.type.equivalent(t.type);
  }

  open(e: Type, k: number = 0): Type {
    return tabs(this.kind, this.type.open(e, k + 1));
  }
  close(x: Name, k: number = 0): Type {
    return tabs(this.kind, this.type.close(x, k + 1));
  }

  normalize(): Type {
    const x = fresh();
    return tabs(this.kind, this.type.openVar(x).normalize().close(x));
  }

  wellformed(ctx: Context): InferResult<Type> {
    return this.type.wellformed(ctx).map(t => tabs(this.kind, t));
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    const x = fresh();
    return this.type.openVar(x).infer(ctx.add(ctvar(x, this.kind)))
      .map(({kind}) => ({kind: karr(this.kind, kind), ctx}));
  }
}
function tabs(kind: Kind, type: Type) {
  return new TAbs(kind, type);
}

class TExtend extends Type {
  readonly label: Name;
  readonly type: Type;
  readonly rest: Type;

  constructor(label: Name, type: Type, rest: Type) {
    super();
    this.label = label;
    this.type = type;
    this.rest = rest;
  }

  toString(): string {
    return `{${this.label} : ${this.type} | ${this.rest})`;
  }
  equals(t: Type): boolean {
    return t instanceof TExtend &&
      this.label === t.label &&
      this.type.equals(t.type) &&
      this.rest.equals(t.rest);
  }
  
  flatten(m: {[key: string]: Type[]} = {}): { map: {[key: string]: Type[]}, rest: Type } {
    if(!m[this.label]) m[this.label] = [];
    m[this.label].push(this.type);
    const { map, rest } = this.rest.flatten(m);
    return { map, rest };
  }

  equivalent(o: Type): boolean {
    if(!(o instanceof TExtend)) return false;
    const a = this.flatten();
    const b = o.flatten();
    const ka = Object.keys(a);
    const kb = Object.keys(b);
    if(ka.length !== kb.length) return false;
    for(let i = 0, l = ka.length; i < l; i++) {
      if(ka[i] !== kb[i]) return false;
    }
    for(var k in a.map) {
      const av = a.map[k];
      const bv = b.map[k];
      if(av.length !== bv.length) return false;
      for(let i = 0, l = av.length; i < l; i++) {
        if(!av[i].equivalent(bv[i])) return false;
      }
    }
    return a.rest.equivalent(b.rest);
  }

  open(e: Type, k: number = 0): Type {
    return textend(
      this.label,
      this.type.open(e, k),
      this.rest.open(e, k)
    );
  }
  close(x: Name, k: number = 0): Type {
    return textend(
      this.label,
      this.type.close(x, k),
      this.rest.close(x, k)
    );
  }

  normalize(): Type {
    return textend(
      this.label,
      this.type.normalize(),
      this.rest.normalize()
    );
  }

  wellformed(ctx: Context): InferResult<Type> {
    return this.type.wellformed(ctx).then(type => this.rest.wellformed(ctx).map(rest => textend(this.label, type, rest)));
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return this.type.infer(ctx)
      .then(({kind: ktype_, ctx}) => this.rest.infer(ctx)
      .then(({kind: krest, ctx}) => {
        if(!ktype_.equals(ktype))
          return err(`type in row must be ${ktype} in ${this} in ${ctx}`);
        if(!krest.equals(krow))
          return err(`rest in row must be ${krow} in ${this} in ${ctx}`);
        return ok({ctx, kind: krow});
      }));
  }
}
function textend(label: Name, type: Type, rest: Type) {
  return new TExtend(label, type, rest);
}

class TEmpty extends Type {
  constructor() {
    super();
  }

  toString(): string {
    return '{}';
  }
  equals(t: Type): boolean {
    return t instanceof TEmpty;
  }
  equivalent(t: Type): boolean {
    return t instanceof TEmpty;
  }

  open(e: Type, k: number = 0): Type {
    return this;
  }
  close(x: Name, k: number = 0): Type {
    return this;
  }

  normalize(): Type {
    return this;
  }

  wellformed(ctx: Context): InferResult<Type> {
    return ok(this);
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return ok({kind: krow, ctx});
  }
}
const tempty = new TEmpty();

class TLacks extends Type {
  readonly label: Name;

  constructor(label: Name) {
    super();
    this.label = label;
  }

  toString(): string {
    return `Lacks ${this.label}`;
  }
  equals(t: Type): boolean {
    return t instanceof TLacks && this.label === t.label;
  }
  equivalent(t: Type): boolean {
    return t instanceof TLacks && this.label === t.label;
  }

  open(e: Type, k: number = 0): Type {
    return this;
  }
  close(x: Name, k: number = 0): Type {
    return this;
  }

  normalize(): Type {
    return this;
  }

  wellformed(ctx: Context): InferResult<Type> {
    return ok(this);
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return ok({kind: karr(krow, ktype), ctx});
  }
}
function tlacks(label: Name) {
  return new TLacks(label);
}

class TApp extends Type {
  readonly left: Type;
  readonly right: Type;

  constructor(left: Type, right: Type) {
    super();
    this.left = left;
    this.right = right;
  }

  toString(): string {
    return `(${this.left} ${this.right})`;
  }
  equals(t: Type): boolean {
    return t instanceof TApp &&
      this.left.equals(t.left) &&
      this.right.equals(t.right);
  }
  equivalent(t: Type): boolean {
    return t instanceof TApp &&
      this.left.equivalent(t.left) &&
      this.right.equivalent(t.right);
  }

  open(e: Type, k: number = 0): Type {
    return tapp(
      this.left.open(e, k),
      this.right.open(e, k)
    );
  }
  close(x: Name, k: number = 0): Type {
    return tapp(
      this.left.close(x, k),
      this.right.close(x, k)
    );
  }

  normalize(): Type {
    const l = this.left.normalize();
    const r = this.right.normalize();
    if(l instanceof TAbs)
      return l.type.open(r).normalize();
    return tapp(l, r);
  }

  wellformed(ctx: Context): InferResult<Type> {
    return this.left.wellformed(ctx).then(l =>  this.right.wellformed(ctx).map(r => tapp(l, r)));
  }
  infer(ctx: Context): InferResult<{kind: Kind, ctx: Context}> {
    return this.left.infer(ctx)
      .then(({kind: kleft, ctx}) => this.right.infer(ctx)
      .then(({kind: kright, ctx}) => {
        if(kleft instanceof KArr) {
          if(kleft.left.equals(kright)) {
            return ok({kind: kleft.right, ctx});
          } else return err(`invalid tapp, right side has invalid kind in ${this}, ${ctx}`);
        } else return err(`invalid tapp, left side has invalid kind in ${this}, ${ctx}`);
      }));
  }
}
function tapp(left: Type, right: Type) {
  return new TApp(left, right);
}
function tapps(...ts: Type[]) {
  return ts.reduce(tapp);
}

// exprs
abstract class Expr {
  abstract toString(): string;
  abstract equals(e: Expr): boolean;

  abstract open(e: Expr, k?: number): Expr;
  abstract close(x: Name, k?: number): Expr;
  abstract openType(x: Name, k?: number): Expr;

  abstract normalize(): Expr;

  abstract infer(c: Context): InferResult<{type: Type, ctx: Context}>;

  openVar(name: Name, k: number = 0): Expr {
    return this.open(efree(name), k);
  }
  subst(x: Name, e: Expr): Expr {
    return this.close(x).open(e);
  }
}

class EBound extends Expr {
  readonly index: number;
  
  constructor(index: number) {
    super();
    this.index = index;
  }

  toString() {
    return `'${this.index}`;
  }
  equals(e: Expr): boolean {
    return e instanceof EBound && this.index === e.index;
  }

  open(e: Expr, k: number = 0): Expr {
    return this.index === k? e: this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(c: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${c}`);
    return err(`Bound variable encountered: ${this}`);
  }
}
function ebound(index: number) { return new EBound(index) }

class EFree extends Expr {
  readonly name: Name;
  
  constructor(name: Name) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }
  equals(e: Expr): boolean {
    return e instanceof EFree && this.name === e.name;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this.name === x? ebound(k): this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(c: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${c}`);
    return c.findVar(this.name).map(type => ({type, ctx: c}));
  }
}
function efree(name: Name) { return new EFree(name) }

let freshN = 0;
function fresh(): Name { return `\$${freshN++}` }

class EAbs extends Expr {
  readonly type: Type;
  readonly expr: Expr;

  constructor(type: Type, expr: Expr) {
    super();
    this.type = type;
    this.expr = expr;
  }

  toString() {
    return `(\\:${this.type}.${this.expr})`;
  }
  equals(e: Expr): boolean {
    return e instanceof EAbs && this.type.equals(e.type) && this.expr.equals(e.expr);
  }

  open(e: Expr, k: number = 0): Expr {
    return eabs(this.type, this.expr.open(e, k + 1));
  }
  close(x: Name, k: number = 0): Expr {
    return eabs(this.type, this.expr.close(x, k + 1));
  }
  openType(x: Name, k: number = 0): Expr {
    return eabs(this.type.openVar(x, k), this.expr.openType(x, k));
  }

  normalize(): Expr {
    const x = fresh();
    return eabs(this.type.normalize(), this.expr.openVar(x).normalize().close(x));
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    return this.type.wellformed(ctx)
      .then(abstype => abstype.infer(ctx)
      .then(({kind, ctx}) => {
        if(!kind.equals(ktype))
          return err(`kind must be ${ktype} in abs but ${kind} found in ${this} in ${ctx}`);
        const x = fresh();
        return this.expr.openVar(x).infer(ctx.add(cvar(x, abstype)))
          .map(({type}) => ({type: tarr(abstype, type), ctx}));
      }));
  }
}
function eabs(type: Type, expr: Expr): Expr { return new EAbs(type, expr) }

class EApp extends Expr {
  readonly left: Expr;
  readonly right: Expr;

  constructor(left: Expr, right: Expr) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} ${this.right})`;
  }
  equals(e: Expr): boolean {
    return e instanceof EApp &&
      this.left.equals(e.left) &&
      this.right.equals(e.right);
  }

  open(e: Expr, k: number = 0): Expr {
    return eapp(
      this.left.open(e, k),
      this.right.open(e, k)
    );
  }
  close(x: Name, k: number = 0): Expr {
    return eapp(
      this.left.close(x, k),
      this.right.close(x, k)
    );
  }
  openType(x: Name, k: number = 0): Expr {
    return eapp(this.left.openType(x, k), this.right.openType(x, k));
  }

  normalize(): Expr {
    const l = this.left.normalize();
    const r = this.right.normalize();
    if(l instanceof EAbs)
      return l.expr.open(r).normalize();
    return eapp(l, r);
  }

  infer(c: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${c}`);
    return this.left.infer(c)
      .then(({type: tleft_, ctx}) => this.right.infer(ctx)
      .then(({type: tright, ctx}) => {
        const tleft = tleft_.normalize();
        if(tleft instanceof TArr) {
          if(tleft.left.betaEquivalent(tright)) return ok({type: tleft.right, ctx});
          return err(`invalid application: ${this}, ${tleft.left} ~ ${tright}, ${ctx}`);
        } else return err(`invalid application: ${this}, ${tleft}, ${tright}, ${ctx}`);
      }));
  }
}
function eapp(...es: Expr[]) {
  return es.reduce((x, y) => new EApp(x, y));
}

class ETAbs extends Expr {
  readonly kind: Kind;
  readonly expr: Expr;

  constructor(kind: Kind, expr: Expr) {
    super();
    this.kind = kind;
    this.expr = expr;
  }

  toString() {
    return `(/:${this.kind}.${this.expr})`;
  }
  equals(e: Expr): boolean {
    return e instanceof ETAbs && this.kind.equals(e.kind) && this.expr.equals(e.expr);
  }

  open(e: Expr, k: number = 0): Expr {
    return etabs(this.kind, this.expr.open(e, k + 1));
  }
  close(x: Name, k: number = 0): Expr {
    return etabs(this.kind, this.expr.close(x, k + 1));
  }
  openType(x: Name, k: number = 0): Expr {
    return etabs(this.kind, this.expr.openType(x, k + 1));
  }

  normalize(): Expr {
    return etabs(this.kind, this.expr.normalize());
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    const x = fresh();
    return this.expr.openType(x).infer(ctx.add(ctvar(x, this.kind)))
      .map(({type}) => ({type: tall(this.kind, type.close(x)), ctx}));
  }
}
function etabs(kind: Kind, expr: Expr): Expr { return new ETAbs(kind, expr) }

class ETApp extends Expr {
  readonly left: Expr;
  readonly right: Type;

  constructor(left: Expr, right: Type) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} [${this.right}])`;
  }
  equals(e: Expr): boolean {
    return e instanceof ETApp &&
      this.left.equals(e.left) &&
      this.right.equals(e.right);
  }

  open(e: Expr, k: number = 0): Expr {
    return etapp(
      this.left.open(e, k),
      this.right
    );
  }
  close(x: Name, k: number = 0): Expr {
    return etapp(
      this.left.close(x, k),
      this.right
    );
  }
  openType(x: Name, k: number = 0): Expr {
    return etapp(this.left.openType(x, k + 1), this.right.openVar(x, k + 1));
  }

  normalize(): Expr {
    return etapp(this.left.normalize(), this.right.normalize());
  }

  infer(c: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${c}`);
    return this.right.wellformed(c)
      .then(tright => this.left.infer(c)
      .then(({type: tleft_, ctx}) => tright.infer(ctx)
      .then(({kind, ctx}) => {
        const tleft = tleft_.normalize();
        if(tleft instanceof TAll) {
          if(!kind.equals(tleft.kind))
            return err(`kind mismatch: ${kind} ~ ${tleft.kind}, ${ctx}`);
          return ok({ctx, type: tleft.type.open(tright)});
        } else return err(`invalid tapp: ${tleft}, ${this}, ${c}`);
      })));
  }
}
function etapp(e: Expr, ...ts: Type[]): Expr {
  return ts.reduce((x: any, y) => new ETApp(x, y), e) as Expr;
}

class EPack extends Expr {
  readonly type: Type;
  readonly expr: Expr;
  readonly anno: Type;

  constructor(type: Type, expr: Expr, anno: Type) {
    super();
    this.type = type;
    this.expr = expr;
    this.anno = anno;
  }

  toString() {
    return `(${this.type} * ${this.expr} : ${this.anno})`;
  }
  equals(e: Expr): boolean {
    return e instanceof EPack &&
      this.type.equals(e.type) &&
      this.expr.equals(e.expr) &&
      this.anno.equals(e.anno);
  }

  open(e: Expr, k: number = 0): Expr {
    return epack(
      this.type,
      this.expr.open(e, k),
      this.anno
    );
  }
  close(x: Name, k: number = 0): Expr {
    return epack(
      this.type,
      this.expr.close(x, k),
      this.anno
    );
  }
  openType(x: Name, k: number = 0): Expr {
    return epack(
      this.type.openVar(x, k),
      this.expr.openType(x, k),
      this.anno.openVar(x, k)
    );
  }

  normalize(): Expr {
    return epack(this.type.normalize(), this.expr.normalize(), this.anno.normalize());
  }

  infer(c: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${c}`);
    return this.type.wellformed(ctx)
      .then(type => this.anno.wellformed(ctx)
      .then(anno_ => this.type.infer(ctx)
      .then(({kind: ktype_, ctx}) => this.expr.infer(ctx)
      .then(({type: texpr, ctx}) => {
        const anno = anno_.normalize();
        if(anno instanceof TExist) {
          if(!ktype_.equals(anno.kind))
            return err(`kind mismatch: ${ktype_} and ${anno.kind} in ${this} in ${ctx}`);
          const a = anno.type.open(type);
          if(a.betaEquivalent(texpr))
            return ok({ctx, type: anno});
          return err(`existential type mismatch: ${a} ~ ${texpr} in ${this} in ${ctx}`);
        } else return err(`annoted type in pack is not an existential type: ${anno} in ${this} in ${ctx}`);
      }))));
  }
}
function epack(type: Type, expr: Expr, anno: Type) {
  return new EPack(type, expr, anno);
}

class EUnpack extends Expr {
  readonly val: Expr;
  readonly expr: Expr;

  constructor(val: Expr, expr: Expr) {
    super();
    this.val = val;
    this.expr = expr;
  }

  toString() {
    return `(unpack ${this.val} in ${this.expr})`;
  }
  equals(e: Expr): boolean {
    return e instanceof EUnpack &&
      this.val.equals(e.val) &&
      this.expr.equals(e.expr);
  }

  open(e: Expr, k: number = 0): Expr {
    return eunpack(
      this.val.open(e, k),
      this.expr.open(e, k)
    );
  }
  close(x: Name, k: number = 0): Expr {
    return eunpack(
      this.val.close(x, k),
      this.expr.close(x, k)
    );
  }
  openType(x: Name, k: number = 0): Expr {
    return eunpack(
      this.val.openType(x, k + 1),
      this.expr.openType(x, k + 1)
    );
  }

  normalize(): Expr {
    return eunpack(this.val.normalize(), this.expr.normalize());
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    const X = fresh();
    const x = fresh();
    return this.val.openType(X).infer(ctx)
      .then(({type: tval_, ctx}) => {
        const tval = tval_.normalize();
        if(tval instanceof TExist) {
          const nctx = ctx.add(ctvar(X, tval.kind)).add(cvar(x, tval.type));
          return this.expr.openVar(x).infer(nctx);
        } else return err(`annoted type in pack is not an existential type: ${tval} in ${this} in ${ctx}`);
      });
  }
}
function eunpack(val: Expr, expr: Expr) {
  return new EUnpack(val, expr);
}

class EEmpty extends Expr {
  constructor() {
    super();
  }

  toString() {
    return `{}`;
  }
  equals(e: Expr): boolean {
    return e instanceof EEmpty;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    return ok({ctx, type: tapp(trecord, tempty)});
  }
}
const eempty = new EEmpty();

class ESelect extends Expr {
  readonly label: Name;

  constructor(label: Name) {
    super();
    this.label = label;
  }

  toString() {
    return `.${this.label}`;
  }
  equals(e: Expr): boolean {
    return e instanceof ESelect && this.label === e.label;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    // forall (t : Type) (r : Row) . Lacks label r -> {x : t | r} -> t
    const type = tall(ktype, tall(krow, tarrs(
      tapp(tlacks(this.label), tbound(0)),
      tapp(trecord, textend(this.label, tbound(1), tbound(0))), tbound(1)
    )));
    return ok({ctx, type});
  }
}
function eselect(label: Name) {
  return new ESelect(label);
}

class EExtend extends Expr {
  readonly label: Name;

  constructor(label: Name) {
    super();
    this.label = label;
  }

  toString() {
    return `.+${this.label}`;
  }
  equals(e: Expr): boolean {
    return e instanceof EExtend && this.label === e.label;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    // forall (t : Type) (r : Row) . Lacks label r -> t -> Rec r -> Rec {x : t | r}
    const type = tall(ktype, tall(krow, tarrs(
      tapp(tlacks(this.label), tbound(0)),
      tbound(1),
      tapp(trecord, tbound(0)),
      tapp(trecord, textend(this.label, tbound(1), tbound(0)))
    )));
    return ok({ctx, type});
  }
}
function eextend(label: Name) {
  return new EExtend(label);
}

class ERestrict extends Expr {
  readonly label: Name;

  constructor(label: Name) {
    super();
    this.label = label;
  }

  toString() {
    return `.-${this.label}`;
  }
  equals(e: Expr): boolean {
    return e instanceof ERestrict && this.label === e.label;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    // forall (t : Type) (r : Row) . Lacks label r -> Rec {x : t | r} -> Rec r
    const type = tall(ktype, tall(krow, tarrs(
      tapp(tlacks(this.label), tbound(0)),
      tapp(trecord, textend(this.label, tbound(1), tbound(0))),
      tapp(trecord, tbound(0))
    )));
    return ok({ctx, type});
  }
}
function erestrict(label: Name) {
  return new ERestrict(label);
}

class ELacksEmpty extends Expr {
  readonly label: Name;

  constructor(label: Name) {
    super();
    this.label = label;
  }

  toString() {
    return `{}/${this.label}`;
  }
  equals(e: Expr): boolean {
    return e instanceof ELacksEmpty && this.label === e.label;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    // Lacks label {}
    const type = tapp(tlacks(this.label), tempty);
    return ok({ctx, type});
  }
}
function elacksempty(label: Name) {
  return new ELacksEmpty(label);
}

class ELacksDiff extends Expr {
  readonly label1: Name;
  readonly label2: Name;

  constructor(label1: Name, label2: Name) {
    super();
    if(label1 === label2) throw new TypeError(`invalid ELacksDiff: ${label1} and ${label2}`);
    this.label1 = label1;
    this.label2 = label2;
  }

  toString() {
    return `${this.label1}/${this.label2}`;
  }
  equals(e: Expr): boolean {
    return e instanceof ELacksDiff && this.label1 === e.label1 && this.label2 === e.label2;
  }

  open(e: Expr, k: number = 0): Expr {
    return this;
  }
  close(x: Name, k: number = 0): Expr {
    return this;
  }
  openType(x: Name, k: number = 0): Expr {
    return this;
  }

  normalize(): Expr {
    return this;
  }

  infer(ctx: Context): InferResult<{type: Type, ctx: Context}> {
    console.log(`infer ${this} in ${ctx}`);
    // forall (t: Type) (r: Row) . Lacks label1 r -> Lacks label1 { label2 : t | r }
    const type = tall(ktype, tall(krow, tarrs(
      tapp(tlacks(this.label1), tbound(0)),
      tapp(tlacks(this.label1), textend(this.label2, tbound(1), tbound(0))),
    )));
    return ok({ctx, type});
  }
}
function elacksdiff(label1: Name, label2: Name) {
  return new ELacksDiff(label1, label2);
}

// testing
const Vt = tbound;
const Ft = tfree;
const At = tapps;
const R = tall;
const E = texist;

const tbool = tcon('Bool');
const tint = tcon('Int');
const tlist = tcon('List');

const tinstint = tabs(karr(ktype, ktype), tapp(Vt(0), tint));

const V = ebound;
const F = efree;
const L = eabs;
const B = etabs;
const A = eapp;
const T = etapp;
const P = epack;
const U = eunpack;
const S = eselect;
const M = eempty;
const X = eextend;
const C = erestrict;

const ctx = new Context([
  ctcon('Rec', karr(krow, ktype)),
  ctcon('Var', karr(krow, ktype)),
  ctcon('Bool', ktype),
  ctcon('Int', ktype),
  ctcon('List', karr(ktype, ktype)),
  cvar('add', tarrs(tint, tint, tint)),
  cvar('inc', tarr(tint, tint)),
  cvar('iszero', tarr(tint, tbool)),
  cvar('zero', tint),
  cvar('true', tbool),
]);

const e = A(T(S('x'), tint, textend('y', tbool, tempty)), A(T(elacksdiff('x', 'y'), tbool, tempty), elacksempty('x')),
  A(T(X('y'), tbool, textend('x', tint, tempty)), A(T(elacksdiff('y', 'x'), tint, tempty), elacksempty('y')), F('true'),
    A(T(X('x'), tint, tempty), elacksempty('x'), F('zero'), M))
);
console.log('' + e);
console.log('' + e.infer(ctx).map(({type, ctx}) => `${type.normalize()} ; ${ctx}`));
console.log('' + e.normalize());

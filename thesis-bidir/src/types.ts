import Context, {
  cforall,
  cexists,
  CExists,
  CSolved,
  ContextElem,
} from './Context';
import Id from './Id';
import Map from './Map';

export abstract class Type {
  abstract toString(): string;
  abstract isMono(): boolean;
  abstract isWellformed(c: Context): boolean;
  abstract equals(other: Type): boolean;
  abstract apply(c: Context): Type;
  abstract subst(id: Id, t: Type): Type;
  abstract contains(id: Id): boolean;
  abstract free(): Map<Id>;

  substAll(sub: [Id, Type][]): Type {
    return sub.reduce((t, [id, type]) => t.subst(id, type), this);
  }
}

export class TUnit extends Type {
  toString() {
    return '()';
  }

  isMono() {
    return true;
  }

  isWellformed(c: Context) {
    return true;
  }

  equals(other: Type): boolean {
    return other instanceof TUnit;
  }

  apply(c: Context): Type {
    return this;
  }

  subst(id: Id, t: Type): Type {
    return this;
  }

  contains(id: Id): boolean {
    return false;
  }

  free() {
    return Map.empty<Id>();
  }
}
export const tunit = new TUnit();

export class TVar extends Type {
  readonly id: Id;

  constructor(id: Id) {
    super();
    this.id = id;
  }

  toString() {
    return this.id.toString();
  }

  isMono() {
    return true;
  }

  isWellformed(c: Context) {
    return c.contains(cforall(this.id));
  }

  equals(other: Type): boolean {
    return other instanceof TVar && this.id.equals(other.id);
  }

  apply(c: Context): Type {
    return this;
  }

  subst(id: Id, t: Type): Type {
    return this.id.equals(id)? t: this;
  }

  contains(id: Id): boolean {
    return this.id.equals(id);
  }

  free() {
    return Map.empty<Id>();
  }
}
export function tvar(id: Id) {
  return new TVar(id);
}

export class TExists extends Type {
  readonly id: Id;

  constructor(id: Id) {
    super();
    this.id = id;
  }

  toString() {
    return `^${this.id}`;
  }

  isMono() {
    return true;
  }

  isWellformed(c: Context) {
    return c.contains((e: ContextElem) =>
      (e instanceof CExists || e instanceof CSolved) && e.id.equals(this.id));
  }

  equals(other: Type): boolean {
    return other instanceof TExists && this.id.equals(other.id);
  }

  apply(c: Context): Type {
    return c.getOrSolved(this.id, t => t.apply(c), this);
  }

  subst(id: Id, t: Type): Type {
    return this.id.equals(id)? t: this;
  }

  contains(id: Id): boolean {
    return this.id.equals(id);
  }

  free() {
    return Map.of([this.id.id, this.id]);
  }
}
export function texists(id: Id) {
  return new TExists(id);
}

export class TArr extends Type {
  readonly left: Type;
  readonly right: Type;

  constructor(left: Type, right: Type) {
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

  isWellformed(c: Context) {
    return this.left.isWellformed(c) && this.right.isWellformed(c);
  }

  equals(other: Type): boolean {
    return other instanceof TArr && this.left.equals(other.left) && this.right.equals(other.right);
  }

  apply(c: Context): Type {
    return new TArr(this.left.apply(c), this.right.apply(c));
  }

  subst(id: Id, t: Type): Type {
    return new TArr(this.left.subst(id, t), this.right.subst(id, t));
  }

  contains(id: Id): boolean {
    return this.left.contains(id) || this.right.contains(id);
  }

  free() {
    return this.left.free().union(this.right.free());
  }
}
export function tarr(...ts: Type[]) {
  return ts.reduceRight((x, y) => new TArr(y, x));
}

export class TForall extends Type {
  readonly tvar: TVar;
  readonly type: Type;

  constructor(tvar: TVar, type: Type) {
    super();
    this.tvar = tvar;
    this.type = type;
  }

  toString() {
    return `(forall ${this.tvar} . ${this.type})`;
  }

  isMono() {
    return false;
  }

  isWellformed(c: Context) {
    return this.type.isWellformed(c.append(cforall(this.tvar.id)));
  }

  equals(other: Type): boolean {
    return other instanceof TForall && this.tvar.equals(other.tvar) && this.type.equals(other.type);
  }

  apply(c: Context): Type {
    return new TForall(this.tvar, this.type.apply(c));
  }

  subst(id: Id, t: Type): Type {
    return this.tvar.id.equals(id)? this: new TForall(this.tvar, this.type.subst(id, t));
  }

  contains(id: Id): boolean {
    return !this.tvar.id.equals(id) && this.type.contains(id);
  }

  free() {
    return this.type.free();
  }
}
export function tforall(tvs: TVar[], t: Type) {
  return tvs.length === 0? t: tvs.reduceRight((t, tv) => new TForall(tv, t), t);
}

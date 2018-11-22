import Kind from './kinds';
import NameRep, { name } from '../NameRep';

export default abstract class Type {

  abstract toString(): string;

  abstract isMono(): boolean;

  abstract substTVar(name: NameRep, type: ValType): Type;
  abstract substTMeta(name: NameRep, type: ValType): Type;

  abstract containsTMeta(name: NameRep): boolean;

  abstract freeTMeta(): NameRep[];

  abstract equals(that: Type): boolean;

}

export abstract class ValType extends Type {
  private readonly _type: 'ValType';

  abstract substTVar(name: NameRep, type: ValType): ValType;
  abstract substTMeta(name: NameRep, type: ValType): ValType;

  abstract equals(that: ValType): boolean;
} 

export class TVar extends ValType {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  isMono() {
    return true;
  }

  substTVar(name: NameRep, type: ValType): ValType {
    return this.name.equals(name) ? type: this;
  }
  substTMeta(name: NameRep, type: ValType): ValType {
    return this;
  }

  containsTMeta(name: NameRep): boolean {
    return false;
  }

  freeTMeta(): NameRep[] {
    return [];
  }

  equals(that: ValType): boolean {
    return that instanceof TVar && this.name.equals(that.name);
  }

}
export const tvar = (name: NameRep) => new TVar(name);
export const isTVar = (type: Type): type is TVar => type instanceof TVar;

export class TMeta extends ValType {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return `^${this.name}`;
  }

  isMono() {
    return true;
  }

  substTVar(name: NameRep, type: ValType): ValType {
    return this;
  }
  substTMeta(name: NameRep, type: ValType): ValType {
    return this.name.equals(name) ? type: this;
  }

  containsTMeta(name: NameRep): boolean {
    return this.name.equals(name);
  }

  freeTMeta(): NameRep[] {
    return [this.name];
  }

  equals(that: ValType): boolean {
    return that instanceof TMeta && this.name.equals(that.name);
  }

}
export const tmeta = (name: NameRep) => new TMeta(name);
export const isTMeta = (type: Type): type is TMeta => type instanceof TMeta;

export class TApp extends ValType {

  constructor(
    public readonly left: ValType,
    public readonly right: ValType,
  ) { super() }

  toString(): string{
    return `(${this.left} ${this.right})`;
  }

  isMono() {
    return this.left.isMono() && this.right.isMono();
  }

  substTVar(name: NameRep, type: ValType): ValType {
    return new TApp(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }
  substTMeta(name: NameRep, type: ValType): ValType {
    return new TApp(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.left.containsTMeta(name) || this.right.containsTMeta(name);
  }

  freeTMeta(): NameRep[] {
    return this.left.freeTMeta().concat(this.right.freeTMeta());
  }

  equals(that: ValType): boolean {
    return that instanceof TApp && this.left.equals(that.left) && this.right.equals(that.right);
  }

}
export const tapp = (left: ValType, right: ValType) => new TApp(left, right);
export const tappsFrom = (ts: ValType[]) => ts.reduce(tapp);
export const tapps = (...ts: ValType[]) => tappsFrom(ts);
export const isTApp = (type: Type): type is TApp => type instanceof TApp;
export const flattenTApp = (type: ValType): { head: ValType, tail: ValType[] } => {
  if (isTApp(type)) {
    const rec = flattenTApp(type.left);
    return { head: rec.head, tail: rec.tail.concat([type.right]) };
  }
  return { head: type, tail: [] };
};
export const headTApp = (type: ValType): ValType => flattenTApp(type).head;

export class TFun extends ValType {

  constructor(
    public readonly left: ValType,
    public readonly right: TComp,
  ) { super() }

  toString(): string{
    return `(${this.left} -> ${this.right})`;
  }

  isMono() {
    return this.left.isMono() && this.right.isMono();
  }

  substTVar(name: NameRep, type: ValType): ValType {
    return new TFun(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }
  substTMeta(name: NameRep, type: ValType): ValType {
    return new TFun(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.left.containsTMeta(name) || this.right.containsTMeta(name);
  }

  freeTMeta(): NameRep[] {
    return this.left.freeTMeta().concat(this.right.freeTMeta());
  }

  equals(that: ValType): boolean {
    return that instanceof TFun && this.left.equals(that.left) && this.right.equals(that.right);
  }

}
export const tfun = (left: ValType, right: TComp) => new TFun(left, right);
export const tfunsFrom = (ts: ValType[]) => ts.reduceRight((x, y) => tfun(y, tcomp(x)));
export const tfuns = (...ts: ValType[]) => tappsFrom(ts);
export const isTFun = (type: Type): type is TFun => type instanceof TFun;

export class TForall extends ValType {

  constructor(
    public readonly name: NameRep,
    public readonly kind: Kind,
    public readonly type: TComp,
  ) { super() }

  toString() {
    return `(forall(${this.name} : ${this.kind}). ${this.type})`;
  }

  isMono() {
    return false;
  }

  substTVar(name: NameRep, type: ValType): ValType {
    return this.name.equals(name) ? this : new TForall(this.name, this.kind, this.type.substTVar(name, type));
  }
  open(type: ValType): Type {
    return this.type.substTVar(this.name, type);
  }

  substTMeta(name: NameRep, type: ValType): ValType {
    return new TForall(this.name, this.kind, this.type.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.type.containsTMeta(name);
  }

  freeTMeta(): NameRep[] {
    return this.type.freeTMeta();
  }

  equals(that: ValType): boolean {
    return that instanceof TForall && this.name.equals(that.name) && this.kind.equals(that.kind) && this.type.equals(that.type);
  }

}
export const tforall = (name: NameRep, kind: Kind, type: TComp) =>
  new TForall(name, kind, type);
export const tforalls = (ns: [NameRep, Kind][], type: TComp) =>
  ns.reduceRight((t, [n, k]) => tcomp(tforall(n, k, t)), type);
export const isTForall = (type: Type): type is TForall => type instanceof TForall;

export class TComp extends Type {

  constructor(
    public readonly type: ValType,
  ) { super() }

  toString() {
    return `${this.type}`;
  }

  isMono() {
    return this.type.isMono();
  }

  substTVar(name: NameRep, type: ValType): TComp {
    return new TComp(this.type.substTVar(name, type));
  }

  substTMeta(name: NameRep, type: ValType): TComp {
    return new TComp(this.type.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.type.containsTMeta(name);
  }

  freeTMeta(): NameRep[] {
    return this.type.freeTMeta();
  }

  equals(that: TComp): boolean {
    return this.type.equals(that.type);
  }

}
export const tcomp = (type: ValType) => new TComp(type);
export const isTComp = (type: Type): type is TComp => type instanceof TComp;

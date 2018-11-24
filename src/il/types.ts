import Kind from './kinds';
import NameRep, { name } from '../NameRep';

export default abstract class Type {

  abstract toString(): string;

  abstract isMono(): boolean;

  abstract substTVar(name: NameRep, type: Type): Type;
  abstract substTMeta(name: NameRep, type: Type): Type;

  abstract containsTMeta(name: NameRep): boolean;
  abstract containsTVar(name: NameRep): boolean;

  abstract freeTMeta(): NameRep[];

  abstract equals(that: Type): boolean;

}

export class TVar extends Type {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  isMono() {
    return true;
  }

  substTVar(name: NameRep, type: Type): Type {
    return this.name.equals(name) ? type: this;
  }
  substTMeta(name: NameRep, type: Type): Type {
    return this;
  }

  containsTMeta(name: NameRep): boolean {
    return false;
  }
  containsTVar(name: NameRep): boolean {
    return this.name.equals(name);
  }

  freeTMeta(): NameRep[] {
    return [];
  }

  equals(that: Type): boolean {
    return that instanceof TVar && this.name.equals(that.name);
  }

}
export const tvar = (name: NameRep) => new TVar(name);
export const isTVar = (type: Type): type is TVar => type instanceof TVar;

export class TMeta extends Type {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return `^${this.name}`;
  }

  isMono() {
    return true;
  }

  substTVar(name: NameRep, type: Type): Type {
    return this;
  }
  substTMeta(name: NameRep, type: Type): Type {
    return this.name.equals(name) ? type: this;
  }

  containsTMeta(name: NameRep): boolean {
    return this.name.equals(name);
  }
  containsTVar(name: NameRep): boolean {
    return false;
  }

  freeTMeta(): NameRep[] {
    return [this.name];
  }

  equals(that: Type): boolean {
    return that instanceof TMeta && this.name.equals(that.name);
  }

}
export const tmeta = (name: NameRep) => new TMeta(name);
export const isTMeta = (type: Type): type is TMeta => type instanceof TMeta;

export class TApp extends Type {

  constructor(
    public readonly left: Type,
    public readonly right: Type,
  ) { super() }

  toString(): string{
    const left = this.left;
    if (isTApp(left) && isTVar(left.left) && /[^a-z]/i.test(left.left.toString()[0]))
      return `(${left.right} ${left.left} ${this.right})`;
    return `(${left} ${this.right})`;
  }

  isMono() {
    return this.left.isMono() && this.right.isMono();
  }

  substTVar(name: NameRep, type: Type): Type {
    return new TApp(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }
  substTMeta(name: NameRep, type: Type): Type {
    return new TApp(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.left.containsTMeta(name) || this.right.containsTMeta(name);
  }
  containsTVar(name: NameRep): boolean {
    return this.left.containsTVar(name) || this.right.containsTVar(name);
  }

  freeTMeta(): NameRep[] {
    return this.left.freeTMeta().concat(this.right.freeTMeta());
  }

  equals(that: Type): boolean {
    return that instanceof TApp && this.left.equals(that.left) && this.right.equals(that.right);
  }

}
export const tapp = (left: Type, right: Type) => new TApp(left, right);
export const tappsFrom = (ts: Type[]) => ts.reduce(tapp);
export const tapps = (...ts: Type[]) => tappsFrom(ts);
export const isTApp = (type: Type): type is TApp => type instanceof TApp;
export const flattenTApp = (type: Type): { head: Type, tail: Type[] } => {
  if (isTApp(type)) {
    const rec = flattenTApp(type.left);
    return { head: rec.head, tail: rec.tail.concat([type.right]) };
  }
  return { head: type, tail: [] };
};
export const headTApp = (type: Type): Type => flattenTApp(type).head;

export class TFun extends Type {

  constructor(
    public readonly left: Type,
    public readonly eff: Type,
    public readonly right: Type,
  ) { super() }

  toString(): string{
    return isTEffsEmpty(this.eff) ? `(${this.left} -> ${this.right})` : `(${this.left} -> ${this.right}!${this.eff})`;
  }

  isMono() {
    return this.left.isMono() && this.eff.isMono() && this.right.isMono();
  }

  substTVar(name: NameRep, type: Type): Type {
    return new TFun(this.left.substTVar(name, type), this.eff.substTVar(name, type), this.right.substTVar(name, type));
  }
  substTMeta(name: NameRep, type: Type): Type {
    return new TFun(this.left.substTMeta(name, type), this.eff.substTMeta(name, type), this.right.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.left.containsTMeta(name) || this.eff.containsTMeta(name) || this.right.containsTMeta(name);
  }
  containsTVar(name: NameRep): boolean {
    return this.left.containsTVar(name) || this.eff.containsTVar(name) || this.right.containsTVar(name);
  }

  freeTMeta(): NameRep[] {
    return this.left.freeTMeta().concat(this.eff.freeTMeta()).concat(this.right.freeTMeta());
  }

  equals(that: Type): boolean {
    return that instanceof TFun && this.left.equals(that.left) && this.eff.equals(that.eff) && this.right.equals(that.right);
  }

}
export const tfun = (left: Type, eff: Type, right: Type) => new TFun(left, eff, right);
export const tfunFrom = (ts: Type[]) => ts.reduceRight((x, y) => tfun(y, teffsempty(), x));
export function tfuns(...ts: Type[]) { return tfunFrom(ts) }
export const isTFun = (type: Type): type is TFun => type instanceof TFun;

export class TForall extends Type {

  constructor(
    public readonly name: NameRep,
    public readonly kind: Kind,
    public readonly type: Type,
  ) { super() }

  toString() {
    return `(forall(${this.name} : ${this.kind}). ${this.type})`;
  }

  isMono() {
    return false;
  }

  substTVar(name: NameRep, type: Type): Type {
    return this.name.equals(name) ? this : new TForall(this.name, this.kind, this.type.substTVar(name, type));
  }
  open(type: Type): Type {
    return this.type.substTVar(this.name, type);
  }

  substTMeta(name: NameRep, type: Type): Type {
    return new TForall(this.name, this.kind, this.type.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.type.containsTMeta(name);
  }
  containsTVar(name: NameRep): boolean {
    return this.name.equals(name) ? false : this.type.containsTVar(name);
  }

  freeTMeta(): NameRep[] {
    return this.type.freeTMeta();
  }

  equals(that: Type): boolean {
    return that instanceof TForall && this.name.equals(that.name) && this.kind.equals(that.kind) && this.type.equals(that.type);
  }

}
export const tforall = (name: NameRep, kind: Kind, type: Type) =>
  new TForall(name, kind, type);
export const tforalls = (ns: [NameRep, Kind][], type: Type) =>
  ns.reduceRight((t, [n, k]) => tforall(n, k, t), type);
export const isTForall = (type: Type): type is TForall => type instanceof TForall;
export const flattenTForall = (type: Type): { ns: [NameRep, Kind][], type: Type } => {
  if (isTForall(type)) {
    const rec = flattenTForall(type.type);
    return { ns: [[type.name, type.kind] as [NameRep, Kind]].concat(rec.ns), type: rec.type };
  }
  return { ns: [], type };
};

export class TEffsEmpty extends Type {

  constructor() { super() }

  toString() {
    return `{}`;
  }

  isMono() {
    return true;
  }

  substTVar(name: NameRep, type: Type): Type {
    return this;
  }

  substTMeta(name: NameRep, type: Type): Type {
    return this;
  }

  containsTMeta(name: NameRep): boolean {
    return false;
  }
  containsTVar(name: NameRep): boolean {
    return false;
  }

  freeTMeta(): NameRep[] {
    return [];
  }

  equals(that: Type): boolean {
    return that instanceof TEffsEmpty;
  }

}
export const teffsempty = () => new TEffsEmpty();
export const isTEffsEmpty = (type: Type): type is TEffsEmpty => type instanceof TEffsEmpty;

export class TEffsExtend extends Type {

  constructor(
    public readonly type: Type,
    public readonly rest: Type,
  ) { super() }

  toString() {
    return `{ ${this.type} | ${this.rest} }`;
  }

  isMono() {
    return this.type.isMono() && this.rest.isMono();
  }

  substTVar(name: NameRep, type: Type): Type {
    return new TEffsExtend(this.type.substTVar(name, type), this.rest.substTVar(name, type));
  }

  substTMeta(name: NameRep, type: Type): Type {
    return new TEffsExtend(this.type.substTMeta(name, type), this.rest.substTMeta(name, type));
  }

  containsTMeta(name: NameRep): boolean {
    return this.type.containsTMeta(name) || this.rest.containsTMeta(name);
  }
  containsTVar(name: NameRep): boolean {
    return this.type.containsTVar(name) || this.rest.containsTVar(name);
  }

  freeTMeta(): NameRep[] {
    return this.type.freeTMeta().concat(this.rest.freeTMeta());
  }

  equals(that: Type): boolean {
    return that instanceof TEffsExtend && this.type.equals(that.type) && this.rest.equals(that.rest);
  }

}
export const teffsextend = (type: Type, rest: Type) => new TEffsExtend(type, rest);
export const teffsFrom = (ts: Type[], rest?: Type) => ts.reduceRight((a, b) => teffsextend(b, a), rest || teffsempty());
export const teffs = (...ts: Type[]) => teffsFrom(ts);
export const isTEffsExtend = (type: Type): type is TEffsExtend => type instanceof TEffsExtend;
export const flattenEffs = (row: Type): { types: Type[], rest: Type } => {
  if (isTEffsExtend(row)) {
    const rec = flattenEffs(row.rest);
    return { types: [row.type].concat(rec.types), rest: rec.rest };
  }
  return { types: [], rest: row };
};

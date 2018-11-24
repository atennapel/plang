import Kind from './kinds';

export default abstract class Type {

  abstract toString(): string;

}

export class TVar extends Type {

  constructor(
    public readonly name: string,
  ) { super() }

  toString() {
    return this.name.toString();
  }

}
export const tvar = (name: string) => new TVar(name);
export const isTVar = (type: Type): type is TVar => type instanceof TVar;

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

}
export const tfun = (left: Type, eff: Type, right: Type) => new TFun(left, eff, right);
export const tfunFrom = (ts: Type[]) => ts.reduceRight((x, y) => tfun(y, teffsempty(), x));
export function tfuns(...ts: Type[]) { return tfunFrom(ts) }
export const isTFun = (type: Type): type is TFun => type instanceof TFun;

export class TForall extends Type {

  constructor(
    public readonly name: string,
    public readonly kind: Kind,
    public readonly type: Type,
  ) { super() }

  toString() {
    return `(forall(${this.name} : ${this.kind}). ${this.type})`;
  }

}
export const tforall = (name: string, kind: Kind, type: Type) =>
  new TForall(name, kind, type);
export const tforalls = (ns: [string, Kind][], type: Type) =>
  ns.reduceRight((t, [n, k]) => tforall(n, k, t), type);
export const isTForall = (type: Type): type is TForall => type instanceof TForall;
export const flattenTForall = (type: Type): { ns: [string, Kind][], type: Type } => {
  if (isTForall(type)) {
    const rec = flattenTForall(type.type);
    return { ns: [[type.name, type.kind] as [string, Kind]].concat(rec.ns), type: rec.type };
  }
  return { ns: [], type };
};

export class TEffsEmpty extends Type {

  constructor() { super() }

  toString() {
    return `{}`;
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

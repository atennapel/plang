import Kind from './kinds';
import { INameRep, name } from './generic/NameRep';
import { containsDuplicate } from './utils';

export default abstract class Type<N extends INameRep<N>> {

  abstract toString(): string;

  abstract isMono(): boolean;

  abstract substTVar(name: N, type: Type<N>): Type<N>;
  abstract substTMeta(name: N, type: Type<N>): Type<N>;

  abstract containsTMeta(name: N): boolean;

  abstract freeTMeta(): N[];

}

export class TVar<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly name: N,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  isMono() {
    return true;
  }

  substTVar(name: N, type: Type<N>): Type<N> {
    return this.name.equals(name) ? type: this;
  }
  substTMeta(name: N, type: Type<N>): Type<N> {
    return this;
  }

  containsTMeta(name: N): boolean {
    return false;
  }

  freeTMeta(): N[] {
    return [];
  }

}
export const tvar = <N extends INameRep<N>>(name: N) => new TVar(name);
export const isTVar = <N extends INameRep<N>>(type: Type<N>): type is TVar<N> => type instanceof TVar;

export class TMeta<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly name: N,
  ) { super() }

  toString() {
    return `^${this.name}`;
  }

  isMono() {
    return true;
  }

  substTVar(name: N, type: Type<N>): Type<N> {
    return this;
  }
  substTMeta(name: N, type: Type<N>): Type<N> {
    return this.name.equals(name) ? type: this;
  }

  containsTMeta(name: N): boolean {
    return this.name.equals(name);
  }

  freeTMeta(): N[] {
    return [this.name];
  }

}
export const tmeta = <N extends INameRep<N>>(name: N) => new TMeta(name);
export const isTMeta = <N extends INameRep<N>>(type: Type<N>): type is TMeta<N> => type instanceof TMeta;

export class TApp<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly left: Type<N>,
    public readonly right: Type<N>,
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

  substTVar(name: N, type: Type<N>): Type<N> {
    return new TApp(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }
  substTMeta(name: N, type: Type<N>): Type<N> {
    return new TApp(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
  }

  containsTMeta(name: N): boolean {
    return this.left.containsTMeta(name) || this.right.containsTMeta(name);
  }

  freeTMeta(): N[] {
    return this.left.freeTMeta().concat(this.right.freeTMeta());
  }

}
export const tapp = <N extends INameRep<N>>(left: Type<N>, right: Type<N>) => new TApp(left, right);
export const tappFrom = <N extends INameRep<N>>(ts: Type<N>[]) => ts.reduce(tapp);
export function tapps<N extends INameRep<N>>(...ts: Type<N>[]) { return tappFrom(ts) }
export const isTApp = <N extends INameRep<N>>(type: Type<N>): type is TApp<N> => type instanceof TApp;

export class TForall<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly name: N,
    public readonly kind: Kind<N>,
    public readonly type: Type<N>,
  ) { super() }

  toString() {
    return `(forall(${this.name} : ${this.kind}). ${this.type})`;
  }

  isMono() {
    return false;
  }

  substTVar(name: N, type: Type<N>): Type<N> {
    return this.name.equals(name) ? this : new TForall(this.name, this.kind, this.type.substTVar(name, type));
  }
  open(type: Type<N>): Type<N> {
    return this.type.substTVar(this.name, type);
  }

  substTMeta(name: N, type: Type<N>): Type<N> {
    return new TForall(this.name, this.kind, this.type.substTMeta(name, type));
  }

  containsTMeta(name: N): boolean {
    return this.type.containsTMeta(name);
  }

  freeTMeta(): N[] {
    return this.type.freeTMeta();
  }

}
export const tforall = <N extends INameRep<N>>(name: N, kind: Kind<N>, type: Type<N>) =>
  new TForall(name, kind, type);
export const tforalls = <N extends INameRep<N>>(ns: [N, Kind<N>][], type: Type<N>) =>
  ns.reduceRight((t, [n, k]) => tforall(n, k, t), type);
export const isTForall = <N extends INameRep<N>>(type: Type<N>): type is TForall<N> => type instanceof TForall;

export class TRowEmpty<N extends INameRep<N>> extends Type<N> {

  constructor() { super() }

  toString() {
    return `{}`;
  }

  isMono() {
    return true;
  }

  substTVar(name: N, type: Type<N>): Type<N> {
    return this;
  }

  substTMeta(name: N, type: Type<N>): Type<N> {
    return this;
  }

  containsTMeta(name: N): boolean {
    return false;
  }

  freeTMeta(): N[] {
    return [];
  }

}
export const trowempty = <N extends INameRep<N>>() => new TRowEmpty<N>();
export const isTRowEmpty = <N extends INameRep<N>>(type: Type<N>): type is TRowEmpty<N> => type instanceof TRowEmpty;

export class TRowExtend<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly label: N,
    public readonly type: Type<N>,
    public readonly rest: Type<N>,
  ) { super() }

  toString() {
    return `{ ${this.label} : ${this.type} | ${this.rest} }`;
  }

  isMono() {
    return this.type.isMono() && this.rest.isMono();
  }

  substTVar(name: N, type: Type<N>): Type<N> {
    return new TRowExtend(this.label, this.type.substTVar(name, type), this.rest.substTVar(name, type));
  }

  substTMeta(name: N, type: Type<N>): Type<N> {
    return new TRowExtend(this.label, this.type.substTMeta(name, type), this.rest.substTMeta(name, type));
  }

  containsTMeta(name: N): boolean {
    return this.type.containsTMeta(name) || this.rest.containsTMeta(name);
  }

  freeTMeta(): N[] {
    return this.type.freeTMeta().concat(this.rest.freeTMeta());
  }

}
export const trowextend = <N extends INameRep<N>>(label: N, type: Type<N>, rest: Type<N>) => new TRowExtend<N>(label, type, rest);
export const isTRowExtend = <N extends INameRep<N>>(type: Type<N>): type is TRowExtend<N> => type instanceof TRowExtend;
export const flattenRow = <N extends INameRep<N>>(row: Type<N>): { map: [N, Type<N>][], rest: Type<N> } => {
  if (isTRowExtend(row)) {
    const rec = flattenRow(row.rest);
    return { map: [[row.label, row.type] as [N, Type<N>]].concat(rec.map), rest: rec.rest };
  }
  return { map: [], rest: row };
};
export const labelsOfRow = <N extends INameRep<N>>(row: Type<N>): N[] => flattenRow(row).map.map(x => x [0]);
export const rowContainsDuplicate = <N extends INameRep<N>>(row: Type<N>): boolean => containsDuplicate(labelsOfRow(row));

import Kind from './kinds';
import { INameRep } from './generic/NameRep';

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

export class TFun<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly left: Type<N>,
    public readonly right: Type<N>,
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }

  isMono() {
    return this.left.isMono() && this.right.isMono();
  }

  substTVar(name: N, type: Type<N>): Type<N> {
    return new TFun(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }
  substTMeta(name: N, type: Type<N>): Type<N> {
    return new TFun(this.left.substTMeta(name, type), this.right.substTMeta(name, type));
  }

  containsTMeta(name: N): boolean {
    return this.left.containsTMeta(name) || this.right.containsTMeta(name);
  }

  freeTMeta(): N[] {
    return this.left.freeTMeta().concat(this.right.freeTMeta());
  }

}
export const tfun = <N extends INameRep<N>>(left: Type<N>, right: Type<N>) => new TFun(left, right);
export const tfunFrom = <N extends INameRep<N>>(ts: Type<N>[]) => ts.reduceRight((x, y) => tfun(y, x));
export function tfuns<N extends INameRep<N>>(...ts: Type<N>[]) { return tfunFrom(ts) }
export const isTFun = <N extends INameRep<N>>(type: Type<N>): type is TFun<N> => type instanceof TFun;

export class TApp<N extends INameRep<N>> extends Type<N> {

  constructor(
    public readonly left: Type<N>,
    public readonly right: Type<N>,
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
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

import Kind from './kinds';
import Type from './types';
import { INameRep } from './generic/NameRep';

export default abstract class Elem<N extends INameRep<N>> {

  abstract toString(): string;

}

export class CKVar<N extends INameRep<N>> extends Elem<N> {

  constructor(
    public readonly name: N,
  ) { super() }

  toString() {
    return `kind ${this.name}`;
  }

}
export const ckvar = <N extends INameRep<N>>(name: N) => new CKVar(name);
export const isCKVar = <N extends INameRep<N>>(name: N) =>
  (e: Elem<N>): e is CKVar<N> => e instanceof CKVar && e.name.equals(name);

export class CTVar<N extends INameRep<N>> extends Elem<N> {

  constructor(
    public readonly name: N,
    public readonly kind: Kind<N>,
  ) { super() }

  toString() {
    return `${this.name} :k ${this.kind}`;
  }

}
export const ctvar = <N extends INameRep<N>>(name: N, kind: Kind<N>) => new CTVar(name, kind);
export const isCTVar = <N extends INameRep<N>>(name: N) =>
  (e: Elem<N>): e is CTVar<N> => e instanceof CTVar && e.name.equals(name);

export class CTMeta<N extends INameRep<N>> extends Elem<N> {

  constructor(
    public readonly name: N,
    public readonly kind: Kind<N>,
    public readonly type: Type<N> | null,
  ) { super() }

  toString() {
    return this.type ? `^${this.name} :k ${this.kind} = ${this.type}` : `^${this.name} :k ${this.kind}`;
  }

  solve(type: Type<N>) {
    return new CTMeta(this.name, this.kind, type);
  }

}
export const ctmeta = <N extends INameRep<N>>(name: N, kind: Kind<N>) => new CTMeta(name, kind, null);
export const csolved = <N extends INameRep<N>>(name: N, kind: Kind<N>, type: Type<N>) => new CTMeta(name, kind, type);
export const isCTMeta = <N extends INameRep<N>>(name: N) =>
  (e: Elem<N>): e is CTMeta<N> => e instanceof CTMeta && e.name.equals(name);

export class CMarker<N extends INameRep<N>> extends Elem<N> {

  constructor(
    public readonly name: N,
  ) { super() }

  toString() {
    return `|>${this.name}`;
  }

}
export const cmarker = <N extends INameRep<N>>(name: N) => new CMarker(name);
export const isCMarker = <N extends INameRep<N>>(name: N) =>
  (e: Elem<N>): e is CMarker<N> => e instanceof CMarker && e.name.equals(name);

export class CVar<N extends INameRep<N>> extends Elem<N> {

  constructor(
    public readonly name: N,
    public readonly type: Type<N>,
  ) { super() }

  toString() {
    return `${this.name} : ${this.type}`;
  }

}
export const cvar = <N extends INameRep<N>>(name: N, type: Type<N>) => new CVar(name, type);
export const isCVar = <N extends INameRep<N>>(name: N) =>
  (e: Elem<N>): e is CVar<N> => e instanceof CVar && e.name.equals(name);

import { INameRep } from './generic/NameRep';

export default abstract class Kind<N extends INameRep<N>> {

  abstract toString(): string;
  abstract equals(that: Kind<N>): boolean;

}

export class KVar<N extends INameRep<N>> extends Kind<N> {

  constructor(
    readonly name: N,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  equals(that: Kind<N>): boolean {
    return that instanceof KVar && this.name.equals(that.name);
  }

}
export const kvar = <N extends INameRep<N>>(name: N) => new KVar(name);
export const isKVar = <N extends INameRep<N>>(kind: Kind<N>): kind is KVar<N> => kind instanceof KVar;

export class KFun<N extends INameRep<N>> extends Kind<N> {

  constructor(
    readonly left: Kind<N>,
    readonly right: Kind<N>,
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }

  equals(that: Kind<N>): boolean {
    return that instanceof KFun && this.left.equals(that.left) && this.right.equals(that.right);
  }

}
export const kfun = <N extends INameRep<N>>(left: Kind<N>, right: Kind<N>) => new KFun(left, right);
export const kfunFrom = <N extends INameRep<N>>(ks: Kind<N>[]) => ks.reduceRight((x, y) => kfun(y, x));
export function kfuns<N extends INameRep<N>>(...ks: Kind<N>[]) { return kfunFrom(ks) }
export const isKFun = <N extends INameRep<N>>(kind: Kind<N>): kind is KFun<N> => kind instanceof KFun;

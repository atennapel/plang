import NameRep, { name } from '../NameRep';

export default abstract class Kind {

  abstract toString(): string;
  abstract equals(that: Kind): boolean;

}

export class KVar extends Kind {

  constructor(
    readonly name: NameRep,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  equals(that: Kind): boolean {
    return that instanceof KVar && this.name.equals(that.name);
  }

}
export const kvar = (name: NameRep) => new KVar(name);
export const isKVar = (kind: Kind): kind is KVar => kind instanceof KVar;

export class KFun extends Kind {

  constructor(
    readonly left: Kind,
    readonly right: Kind,
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }

  equals(that: Kind): boolean {
    return that instanceof KFun && this.left.equals(that.left) && this.right.equals(that.right);
  }

}
export const kfun = (left: Kind, right: Kind) => new KFun(left, right);
export const kfunFrom = (ks: Kind[]) => ks.reduceRight((x, y) => kfun(y, x));
export function kfuns(...ks: Kind[]) { return kfunFrom(ks) }
export const isKFun = (kind: Kind): kind is KFun => kind instanceof KFun;

export const nType = name('Type');
export const kType = kvar(nType);

export const nComp = name('Comp');
export const kComp = kvar(nComp);

export default abstract class Kind {

  abstract toString(): string;

}

export class KVar extends Kind {

  constructor(
    readonly name: string,
  ) { super() }

  toString() {
    return this.name.toString();
  }

}
export const kvar = (name: string) => new KVar(name);
export const isKVar = (kind: Kind): kind is KVar => kind instanceof KVar;

export class KFun extends Kind {

  constructor(
    readonly left: Kind,
    readonly right: Kind,
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }

}
export const kfun = (left: Kind, right: Kind) => new KFun(left, right);
export const kfunFrom = (ks: Kind[]) => ks.reduceRight((x, y) => kfun(y, x));
export function kfuns(...ks: Kind[]) { return kfunFrom(ks) }
export const isKFun = (kind: Kind): kind is KFun => kind instanceof KFun;

import Kind from "./kinds";

export default abstract class Type {

  abstract toString(): string;

}

export class TVar extends Type {

  constructor(
    public readonly name: string,
  ) { super() }

  toString() {
    return this.name;
  }

}
export const tvar = (name: string) => new TVar(name);

export class TMeta extends Type {

  constructor(
    public readonly name: string,
  ) { super() }

  toString() {
    return this.name;
  }

}
export const tmeta = (name: string) => new TMeta(name);

export class TFun extends Type {

  constructor(
    public readonly left: Type,
    public readonly right: Type,
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }

}
export const tfun = (left: Type, right: Type) => new TFun(left, right);
export const tfunFrom = (ts: Type[]) => ts.reduceRight((x, y) => tfun(y, x));
export function tfuns(...ts: Type[]) { return tfunFrom(ts) }

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

export abstract class Kind {
  abstract toString(): string;
  abstract equals(other: Kind): boolean;
}

export class KCon extends Kind {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
  }
  equals(other: Kind): boolean {
    return other instanceof KCon && this.name === other.name;
  }
}
export const kcon = (name: string) => new KCon(name);

export class KFun extends Kind {
  constructor(
    public readonly left: Kind,
    public readonly right: Kind
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }
  equals(other: Kind): boolean {
    return other instanceof KFun && this.left.equals(other.left) && this.right.equals(other.right);
  }
}
export const kfun = (left: Kind, right: Kind) => new KFun(left, right);
export const kfuns = (...ts: Kind[]) => ts.reduceRight((a, b) => kfun(b, a));

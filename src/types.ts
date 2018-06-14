export abstract class Type {
  abstract toString(): string;
  abstract isMono(): boolean;
  abstract subst(name: string, type: Type): Type;
  abstract substEx(name: string, type: Type): Type;
}

export class TUnit extends Type {
  toString() {
    return '()';
  }
  isMono() {
    return true;
  }
  subst(name: string, type: Type) {
    return this;
  }
  substEx(name: string, type: Type) {
    return this;
  }
}
export const tunit = new TUnit();

export class TVar extends Type {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
  }
  isMono() {
    return true;
  }
  subst(name: string, type: Type): Type {
    return this.name === name? type: this;
  }
  substEx(name: string, type: Type): Type {
    return this;
  }
}
export const tvar = (name: string) => new TVar(name);

export class TEx extends Type {
  constructor(public readonly name: string) { super() }

  toString() {
    return `^${this.name}`;
  }
  isMono() {
    return true;
  }
  subst(name: string, type: Type) {
    return this;
  }
  substEx(name: string, type: Type): Type {
    return this.name === name? type: this;
  }
}
export const tex = (name: string) => new TEx(name);

export class TFun extends Type {
  constructor(
    public readonly left: Type,
    public readonly right: Type
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }
  isMono() {
    return this.left.isMono() && this.right.isMono();
  }
  subst(name: string, type: Type) {
    return new TFun(this.left.subst(name, type), this.right.subst(name, type));
  }
  substEx(name: string, type: Type) {
    return new TFun(this.left.substEx(name, type), this.right.substEx(name, type));
  }
}
export const tfun = (left: Type, right: Type) => new TFun(left, right);
export const tfuns = (...ts: Type[]) => ts.reduceRight((a, b) => tfun(b, a));

export class TForall extends Type {
  constructor(
    public readonly name: string,
    public readonly type: Type
  ) { super() }

  toString() {
    return `(forall ${this.name}. ${this.type})`;
  }
  isMono() {
    return false;
  }
  subst(name: string, type: Type) {
    return this.name === name? this: new TForall(this.name, this.type.subst(name, type));
  }
  substEx(name: string, type: Type) {
    return new TForall(this.name, this.type.substEx(name, type));
  }
}
export const tforall = (name: string, type: Type) => new TForall(name, type);
export const tforalls = (ns: string[], type: Type) => ns.reduceRight((a, b) => tforall(b, a), type);

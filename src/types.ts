import { Kind } from './kinds';

export abstract class Type {
  abstract toString(): string;
  abstract isMono(): boolean;
  abstract subst(name: string, type: Type): Type;
  abstract substEx(name: string, type: Type): Type;
  abstract containsEx(name: string): boolean;
  abstract texs(): string[];
}

export class TCon extends Type {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
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
  containsEx(name: string): boolean {
    return false;
  }
  texs(): string[] {
    return [];
  }
}
export const tcon = (name: string) => new TCon(name);

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
  containsEx(name: string): boolean {
    return false;
  }
  texs(): string[] {
    return [];
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
  containsEx(name: string): boolean {
    return this.name === name;
  }
  texs(): string[] {
    return [this.name];
  }
}
export const tex = (name: string) => new TEx(name);

export class TApp extends Type {
  constructor(
    public readonly left: Type,
    public readonly right: Type
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }
  isMono() {
    return this.left.isMono() && this.right.isMono();
  }
  subst(name: string, type: Type) {
    return new TApp(this.left.subst(name, type), this.right.subst(name, type));
  }
  substEx(name: string, type: Type) {
    return new TApp(this.left.substEx(name, type), this.right.substEx(name, type));
  }
  containsEx(name: string): boolean {
    return this.left.containsEx(name) || this.right.containsEx(name);
  }
  texs(): string[] {
    return this.left.texs().concat(this.right.texs());
  }
}
export const tapp = (left: Type, right: Type) => new TApp(left, right);
export const tapps = (...ts: Type[]) => ts.reduce(tapp);

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
  containsEx(name: string): boolean {
    return this.left.containsEx(name) || this.right.containsEx(name);
  }
  texs(): string[] {
    return this.left.texs().concat(this.right.texs());
  }
}
export const tfun = (left: Type, right: Type) => new TFun(left, right);
export const tfuns = (...ts: Type[]) => ts.reduceRight((a, b) => tfun(b, a));

export class TForall extends Type {
  constructor(
    public readonly name: string,
    public readonly kind: Kind,
    public readonly type: Type
  ) { super() }

  toString() {
    return `(forall(${this.name} : ${this.kind}). ${this.type})`;
  }
  isMono() {
    return false;
  }
  subst(name: string, type: Type) {
    return this.name === name? this: new TForall(this.name, this.kind, this.type.subst(name, type));
  }
  substEx(name: string, type: Type) {
    return new TForall(this.name, this.kind, this.type.substEx(name, type));
  }
  open(type: Type) {
    return this.type.subst(this.name, type);
  }
  containsEx(name: string): boolean {
    return this.type.containsEx(name);
  }
  texs(): string[] {
    return this.type.texs();
  }
}
export const tforall = (name: string, kind: Kind, type: Type) => new TForall(name, kind, type);
export const tforalls = (ns: [string, Kind][], type: Type) =>
  ns.reduceRight((a, b) => tforall(b[0], b[1], a), type);

import { Kind } from './kinds';
import { arrEquals, any, all, concatAll } from './util';

export abstract class Type {
  abstract toString(): string;
  abstract equals(other: Type): boolean;
  abstract isMono(): boolean;
  abstract subst(name: string, type: Type): Type;
  abstract substEx(name: string, type: Type): Type;
  abstract containsEx(name: string): boolean;
  abstract containsTCon(name: string): boolean;
  abstract texs(): string[];
  abstract tvars(): string[];
  abstract occursNegatively(name: string, negative: boolean): boolean;
}

export class TCon extends Type {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
  }
  equals(other: Type): boolean {
    return other instanceof TCon && this.name === other.name;
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
  containsTCon(name: string): boolean {
    return this.name === name;
  }
  texs(): string[] {
    return [];
  }
  tvars(): string[] {
    return [];
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return this.name === name && negative;
  }
}
export const tcon = (name: string) => new TCon(name);

export class TVar extends Type {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
  }
  equals(other: Type): boolean {
    return other instanceof TVar && this.name === other.name;
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
  containsTCon(name: string): boolean {
    return false;
  }
  texs(): string[] {
    return [];
  }
  tvars(): string[] {
    return [this.name];
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return false;
  }
}
export const tvar = (name: string) => new TVar(name);

export class TEx extends Type {
  constructor(public readonly name: string) { super() }

  toString() {
    return `^${this.name}`;
  }
  equals(other: Type): boolean {
    return other instanceof TEx && this.name === other.name;
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
  containsTCon(name: string): boolean {
    return false;
  }
  texs(): string[] {
    return [this.name];
  }
  tvars(): string[] {
    return [];
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return false;
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
  equals(other: Type): boolean {
    return other instanceof TApp && this.left.equals(other.left) && this.right.equals(other.right);
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
  containsTCon(name: string): boolean {
    return this.left.containsTCon(name) || this.right.containsTCon(name);
  }
  texs(): string[] {
    return this.left.texs().concat(this.right.texs());
  }
  tvars(): string[] {
    return this.left.tvars().concat(this.right.tvars());
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return this.left.occursNegatively(name, negative) || this.right.occursNegatively(name, negative);
  }
}
export const tapp = (left: Type, right: Type) => new TApp(left, right);
export const tapps = (...ts: Type[]) => ts.reduce(tapp);

export function flattenTApp(a: TApp): Type[] {
  const r = [];
  let c: Type = a;
  while(c instanceof TApp) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
}

export class TFun extends Type {
  constructor(
    public readonly left: Type,
    public readonly right: Type
  ) { super() }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }
  equals(other: Type): boolean {
    return other instanceof TFun && this.left.equals(other.left) && this.right.equals(other.right);
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
  containsTCon(name: string): boolean {
    return this.left.containsTCon(name) || this.right.containsTCon(name);
  }
  texs(): string[] {
    return this.left.texs().concat(this.right.texs());
  }
  tvars(): string[] {
    return this.left.tvars().concat(this.right.tvars());
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return this.left.occursNegatively(name, !negative) || this.right.occursNegatively(name, negative);
  }
}
export const tfun = (left: Type, right: Type) => new TFun(left, right);
export const tfuns = (...ts: Type[]) => ts.reduceRight((a, b) => tfun(b, a));

export class TForall extends Type {
  constructor(
    public readonly name: string,
    public readonly kind: Kind,
    public readonly constraints: Type[],
    public readonly type: Type
  ) { super() }

  toString() {
    return `(forall(${this.name} : ${this.kind}). ${this.constraints.join(', ')} => ${this.type})`;
  }
  equals(other: Type): boolean {
    return other instanceof TForall &&
      this.name === other.name &&
      this.kind.equals(other.kind) &&
      arrEquals(this.constraints, other.constraints) &&
      this.type.equals(other.type);
  }
  isMono() {
    return false;
  }
  subst(name: string, type: Type) {
    return this.name === name? this: new TForall(this.name, this.kind, this.constraints.map(t => t.subst(name, type)), this.type.subst(name, type));
  }
  substEx(name: string, type: Type) {
    return new TForall(this.name, this.kind, this.constraints.map(t => t.substEx(name, type)), this.type.substEx(name, type));
  }
  open(type: Type) {
    return this.type.subst(this.name, type);
  }
  containsEx(name: string): boolean {
    return any(this.constraints.map(t => t.containsEx(name))) || this.type.containsEx(name);
  }
  containsTCon(name: string): boolean {
    return any(this.constraints.map(t => t.containsTCon(name))) || this.type.containsTCon(name);
  }
  texs(): string[] {
    return concatAll(this.constraints.map(t => t.texs())).concat(this.type.texs());
  }
  tvars(): string[] {
    return [this.name].concat(concatAll(this.constraints.map(t => t.tvars()))).concat(this.type.tvars());
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return any(this.constraints.map(t => t.occursNegatively(name, negative))) || this.type.occursNegatively(name, negative);
  }
}
export const tforall = (name: string, kind: Kind, type: Type) => new TForall(name, kind, [], type);
export const tforallc = (name: string, kind: Kind, constraints: Type[], type: Type) => new TForall(name, kind, constraints, type);
export const tforalls = (ns: [string, Kind][], type: Type) =>
  ns.reduceRight((a, b) => tforall(b[0], b[1], a), type);
export const tforallsc = (ns: [string, Kind, Type[]][], type: Type) =>
  ns.reduceRight((a, b) => tforallc(b[0], b[1], b[2], a), type);

export class TEmpty extends Type {
  toString() {
    return `{}`;
  }
  equals(other: Type): boolean {
    return other instanceof TEmpty;
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
  containsTCon(name: string): boolean {
    return false;
  }
  texs(): string[] {
    return [];
  }
  tvars(): string[] {
    return [];
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return false;
  }
}
export const tempty = new TEmpty();

export class TExtend extends Type {
  constructor(
    public readonly label: string,
    public readonly type: Type,
    public readonly rest: Type,
  ) { super() }

  toString() {
    return `{ ${this.label} : ${this.type} | ${this.rest} }`;
  }
  equals(other: Type): boolean {
    return other instanceof TExtend && this.label === other.label &&
      this.type.equals(other.type) && this.rest.equals(other.rest);
  }
  isMono() {
    return this.type.isMono() && this.rest.isMono();
  }
  subst(name: string, type: Type) {
    return new TExtend(this.label, this.type.subst(name, type), this.rest.subst(name, type));
  }
  substEx(name: string, type: Type) {
    return new TExtend(this.label, this.type.substEx(name, type), this.rest.substEx(name, type));
  }
  containsEx(name: string): boolean {
    return this.type.containsEx(name) || this.rest.containsEx(name);
  }
  containsTCon(name: string): boolean {
    return this.type.containsTCon(name) || this.rest.containsTCon(name);
  }
  texs(): string[] {
    return this.type.texs().concat(this.rest.texs());
  }
  tvars(): string[] {
    return this.type.tvars().concat(this.rest.tvars());
  }
  occursNegatively(name: string, negative: boolean): boolean {
    return this.type.occursNegatively(name, !negative) || this.rest.occursNegatively(name, negative);
  }
}
export const textend = (label: string, type: Type, rest: Type) => new TExtend(label, type, rest);
export const trow = (props: [string, Type][], rest?: Type) =>
  props.reduceRight((r, [l, t]) => textend(l, t, r), rest || tempty);

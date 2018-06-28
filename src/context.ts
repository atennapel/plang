import { Type, TEx, TFun, tfun, TForall, tforall, TApp, tapp, TImpl, timpl } from './types';
import { Kind } from './kinds';

export abstract class ContextElem {
  abstract toString(): string;
}

export class CKCon extends ContextElem {
  constructor(public readonly name: string) { super() }
  
  toString() {
    return `kind ${this.name}`;
  }
}
export const ckcon = (name: string) => new CKCon(name);
export const isCKCon =
  (name: string) => (e: ContextElem): e is CKCon => e instanceof CKCon && e.name === name;

export class CTCon extends ContextElem {
  constructor(public readonly name: string, public readonly kind: Kind) { super() }
  
  toString() {
    return `type ${this.name} : ${this.kind}`;
  }
}
export const ctcon = (name: string, kind: Kind) => new CTCon(name, kind);
export const isCTCon =
  (name: string) => (e: ContextElem): e is CTCon => e instanceof CTCon && e.name === name;

export class CTVar extends ContextElem {
  constructor(public readonly name: string, public readonly kind: Kind) { super() }
  
  toString() {
    return `tvar ${this.name} : ${this.kind}`;
  }
}
export const ctvar = (name: string, kind: Kind) => new CTVar(name, kind);
export const isCTVar =
  (name: string) => (e: ContextElem): e is CTVar => e instanceof CTVar && e.name === name;

export class CTEx extends ContextElem {
  constructor(public readonly name: string, public readonly kind: Kind, public readonly implicit: boolean = false) { super() }
  
  toString() {
    return `^${this.name}${this.implicit? '?': ''} : ${this.kind}`;
  }
}
export const ctex = (name: string, kind: Kind, implicit?: boolean) => new CTEx(name, kind, implicit);
export const isCTEx =
  (name: string) => (e: ContextElem): e is CTEx => e instanceof CTEx && e.name === name;

export class CVar extends ContextElem {
  constructor(public readonly name: string, public readonly type: Type) { super() }
  
  toString() {
    return `${this.name} : ${this.type}`;
  }
}
export const cvar = (name: string, type: Type) => new CVar(name, type);
export const isCVar =
  (name: string) => (e: ContextElem): e is CVar => e instanceof CVar && e.name === name;

export class CSolved extends ContextElem {
  constructor(
    public readonly name: string,
    public readonly kind: Kind,
    public readonly type: Type,
    public readonly implicit: boolean = false
  ) { super() }
  
  toString() {
    return `^${this.name}${this.implicit? '?': ''} : ${this.kind} = ${this.type}`;
  }
}
export const csolved = (name: string, kind: Kind, type: Type, implicit?: boolean) =>
  new CSolved(name, kind, type, implicit);

export class CMarker extends ContextElem {
  constructor(public readonly name: string) { super() }
  
  toString() {
    return `|>${this.name}`;
  }
}
export const cmarker = (name: string) => new CMarker(name);
export const isCMarker =
  (name: string) => (e: ContextElem): e is CMarker => e instanceof CMarker && e.name === name;

export class Context {
  constructor(public readonly elems: ContextElem[]) {}

  toString() {
    return `[${this.elems.join(', ')}]`;
  }

  findIndex(fn: (e: ContextElem) => boolean): number {
    const a = this.elems;
    const l = a.length;
    for(let i = 0; i < l; i++) {
      if(fn(a[i])) return i;
    }
    return -1;
  }
  find<T>(fn: (e: ContextElem) => T | null): T | null {
    const a = this.elems;
    const l = a.length;
    for(let i = 0; i < l; i++) {
      const r = fn(a[i]);
      if(r !== null) return r;
    }
    return null;
  }

  contains(fn: (e: ContextElem) => boolean): boolean {
    return this.find(e => fn(e)? true: null) !== null;
  }

  isComplete(): boolean {
    return !this.contains(e => e instanceof CTEx);
  }

  findVar(name: string): Type | null {
    return this.find(e => e instanceof CVar && e.name === name? e.type: null);
  }
  findSolved(name: string): Type | null {
    return this.find(e => e instanceof CSolved && e.name === name? e.type: null);
  }

  findTCon(name: string): Kind | null {
    return this.find(e => e instanceof CTCon && e.name === name? e.kind: null);
  }
  findTVar(name: string): Kind | null {
    return this.find(e => e instanceof CTVar && e.name === name? e.kind: null);
  }
  findEx(name: string): Kind | null {
    return this.find(e => e instanceof CTEx && e.name === name? e.kind: null);
  }
  findKCon(name: string): true | null {
    return this.find(e => e instanceof CKCon && e.name === name? true: null);
  }
  findMarker(name: string): true | null {
    return this.find(e => e instanceof CMarker && e.name === name? true: null);
  }

  findExOrSolved(name: string): Kind | null {
    return this.findEx(name) || this.find(e => e instanceof CSolved && e.name === name? e.kind: null);
  }

  add(...es: ContextElem[]): Context {
    return new Context(this.elems.concat(es));
  }
  append(other: Context): Context {
    return new Context(this.elems.concat(other.elems));
  }
  split(fn: (e: ContextElem) => boolean): { left: Context, right: Context } {
    const i = this.findIndex(fn);
    return i < 0?
      { left: this, right: new Context([]) }:
      { left: new Context(this.elems.slice(0, i)), right: new Context(this.elems.slice(i + 1)) };
  }
  replace(fn: (e: ContextElem) => boolean, other: Context): Context {
    const i = this.findIndex(fn);
    return i < 0? this: new Context(this.elems.slice(0, i).concat(other.elems, this.elems.slice(i + 1)));
  }

  removeAll(fn: (e: ContextElem) => boolean): Context {
    const r = [];
    const a = this.elems;
    const l = a.length;
    for(let i = 0; i < l; i++) {
      if(!fn(a[i])) r.push(a[i]);
    }
    return new Context(r);
  }

  isOrdered(a: string, b: string): boolean {
    const ia = this.findIndex(e => e instanceof CTEx && e.name === a);
    const ib = this.findIndex(e => e instanceof CTEx && e.name === b);
    return ia < 0 || ib < 0? false: ia < ib;
  }

  kcons(): string[] {
    return this.elems.filter(e => e instanceof CKCon).map((e: CKCon) => e.name);
  }
  tcons(): string[] {
    return this.elems.filter(e => e instanceof CTCon).map((e: CTCon) => e.name);
  }
  vars(): string[] {
    return this.elems.filter(e => e instanceof CVar).map((e: CVar) => e.name);
  }
  tvars(): string[] {
    return this.elems.filter(e => e instanceof CTVar).map((e: CTVar) => e.name);
  }
  texs(): string[] {
    return this.elems.filter(e => e instanceof CTEx || e instanceof CSolved)
      .map((e: CTEx | CSolved) => e.name);
  }
  unsolved(): [string, Kind][] {
    return this.elems.filter(e => e instanceof CTEx).map((e: CTEx) => [e.name, e.kind] as [string, Kind]);
  }
  unsolvedNonImplicits(): [string, Kind][] {
    return this.elems.filter(e => e instanceof CTEx && !e.implicit).map((e: CTEx) => [e.name, e.kind] as [string, Kind]);
  }
  implicits(): CSolved[] {
    return this.elems.filter(e => e instanceof CSolved && e.implicit) as CSolved[];
  }

  apply(type: Type): Type {
    if(type instanceof TEx) {
      const r = this.find(e =>
        (e instanceof CTEx || e instanceof CSolved) && e.name === type.name? e: null);
      return r === null? type: r instanceof CSolved? this.apply(r.type): type; 
    }
    if(type instanceof TFun) return tfun(this.apply(type.left), this.apply(type.right));
    if(type instanceof TImpl) return timpl(this.apply(type.left), this.apply(type.right));
    if(type instanceof TApp) return tapp(this.apply(type.left), this.apply(type.right));
    if(type instanceof TForall) return tforall(type.name, type.kind, this.apply(type.type));
    return type;
  }

  applyContextElem(e: ContextElem): ContextElem {
    if(e instanceof CVar) return cvar(e.name, this.apply(e.type));
    if(e instanceof CSolved) return csolved(e.name, e.kind, this.apply(e.type), e.implicit);
    return e;
  }

  applyContext(context: Context): Context {
    return new Context(context.elems.map(e => this.applyContextElem(e)));
  }
}

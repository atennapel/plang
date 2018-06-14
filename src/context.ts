import { Type, TEx, TFun, tfun, TForall, tforall } from './types';

export abstract class ContextElem {
  abstract toString(): string;
}

export class CTVar extends ContextElem {
  constructor(public readonly name: string) { super() }
  
  toString() {
    return `${this.name}`;
  }
}
export const ctvar = (name: string) => new CTVar(name);

export class CTEx extends ContextElem {
  constructor(public readonly name: string) { super() }
  
  toString() {
    return `^${this.name}`;
  }
}
export const ctex = (name: string) => new CTEx(name);

export class CVar extends ContextElem {
  constructor(public readonly name: string, public readonly type: Type) { super() }
  
  toString() {
    return `${this.name} : ${this.type}`;
  }
}
export const cvar = (name: string, type: Type) => new CVar(name, type);

export class CSolved extends ContextElem {
  constructor(public readonly name: string, public readonly type: Type) { super() }
  
  toString() {
    return `^${this.name} = ${this.type}`;
  }
}
export const csolved = (name: string, type: Type) => new CSolved(name, type);

export class CMarker extends ContextElem {
  constructor(public readonly name: string) { super() }
  
  toString() {
    return `|>${this.name}`;
  }
}
export const cmarker = (name: string) => new CMarker(name);

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
    return this.contains(e => e instanceof CTEx);
  }

  findVar(name: string): Type | null {
    return this.find(e => e instanceof CVar && e.name === name? e.type: null);
  }
  findSolved(name: string): Type | null {
    return this.find(e => e instanceof CSolved && e.name === name? e.type: null);
  }

  findTVar(name: string): true | null {
    return this.find(e => e instanceof CTVar && e.name === name? true: null);
  }
  findEx(name: string): true | null {
    return this.find(e => e instanceof CTEx && e.name === name? true: null);
  }
  findMarker(name: string): true | null {
    return this.find(e => e instanceof CMarker && e.name === name? true: null);
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

  isOrdered(a: string, b: string): boolean {
    const ia = this.findIndex(e => e instanceof CTEx && e.name === a);
    const ib = this.findIndex(e => e instanceof CTEx && e.name === b);
    return ia < 0 || ib < 0? false: ia < ib;
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
  unsolved(): string[] {
    return this.elems.filter(e => e instanceof CTEx).map((e: CTEx) => e.name);
  }

  apply(type: Type): Type {
    if(type instanceof TEx) {
      const r = this.find(e =>
        (e instanceof CTEx || e instanceof CSolved) && e.name === type.name? e: null);
      return r === null? type: r instanceof CSolved? this.apply(r.type): type; 
    }
    if(type instanceof TFun) return tfun(this.apply(type.left), this.apply(type.right));
    if(type instanceof TForall) return tforall(type.name, this.apply(type.type));
    return type;
  }
}

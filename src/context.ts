import { Type } from './types';
import { Result } from './Result';

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

  findVar(name: string): Result<Error, Type> {
    const r = this.find(e => e instanceof CVar && e.name === name? e.type: null);
    return r === null? Result.err(new TypeError(`var ${name} not found in ${this}`)): Result.ok(r);
  }
  findSolved(name: string): Result<Error, Type> {
    const r = this.find(e => e instanceof CSolved && e.name === name? e.type: null);
    return r === null? Result.err(new TypeError(`solved ex ${name} not found in ${this}`)): Result.ok(r);
  }

  findTVar(name: string): Result<Error, null> {
    const r = this.find(e => e instanceof CTVar && e.name === name? true: null);
    return r === null? Result.err(new TypeError(`tvar ${name} not found in ${this}`)): Result.ok(null);
  }
  findEx(name: string): Result<Error, null> {
    const r = this.find(e => e instanceof CTEx && e.name === name? true: null);
    return r === null? Result.err(new TypeError(`ex ^${name} not found in ${this}`)): Result.ok(null);
  }
  findMarker(name: string): Result<Error, null> {
    const r = this.find(e => e instanceof CMarker && e.name === name? true: null);
    return r === null? Result.err(new TypeError(`marker |>${name} not found in ${this}`)): Result.ok(null);
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
    
  }
}

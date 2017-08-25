import {
  Type,
} from './types';
import Id from './Id';
import { Result } from './Result';

export abstract class ContextElem {
  abstract toString(): string;
  abstract equals(other: ContextElem): boolean;
  abstract isWellformed(c: Context): boolean;
}

export class CForall extends ContextElem {
  readonly id: Id;

  constructor(id: Id) {
    super();
    this.id = id;
  }

  toString() {
    return this.id.toString();
  }

  equals(other: ContextElem): boolean {
    return other instanceof CForall && this.id.equals(other.id);
  }

  isWellformed(c: Context): boolean {
    return !c.contains(cforall(this.id));
  }
}
export function cforall(id: Id) {
  return new CForall(id);
}

export class CExists extends ContextElem {
  readonly id: Id;

  constructor(id: Id) {
    super();
    this.id = id;
  }

  toString() {
    return `^${this.id}`;
  }

  equals(other: ContextElem): boolean {
    return other instanceof CExists && this.id.equals(other.id);
  }

  isWellformed(c: Context): boolean {
    return !c.contains((e: ContextElem) =>
      (e instanceof CExists || e instanceof CSolved) && e.id.equals(this.id));
  }
}
export function cexists(id: Id) {
  return new CExists(id);
}

export class CMarker extends ContextElem {
  readonly id: Id;

  constructor(id: Id) {
    super();
    this.id = id;
  }

  toString() {
    return `|>${this.id}`;
  }

  equals(other: ContextElem): boolean {
    return other instanceof CMarker && this.id.equals(other.id);
  }

  isWellformed(c: Context): boolean {
    return !c.contains((e: ContextElem) =>
      (e instanceof CExists || e instanceof CSolved || e instanceof CMarker) && e.id.equals(this.id));
  }
}
export function cmarker(id: Id) {
  return new CMarker(id);
}

export class CVar extends ContextElem {
  readonly id: string;
  readonly type: Type;

  constructor(id: string, type: Type) {
    super();
    this.id = id;
    this.type = type;
  }

  toString() {
    return `${this.id}=${this.type}`;
  }

  equals(other: ContextElem): boolean {
    return other instanceof CVar && this.id === other.id && this.type.equals(other.type);
  }

  isWellformed(c: Context): boolean {
    return this.type.isWellformed(c) && !c.contains((e: ContextElem) =>
      (e instanceof CVar) && e.id === this.id);
  }
}
export function cvar(id: string, type: Type) {
  return new CVar(id, type);
}

export class CSolved extends ContextElem {
  readonly id: Id;
  readonly type: Type;

  constructor(id: Id, type: Type) {
    super();
    this.id = id;
    this.type = type;
  }

  toString() {
    return `^${this.id}=${this.type}`;
  }

  equals(other: ContextElem): boolean {
    return other instanceof CSolved && this.id.equals(other.id) && this.type.equals(other.type);
  }

  isWellformed(c: Context): boolean {
    return this.type.isWellformed(c) && !c.contains((e: ContextElem) =>
      (e instanceof CExists || e instanceof CSolved) && e.id.equals(this.id));
  }
}
export function csolved(id: Id, type: Type) {
  return new CSolved(id, type);
}

export default class Context {
  private readonly context: ContextElem[];

  constructor(context: ContextElem[]) {
    this.context = context;
  }

  static empty() {
    return new Context([]);
  }

  toString() {
    return `Context[${this.context.join(', ')}]`;
  }

  isComplete() {
    for(let i = 0, l = this.context.length; i < l; i++) {
      if(this.context[i] instanceof CExists) return false;
    }
    return true;
  }

  indexOf(c: ContextElem | ((e: ContextElem) => boolean)) {
    for(let i = 0, l = this.context.length; i < l; i++) {
      if(typeof c === 'function'? c(this.context[i]): this.context[i].equals(c))
        return i;
    }
    return -1;
  }

  contains(c: ContextElem | ((e: ContextElem) => boolean)) {
    return this.indexOf(c) >= 0;
  }

  get(id: string): Result<Error, Type> {
    const i = this.indexOf(e => e instanceof CVar && e.id === id);
    if(i >= 0) return Result.ok((this.context[i] as CVar).type);
    return Result.err(`id not in ${this}`);
  }

  getOrSolved<R>(id: Id, map: (t: Type) => R, or: R) {
    const i = this.indexOf(e => e instanceof CSolved && e.id.equals(id));
    return i >= 0? map((this.context[i] as CSolved).type): or;
  }

  append(...es: ContextElem[]) {
    return new Context(this.context.concat(es));
  }

  concat(c: Context) {
    return new Context(this.context.concat(c.context));
  }

  isWellformed(): boolean {
    const c = this.context;
    const l = c.length;
    if(l === 0) return true;
    const last = c[l - 1];
    const prev = new Context(c.slice(0, l - 1));
    return prev.isWellformed() && last.isWellformed(prev);
  }

  split(c: ContextElem | ((e: ContextElem) => boolean)): Result<Error, {left: Context, right: Context}> {
    const i = this.indexOf(c);
    if(i >= 0)
      return Result.ok({
        left: new Context(this.context.slice(0, i)),
        right: new Context(this.context.slice(i + 1)),
      });
    return Result.err(`Cannot split: ${c} in ${this}`);
  }

  insertAt(c: ContextElem | ((e: ContextElem) => boolean), elems: ContextElem[]): Result<Error, Context> {
    const i = this.indexOf(c);
    if(i < 0) Result.err(`Cannot insertAt: ${c} in ${this}`);
    return Result.ok(new Context(
      this.context.slice(0, i).concat(elems, this.context.slice(i + 1))
    ));
  }

  solve(id: Id, type: Type): Result<Error, Context> {
    return this.split(cexists(id))
      .then(({left, right}) => {
        const gamma = left.append(csolved(id, type)).concat(right);
        if(type.isWellformed(left)) return Result.ok(gamma);
        return Result.err(new Error(`Cannot solve ${id} and ${type} in ${this}`));
      });
  }

  unsolved(): Id[] {
    return this.context.filter(e => e instanceof CExists).map(e => (e as CExists).id);
  }
}

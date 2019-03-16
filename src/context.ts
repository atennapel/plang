import { NameT, eqName } from './names';
import {
  Elem,
  ElemTag,
  ElemFromTag,
  CKMeta,
  isCKMeta,
  CTMeta,
  isCTMeta,
  CMarker,
  showElem,
} from './elems';

export class Context {

  constructor(
    private readonly elems: Elem[] = [],
  ) {}

  static of(...es: Elem[]) {
    return new Context(es);
  }

  toString(): string {
    return `[${this.elems.map(showElem).join(', ')}]`;
  }
  clone(): Context {
    return new Context(this.elems.slice(0));
  }

  addAll(es: Elem[]): Context {
    for (let i = 0, l = es.length; i < l; i++)
      this.elems.push(es[i]);
    return this;
  }
  add(...es: Elem[]): Context {
    return this.addAll(es);
  }
  append(c: Context): Context {
    return this.addAll(c.elems);
  }

  indexOf(ty: ElemTag, name: NameT): number {
    for (let a = this.elems, l = a.length, i = 0; i < l; i++) {
      const c = a[i];
      if (c.tag === ty && eqName(c.name, name)) return i;
    }
    return -1;
  }
  contains(ty: ElemTag, name: NameT): boolean {
    return this.indexOf(ty, name) >= 0;
  }
  lookup<T extends ElemTag>(ty: T, name: NameT): ElemFromTag<T> | null {
    const i = this.indexOf(ty, name);
    if (i < 0) return null;
    return this.elems[i] as ElemFromTag<T>;
  }

  pop(): Elem | null {
    return this.elems.pop() || null;
  }

  split(ty: ElemTag, name: NameT): Elem[] {
    const i = this.indexOf(ty, name);
    if (i < 0) return [];
    const ret = this.elems.splice(i);
    ret.shift();
    return ret;
  }

  replace(ty: ElemTag, name: NameT, es: Elem[]): Context {
    const right = this.split(ty, name);
    this.addAll(es);
    this.addAll(right);
    return this;
  }

  isComplete(): boolean {
    for (let a = this.elems, l = a.length, i = 0; i < l; i++) {
      const c = a[i];
      if (c.tag === 'CTMeta' && !c.type) return false;
      if (c.tag === 'CKMeta' && !c.kind) return false;
    }
    return true;
  }

  enter(m: NameT, ...es: Elem[]): void {
    this.add(CMarker(m));
    this.addAll(es);
  }
  leave(m: NameT): void {
    this.split('CMarker', m);
  }
  leaveWithUnsolved(m: NameT): NameT[] {
    const ret = this.split('CMarker', m);
    const ns: NameT[] = [];
    for (let i = 0, l = ret.length; i < l; i++) {
      const c = ret[i];
      if (isCTMeta(c) && !c.type) ns.push(c.name);
    }
    return ns;
  }

}


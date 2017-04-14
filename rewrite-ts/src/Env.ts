import { Apply, Subst } from './typechecker';
import { EVar } from './exprs';
import { Type, TVar, TScheme } from './types';
import Map from './Map';
import Set from './Set';
import { clone } from './utils';

type InternalEnv = {[key: string]: TScheme};

export default class Env implements Apply<Env> {
  imap: InternalEnv;
  constructor(map: InternalEnv) {
    this.imap = map;
  }
  get(name: string): TScheme { return this.imap[name] }
  contains(name: string) { return !!this.get(name) }
  extend(name: string, scheme: TScheme) {
    let nmap = clone(this.imap);
    nmap[name] = scheme;
    return new Env(nmap);
  }
  free() {
    let s = Set.empty<TVar>();
    for(let k in this.imap)
      s = s.union(this.imap[k].free());
    return s;
  }
  subst(sub: Subst) {
    let n: InternalEnv = {};
    for(let k in this.imap)
      n[k] = this.imap[k].subst(sub);
    return new Env(n);
  }
}

import { Type, TVar, TScheme } from './types';
import Map from './Map';
import Set from './Set';
import { clone } from './utils';

type InternalEnv = {[key: string]: TScheme};

export default class Env {
  imap: InternalEnv;
  constructor(map: InternalEnv) {
    this.imap = map;
  }
  static empty(): Env { return new Env({}) }
  get(name: string): TScheme { return this.imap[name] }
  contains(name: string) { return !!this.get(name) }
  extend(name: string, scheme: TScheme) {
    let nmap = clone(this.imap);
    nmap[name] = scheme;
    return new Env(nmap);
  }
  schemes(): TScheme[] { return Object.keys(this.imap).map(k => this.imap[k]) }
  map(f: (v: TScheme, k: string) => TScheme): Env {
    let n: InternalEnv = {};
    for(let k in this.imap)
      n[k] = f(this.imap[k], k);
    return new Env(n);
  }
}

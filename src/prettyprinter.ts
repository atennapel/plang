import { Kind, KCon, KFun } from './kinds';
import { Type, TCon, TVar, TEx, TApp, TFun, TForall } from './types';
import { impossible } from './util';
import { ktype } from './typechecker'; 

const RARROW = ' \u2192 ';
const FORALL = '\u2200';

// Kinds
function flattenKFun(f: KFun): Kind[] {
  const r = [];
  let c: Kind = f;
  while(c instanceof KFun) {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
}

export function ppKind(k: Kind): string {
  if(k instanceof KCon) return `${k.name}`;
  if(k instanceof KFun)
    return flattenKFun(k).map(k => k instanceof KFun? `(${ppKind(k)})`: ppKind(k)).join(`${RARROW}`);
  return impossible();
}

// Types
function flattenTFun(f: TFun): Type[] {
  const r = [];
  let c: Type = f;
  while(c instanceof TFun) {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
}

function flattenTForall(f: TForall): { args: [string, Kind][], ty: Type } {
  const r: [string, Kind][] = [];
  let c: Type = f;
  while(c instanceof TForall) {
    r.push([c.name, c.kind]);
    c = c.type;
  }
  return { args: r, ty: c };
}

export function ppType(t: Type): string {
  if(t instanceof TCon) return `${t.name}`;
  if(t instanceof TVar) return `${t.name}`;
  if(t instanceof TEx) return `^${t.name}`;
  if(t instanceof TApp) return `(${ppType(t.left)} ${ppType(t.right)})`;
  if(t instanceof TFun)
    return flattenTFun(t).map(t => t instanceof TFun || t instanceof TForall? `(${ppType(t)})`: ppType(t)).join(`${RARROW}`);
  if(t instanceof TForall) {
    const f = flattenTForall(t);
    const args = f.args.map(([x, k]) => k.equals(ktype)? x: `(${x} : ${ppKind(k)})`);
    return `${FORALL}${args.join(' ')}. ${ppType(f.ty)}`;
  }
  return impossible();
}

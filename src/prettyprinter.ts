import { Kind, KCon, KFun } from './kinds';
import { Type, TCon, TVar, TEx, TApp, TFun, TForall, TImpl } from './types';
import { Context, ContextElem, CKCon, CTCon, CTVar, CTEx, CVar, CSolved, CMarker } from './context';
import { impossible } from './util';
import { ktype } from './typechecker'; 

const RARROW = ' -> ';
const RDARROW = ' => ';
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

function flattenTImpl(f: TImpl): Type[] {
  const r = [];
  let c: Type = f;
  while(c instanceof TImpl) {
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

function flattenTApp(a: TApp): Type[] {
  const r = [];
  let c: Type = a;
  while(c instanceof TApp) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
}

function isSymbol(n: string): boolean {
  return !/[a-z]/i.test(n[0]);
}

export function ppType(t: Type): string {
  if(t instanceof TCon) return `${t.name}`;
  if(t instanceof TVar) return `${t.name}`;
  if(t instanceof TEx) return `^${t.name}`;
  if(t instanceof TApp) {
    const f = flattenTApp(t);
    const first = f[0];
    if(first instanceof TCon && isSymbol(first.name) && f.length === 3) {
      const args = f.slice(1).map(t => t instanceof TApp || t instanceof TFun || t instanceof TForall? `(${ppType(t)})`: ppType(t));
      return `${args[0]} ${first.name} ${args[1]}`;
    }
    return f.map(t => t instanceof TApp || t instanceof TFun || t instanceof TForall? `(${ppType(t)})`: ppType(t)).join(` `);
  }
  if(t instanceof TFun)
    return flattenTFun(t).map(t => t instanceof TFun || t instanceof TForall? `(${ppType(t)})`: ppType(t)).join(`${RARROW}`);
  if(t instanceof TImpl)
    return `((${ppType(t.left)})${RDARROW}(${ppType(t.right)}))`;
  if(t instanceof TForall) {
    const f = flattenTForall(t);
    const args = f.args.map(([x, k]) => k.equals(ktype)? x: `(${x} : ${ppKind(k)})`);
    return `${FORALL}${args.join(' ')}. ${ppType(f.ty)}`;
  }
  return impossible();
}

// ContextElem
export function ppContextElem(e: ContextElem): string {
  if(e instanceof CKCon) return `kind ${e.name}`;
  if(e instanceof CTCon) return `type ${e.name} : ${ppKind(e.kind)}`;
  if(e instanceof CTVar) return `tvar ${e.name} : ${ppKind(e.kind)}`;
  if(e instanceof CTEx) return `tex ^${e.name}${e.implicit? '?': ''} : ${ppKind(e.kind)}`;
  if(e instanceof CVar) return `${e.name} : ${ppType(e.type)}`;
  if(e instanceof CSolved) return `^${e.name}${e.implicit? '?': ''} : ${ppKind(e.kind)} = ${ppType(e.type)}`;
  if(e instanceof CMarker) return `|>^${e.name}`;
  return impossible();
}

export function ppContext(c: Context): string {
  return `[${c.elems.map(ppContextElem).join(', ')}]`;
}

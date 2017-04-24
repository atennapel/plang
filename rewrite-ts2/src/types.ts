import { Kind, ktype, kindEq, isKArr, karr_ } from './kinds';

type TypeTag =
  'TVar' |
  'TCon' |
  'TApp' |
  'TMu';

export interface Type { tag : TypeTag, kind: Kind };

export interface TVar extends Type { name : string, id : string, hash: string };
export const isTVar = (t: Type): t is TVar => t.tag === 'TVar';
export const tvar = (name: string, id: string, kind?: Kind): TVar => ({
  tag: 'TVar',
  name,
  id,
  hash: id,
  kind: kind || ktype,
});

export interface TCon extends Type { name : string };
export const isTCon = (t: Type): t is TCon => t.tag === 'TCon';
export const tcon = (name: string, kind?: Kind): TCon => ({
  tag: 'TCon',
  name,
  kind: kind || ktype,
});

export interface TMu extends Type { arg: TVar, type: Type };
export const isTMu = (t: Type): t is TMu => t.tag === 'TMu';
export const tmu = (arg: TVar, type: Type): TMu => ({
  tag: 'TMu',
  arg,
  type,
  kind: type.kind,
});

export interface TApp extends Type { left: Type, right: Type };
export const isTApp = (t: Type): t is TApp => t.tag === 'TApp';
export const tapp = (left: Type, right: Type): TApp => {
  if(isKArr(left.kind) && kindEq(left.kind.left, right.kind)) {
    return {
      tag: 'TApp',
      left,
      right,
      kind: left.kind.right,
    };
  }
  throw new TypeError(`Invalid TApp: ${typeStr(left)} ${typeStr(right)}`);
};
export const tapp_ = (a: Type, b: Type, ...rest: Type[]): TApp => {
  const l = rest.length;
  if(l === 0) return tapp(a, b);
  if(l === 1) return tapp(tapp(a, b), rest[0]);
  let c = tapp(a, b);
  for(let i = 0; i < l; i++) c = tapp(c, rest[i]);
  return c;
};

export const tarr = tcon('->', karr_(ktype, ktype, ktype));
export const tarr2 = (a: Type, b: Type): TApp => tapp(tapp(tarr, a), b);
export const tarr_ = (a: Type, b: Type, ...rest: Type[]): TApp => {
  const l = rest.length;
  if(l === 0) return tarr2(a, b);
  if(l === 1) return tarr2(a, tarr2(b, rest[0]));
  let c = rest[l - 1];
  for(let i = l - 2; i >= 0; i--) c = tarr2(rest[i], c);
  return tarr2(a, tarr2(b, c));
};

type TSchemeTag = 'TScheme';
export interface TScheme { tag: TSchemeTag, vars: TVar[], type : Type };
export const isTScheme = (t: any): t is TScheme =>
  typeof t === 'object' && t.tag === 'TScheme';
export const tscheme = (vars: TVar[], type : Type): TScheme => ({
  tag: 'TScheme',
  vars,
  type,
});
export const tschemeStr = (t: TScheme): string =>
  `forall ${t.vars.map(typeStr).join(' ')} . ${typeStr(t.type)}`;

export const typeStr = (t: Type): string => {
  if(isTVar(t)) return t.id;
  if(isTCon(t)) return t.name;
  if(isTApp(t)) return `(${typeStr(t.left)} ${typeStr(t.right)})`;
  if(isTMu(t)) return `mu ${typeStr(t.arg)} . ${typeStr(t.type)}`;
  throw new Error('impossible');
};

import { Kind, ktype, krow, kindEq, isKArr, karr_ } from './kinds';

type TypeTag =
  'TVar' |
  'TCon' |
  'TApp' |
  'TMu' |
	'TRowEmpty' |
	'TRowExtend';

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

export interface TRowEmpty extends Type {};
export const isTRowEmpty = (t: Type): t is TRowEmpty => t.tag === 'TRowEmpty';
export const trowempty : TRowEmpty = { tag: 'TRowEmpty', kind: krow };

export interface TRowExtend extends Type { label : string, type : Type, rest : Type };
export const isTRowExtend = (t: Type): t is TRowExtend => t.tag === 'TRowExtend';
export const trowextend = (label : string, type : Type, rest : Type): TRowExtend => {
	if(rest.kind !== krow)
		throw new TypeError(`Invalid TRowExtend, rest is not of kind Row: ${typeStr(rest)}`);
	return {
		tag: 'TRowExtend',
		kind: krow,
		label,
		type,
		rest,
	};
};

export const TArr = tcon('->', karr_(ktype, ktype, ktype));
export const tarr2 = (a: Type, b: Type): TApp => tapp(tapp(TArr, a), b);
export const tarr_ = (a: Type, b: Type, ...rest: Type[]): TApp => {
  const l = rest.length;
  if(l === 0) return tarr2(a, b);
  if(l === 1) return tarr2(a, tarr2(b, rest[0]));
  let c = rest[l - 1];
  for(let i = l - 2; i >= 0; i--) c = tarr2(rest[i], c);
  return tarr2(a, tarr2(b, c));
};

export const TRecord = tcon('Rec', karr_(krow, ktype));
export const TVariant = tcon('Var', karr_(krow, ktype));

export const trecord = (row: Type): Type => tapp(TRecord, row);
export const tvariant = (row: Type): Type => tapp(TVariant, row);

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
  if(isTApp(t)) {
		if(isTApp(t.left) && t.left.left === TArr)
			return `(${typeStr(t.left.right)} -> ${typeStr(t.right)})`;
		return `(${typeStr(t.left)} ${typeStr(t.right)})`;
	}
  if(isTMu(t)) return `mu ${typeStr(t.arg)} . ${typeStr(t.type)}`;
	if(isTRowExtend(t)) return `{ ${t.label} : ${typeStr(t.type)} | ${typeStr(t.rest)} }`;
	if(isTRowEmpty(t)) return '{}';
  throw new Error('impossible');
};

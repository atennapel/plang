import { Set } from './Map';

export type Type = TUnit | TVar | TExists | TFun | TForall;

export interface TUnit { tag: 'TUnit' };
export const tunit: TUnit = { tag: 'TUnit' };

export interface TVar { tag: 'TVar', id: string };
export const tvar = (id: string): TVar => ({ tag: 'TVar', id });

export interface TExists { tag: 'TExists', id: string };
export const texists = (id: string): TExists => ({ tag: 'TExists', id });

export interface TFun { tag: 'TFun', left: Type, right: Type };
export const tfun = (left: Type, right: Type): TFun => ({ tag: 'TFun', left, right });

export interface TForall { tag: 'TForall', id: TVar, type: Type };
export const tforall = (id: TVar, type: Type): TForall => ({ tag: 'TForall', id, type });
export const tforalls = (tvars: TVar[], type: Type) => {
	if(tvars.length < 1) throw new Error('Not enough arguments for tforalls');
	return tvars.reduceRight((x, y) => tforall(y, x), type);
}
export const tfuns = (ts: Type[]) => {
	if(ts.length < 2) throw new Error('Not enough arguments for tfuns');
	return ts.reduceRight((x, y) => tfun(y, x));
};

export const typeStr = (type: Type): string => {
	switch(type.tag) {
		case 'TUnit': return '()';
		case 'TVar': return type.id;
		case 'TExists': return `^${type.id}`;
		case 'TFun': return `(${typeStr(type.left)} -> ${typeStr(type.right)})`;
		case 'TForall': return `(forall ${typeStr(type.id)} . ${typeStr(type.type)})`;
	}
}

export const typeEq = (a: Type, b: Type): boolean => {
	switch(a.tag) {
		case 'TUnit': return b.tag === 'TUnit';
		case 'TVar': return b.tag === 'TVar' && a.id === b.id;
		case 'TExists': return b.tag === 'TExists' && a.id === b.id;
		case 'TFun': return b.tag === 'TFun' && typeEq(a.left, b.left) && typeEq(a.right, b.right);
		case 'TForall': return b.tag === 'TForall' && typeEq(a.id, b.id) && typeEq(a.type, b.type);
	}
}

export const isMono = (type: Type): boolean => {
	switch(type.tag) {
		case 'TUnit': return true;
		case 'TVar': return true;
		case 'TExists': return true;
		case 'TFun': return isMono(type.left) && isMono(type.right);
		case 'TForall': return false;
	}
}

export const subst = (type: Type, id: TVar, sub: Type): Type => {
	switch(type.tag) {
		case 'TUnit': return type;
		case 'TVar': return typeEq(type, id)? sub: type;
		case 'TExists': return typeEq(type, id)? sub: type;
		case 'TFun': return tfun(subst(type.left, id, sub), subst(type.right, id, sub));
		case 'TForall': return typeEq(type.id, id)? type: tforall(type.id, subst(type.type, id, sub));
	}
}
export const substs = (s: [TVar, Type][], t: Type) => s.reduce((t, s) => subst(t, s[0], s[1]), t);

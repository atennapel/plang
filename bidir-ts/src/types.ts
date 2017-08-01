import { impossible } from './utils';
import { Map, set, union, remove } from './Map'; 

const enum TypeTag {
	TUnit,
	TVar,
	TExists,
	TFun,
	TForall,
}

export interface Type { tag: TypeTag }

export interface TUnit extends Type {}
export function isTUnit(t: Type): t is TUnit { return t.tag === TypeTag.TUnit }
export const tunit: TUnit = { tag: TypeTag.TUnit };

export interface TVar extends Type { id: string }
export function isTVar(t: Type): t is TVar { return t.tag === TypeTag.TVar }
export function tvar(id: string): TVar { return { tag: TypeTag.TVar, id } }

export interface TExists extends Type { id: string }
export function isTExists(t: Type): t is TExists { return t.tag === TypeTag.TExists }
export function texists(id: string): TExists { return { tag: TypeTag.TExists, id } }

export interface TFun extends Type { left: Type; right: Type }
export function isTFun(t: Type): t is TFun { return t.tag === TypeTag.TFun }
export function tfun(left: Type, right: Type): TFun { return { tag: TypeTag.TFun, left, right } }

export interface TForall extends Type { id: string; type: Type }
export function isTForall(type: Type): type is TForall { return type.tag === TypeTag.TForall }
export function tforall(id: string, type: Type): TForall { return { tag: TypeTag.TForall, id, type } }

export function typeStr(type: Type): string {
	if(isTVar(type)) return type.id;
	if(isTExists(type)) return type.id;
	if(isTFun(type)) return `(${type.left} -> ${type.right})`;
	if(isTForall(type)) return `(forall ${type.id}.${type.type})`;
	if(isTUnit(type)) return `()`;
	return impossible<string>();
}

export function typeEq(a: Type, b: Type): boolean {
	if(isTVar(a) && isTVar(b)) return a.id === b.id;
	if(isTExists(a) && isTExists(b)) return a.id === b.id;
	if(isTFun(a) && isTFun(b)) return typeEq(a.left, b.left) && typeEq(a.right, b.right);
	if(isTForall(a) && isTForall(b)) return a.id === b.id && typeEq(a.type, b.type);
	if(isTUnit(a) && isTUnit(b)) return true;
	return impossible<boolean>();
}

export function isMono(type: Type): boolean {
	if(isTVar(type)) return true;
	if(isTExists(type)) return true;
	if(isTFun(type)) return isMono(type.left) && isMono(type.right);
	if(isTForall(type)) return false;
	if(isTUnit(type)) return true;
	return impossible<boolean>();
}

export function subst(id: string, type: Type, inType: Type): Type {
	if(isTVar(inType)) return inType.id === id? type: inType;
	if(isTExists(inType)) return inType.id === id? type: inType;
	if(isTFun(inType)) return tfun(subst(id, type, inType.left), subst(id, type, inType.right));
	if(isTForall(inType)) return inType.id === id? inType: tforall(inType.id, subst(id, type, inType.type));
	if(isTUnit(inType)) return inType;
	return impossible<Type>();
}

export function free(type: Type): Map<string> {
	if(isTVar(type)) return set(type.id);
	if(isTExists(type)) return set(type.id);
	if(isTFun(type)) return union(free(type.left), free(type.right));
	if(isTForall(type)) return remove(free(type.type), type.id);
	if(isTUnit(type)) return set<string>();
	return impossible<Map<string>>();
}

import { Name } from './names';
import { ADT, cases } from './utils';
import { Type, Forall, showType, showForall } from './types';

export type ElemTag = { CTVar: CTVar, CMeta: CMeta, CVar: CVar, CMarker: CMarker };
export type Elem = ADT<ElemTag>;

export type CTVar = { tag: 'CTVar', name: Name };
export const CTVar = (name: Name): Elem => ({ tag: 'CTVar', name });

export type CMeta = { tag: 'CMeta', name: Name, type: Type | null };
export const CMeta = (name: Name, type: Type | null): Elem => ({ tag: 'CMeta', name, type });

export type CVar = { tag: 'CVar', name: Name, type: Forall };
export const CVar = (name: Name, type: Forall): Elem => ({ tag: 'CVar', name, type });

export type CMarker = { tag: 'CMarker', name: Name };
export const CMarker = (name: Name): Elem => ({ tag: 'CMarker', name });

export const caseElem = cases<ElemTag>();

export const showElem = (elem: Elem) => caseElem(elem, {
  CTVar: val => `${val.name}`,
  CMeta: val => val.type ? `^${val.name} = ${showType(val.type)}` : `^${val.name}`,
  CVar: val => `${val.name} : ${showForall(val.type)}`,
  CMarker: val => `|>${val.name}`,
});

export type Context = Elem[];

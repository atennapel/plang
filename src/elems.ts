import { Name } from './names';
import { Type, Forall, showType, showForall } from './types';

export type Elem = CTVar | CMeta | CVar | CMarker;

export interface CTVar { tag: 'CTVar', name: Name };
export const CTVar = (name: Name): Elem => ({ tag: 'CTVar', name });

export interface CMeta { tag: 'CMeta', name: Name, type: Type | null };
export const CMeta = (name: Name, type: Type | null): Elem => ({ tag: 'CMeta', name, type });

export interface CVar { tag: 'CVar', name: Name, type: Forall };
export const CVar = (name: Name, type: Forall): Elem => ({ tag: 'CVar', name, type });

export interface CMarker { tag: 'CMarker', name: Name };
export const CMarker = (name: Name): Elem => ({ tag: 'CMarker', name });

export type CasesElem<R> = {
  CTVar: (name: Name) => R;
  CMeta: (name: Name, type: Type | null) => R;
  CVar: (name: Name, type: Forall) => R;
  CMarker: (name: Name) => R;
};
export const caseOf = <R>(val: Elem, cs: CasesElem<R>): R => {
  switch (val.tag) {
    case 'CTVar': return cs.CTVar(val.name);
    case 'CMeta': return cs.CMeta(val.name, val.type);
    case 'CVar': return cs.CVar(val.name, val.type);
    case 'CMarker': return cs.CMarker(val.name);
  }
};

export const show = (elem: Elem) => caseOf(elem, {
  CTVar: name => `${name}`,
  CMeta: (name, type) => type ? `^${name} = ${showType(type)}` : `^${name}`,
  CVar: (name, type) => `${name} : ${showForall(type)}`,
  CMarker: name => `|>${name}`,
});

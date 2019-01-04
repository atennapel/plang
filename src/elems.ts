import { impossible } from './errors';
import { Name, showName, eqName } from './names';
import { Type, showType, prettyType } from './types';
import { Kind, showKind, prettyKind } from './kinds';

export type Elem
  = CKVar
  | CKMeta
  | CTVar
  | CTMeta
  | CVar
  | CMarker
  | CClass;

export interface CKVar {
  readonly tag: 'CKVar';
  readonly name: Name;
}
export const CKVar = (name: Name): CKVar =>
  ({ tag: 'CKVar', name });
export const isCKVar = (elem: Elem): elem is CKVar =>
  elem.tag === 'CKVar';
export const matchCKVar = (name: Name) => (elem: Elem): elem is CKVar =>
  isCKVar(elem) && eqName(elem.name, name);

export interface CKMeta {
  readonly tag: 'CKMeta';
  readonly name: Name;
  readonly kind: Kind | null;
}
export const CKMeta = (name: Name, kind: Kind | null = null): CKMeta =>
  ({ tag: 'CKMeta', name, kind });
export const isCKMeta = (elem: Elem): elem is CKMeta =>
  elem.tag === 'CKMeta';
export const matchCKMeta = (name: Name) => (elem: Elem): elem is CKMeta =>
  isCKMeta(elem) && eqName(elem.name, name);

export const isKMetaUnsolved = (elem: Elem): elem is CKMeta =>
  isCKMeta(elem) && !elem.kind;

export interface CTVar {
  readonly tag: 'CTVar';
  readonly name: Name;
  readonly kind: Kind;
}
export const CTVar = (name: Name, kind: Kind): CTVar =>
  ({ tag: 'CTVar', name, kind });
export const isCTVar = (elem: Elem): elem is CTVar =>
  elem.tag === 'CTVar';
export const matchCTVar = (name: Name) => (elem: Elem): elem is CTVar =>
  isCTVar(elem) && eqName(elem.name, name);

export interface CTMeta {
  readonly tag: 'CTMeta';
  readonly name: Name;
  readonly kind: Kind;
  readonly cs: Name[];
  readonly type: Type | null;
}
export const CTMeta = (name: Name, kind: Kind, cs: Name[], type: Type | null = null): CTMeta =>
  ({ tag: 'CTMeta', name, kind, cs, type });
export const isCTMeta = (elem: Elem): elem is CTMeta =>
  elem.tag === 'CTMeta';
export const matchCTMeta = (name: Name) => (elem: Elem): elem is CTMeta =>
  isCTMeta(elem) && eqName(elem.name, name);

export const isTMetaUnsolved = (elem: Elem): elem is CTMeta =>
  isCTMeta(elem) && !elem.type;

export const isUnsolved = (elem: Elem): elem is CKMeta | CTMeta =>
  isKMetaUnsolved(elem) || isTMetaUnsolved(elem);

export interface CVar {
  readonly tag: 'CVar';
  readonly name: Name;
  readonly type: Type;
}
export const CVar = (name: Name, type: Type): CVar =>
  ({ tag: 'CVar', name, type });
export const isCVar = (elem: Elem): elem is CVar =>
  elem.tag === 'CVar';
export const matchCVar = (name: Name) => (elem: Elem): elem is CVar =>
  isCVar(elem) && eqName(elem.name, name);

export interface CMarker {
  readonly tag: 'CMarker';
  readonly name: Name;
}
export const CMarker = (name: Name): CMarker =>
  ({ tag: 'CMarker', name });
export const isCMarker = (elem: Elem): elem is CMarker =>
  elem.tag === 'CMarker';
export const matchCMarker = (name: Name) => (elem: Elem): elem is CMarker =>
  isCMarker(elem) && eqName(elem.name, name);

export interface CClass {
  readonly tag: 'CClass';
  readonly name: Name;
}
export const CClass = (name: Name): CClass =>
  ({ tag: 'CClass', name });
export const isCClass = (elem: Elem): elem is CClass =>
  elem.tag === 'CClass';
export const matchCClass = (name: Name) => (elem: Elem): elem is CClass =>
  isCClass(elem) && eqName(elem.name, name);

export const showElem = (elem: Elem): string => {
  if (isCKVar(elem)) return `kind ${showName(elem.name)}`;
  if (isCKMeta(elem)) return `?${showName(elem.name)}${elem.kind ? ` = ${showKind(elem.kind)}` : ''}`;
  if (isCTVar(elem)) return `${showName(elem.name)} : ${showKind(elem.kind)}`;
  if (isCTMeta(elem)) return `?${showName(elem.name)} : ${showKind(elem.kind)}${elem.cs.length > 0 ? ` with [${elem.cs.map(showName).join(', ')}]` : ''}${elem.type ? ` = ${showType(elem.type)}` : ''}`;
  if (isCVar(elem)) return `${showName(elem.name)} : ${showType(elem.type)}`;
  if (isCMarker(elem)) return `|>${showName(elem.name)}`;
  if (isCClass(elem)) return `constraint ${showName(elem.name)}`;
  return impossible('showElem');
};

export const prettyElem = (elem: Elem): string => {
  if (isCKVar(elem)) return `kind ${showName(elem.name)}`;
  if (isCKMeta(elem)) return `?${showName(elem.name)}${elem.kind ? ` = ${prettyKind(elem.kind)}` : ''}`;
  if (isCTVar(elem)) return `${showName(elem.name)} : ${prettyKind(elem.kind)}`;
  if (isCTMeta(elem)) return `?${showName(elem.name)} : ${prettyKind(elem.kind)}${elem.cs.length > 0 ? ` with [${elem.cs.map(showName).join(', ')}]` : ''}${elem.type ? ` = ${prettyType(elem.type)}` : ''}`;
  if (isCVar(elem)) return `${showName(elem.name)} : ${prettyType(elem.type)}`;
  if (isCMarker(elem)) return `|>${showName(elem.name)}`;
  if (isCClass(elem)) return `constraint ${showName(elem.name)}`;
  return impossible('prettyElem');
};

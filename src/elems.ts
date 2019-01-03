import { impossible } from './errors';
import { Name, showName, eqName } from './names';
import { Type, showType } from './types';

export type Elem
  = CTVar
  | CTMeta
  | CVar
  | CMarker;

export interface CTVar {
  readonly tag: 'CTVar';
  readonly name: Name;
}
export const CTVar = (name: Name): CTVar =>
  ({ tag: 'CTVar', name });
export const isCTVar = (elem: Elem): elem is CTVar =>
  elem.tag === 'CTVar';
export const matchCTVar = (name: Name) => (elem: Elem): elem is CTVar =>
  isCTVar(elem) && eqName(elem.name, name);

export interface CTMeta {
  readonly tag: 'CTMeta';
  readonly name: Name;
  type: Type | null;
}
export const CTMeta = (name: Name, type: Type | null = null): CTMeta =>
  ({ tag: 'CTMeta', name, type });
export const isCTMeta = (elem: Elem): elem is CTMeta =>
  elem.tag === 'CTMeta';
export const matchCTMeta = (name: Name) => (elem: Elem): elem is CTMeta =>
  isCTMeta(elem) && eqName(elem.name, name);

export const isUnsolved = (elem: Elem): elem is CTMeta =>
  isCTMeta(elem) && !elem.type;
export const solveCTMeta = (elem: CTMeta, type: Type): CTMeta => {
  elem.type = type;
  return elem;
};

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

export const showElem = (elem: Elem): string => {
  if (isCTVar(elem)) return `${showName(elem.name)}`;
  if (isCTMeta(elem)) return `?${showName(elem.name)}${elem.type ? ` = ${showType(elem.type)}` : ''}`;
  if (isCVar(elem)) return `${showName(elem.name)} = ${showType(elem.type)})`;
  if (isCMarker(elem)) return `|>${showName(elem.name)}`;
  return impossible('showElem');
};

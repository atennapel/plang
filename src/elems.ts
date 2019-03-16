import { NameT, showName } from './names';
import { Kind, showKind } from './kinds';
import { Type, showType } from './types';

export type Elem
  = CKVar
  | CKMeta
  | CTVar
  | CTMeta
  | CVar
  | CMarker;

export type ElemTag = Elem['tag'];

export type ElemFromTag<T extends ElemTag> = {
  CKVar: CKVar;
  CKMeta: CKMeta;
  CTVar: CTVar;
  CTMeta: CTMeta;
  CVar: CVar;
  CMarker: CMarker;
}[T];

export interface CKVar {
  readonly tag: 'CKVar';
  readonly name: NameT;
}
export const CKVar = (name: NameT): CKVar => ({ tag: 'CKVar', name });
export const isCKVar = (elem: Elem): elem is CKVar => elem.tag === 'CKVar';

export interface CKMeta {
  readonly tag: 'CKMeta';
  readonly name: NameT;
  readonly kind: Kind | null;
}
export const CKMeta = (name: NameT, kind: Kind | null = null): CKMeta =>
  ({ tag: 'CKMeta', name, kind });
export const isCKMeta = (elem: Elem): elem is CKMeta => elem.tag === 'CKMeta';

export interface CTVar {
  readonly tag: 'CTVar';
  readonly name: NameT;
  readonly kind: Kind;
}
export const CTVar = (name: NameT, kind: Kind): CTVar =>
  ({ tag: 'CTVar', name, kind });
export const isCTVar = (elem: Elem): elem is CTVar => elem.tag === 'CTVar';

export interface CTMeta {
  readonly tag: 'CTMeta';
  readonly name: NameT;
  readonly kind: Kind;
  readonly type: Type | null;
}
export const CTMeta = (name: NameT, kind: Kind, type: Type | null = null): CTMeta =>
  ({ tag: 'CTMeta', name, kind, type });
export const isCTMeta = (elem: Elem): elem is CTMeta => elem.tag === 'CTMeta';

export interface CVar {
  readonly tag: 'CVar';
  readonly name: NameT;
  readonly type: Type;
}
export const CVar = (name: NameT, type: Type): CVar =>
  ({ tag: 'CVar', name, type });
export const isCVar = (elem: Elem): elem is CVar => elem.tag === 'CVar';

export interface CMarker {
  readonly tag: 'CMarker';
  readonly name: NameT;
}
export const CMarker = (name: NameT): CMarker => ({ tag: 'CMarker', name });
export const isCMarker = (elem: Elem): elem is CMarker => elem.tag === 'CMarker';

export const showElem = (elem: Elem): string => {
  switch (elem.tag) {
    case 'CKVar':
      return `kind ${showName(elem.name)}`;
    case 'CKMeta':
      return `kind ?${showName(elem.name)}${elem.kind ? ` = ${showKind(elem.kind)}` : ''}`;
    case 'CTVar':
      return `${showName(elem.name)} :k ${showKind(elem.kind)}`;
    case 'CTMeta':
      return `?${showName(elem.name)} :k ${showKind(elem.kind)}${elem.type ? ` = ${showType(elem.type)}` : ''}`;
    case 'CVar':
      return `${showName(elem.name)} : ${showType(elem.type)}`;
    case 'CMarker':
      return `|>${showName(elem.name)}`;
  }
};

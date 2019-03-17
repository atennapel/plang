import { NameT, showName } from "./names";
import { Type, showType } from "./types";
import { Kind, showKind } from "./kinds";
import { Term, showTerm } from "./terms";

export type Def
  = DType
  | DLet
  | DDeclType
  | DDeclare;

export interface DType {
  readonly tag: 'DType';
  readonly name: NameT;
  readonly args: [NameT, Kind | null][];
  readonly type: Type;
}
export const DType = (name: NameT, args: [NameT, Kind | null][], type: Type): DType =>
  ({ tag: 'DType', name, args, type });
export const isDType = (def: Def): def is DType => def.tag === 'DType';

export interface DLet {
  readonly tag: 'DLet';
  readonly name: NameT;
  readonly args: NameT[];
  readonly term: Term;
}
export const DLet = (name: NameT, args: NameT[], term: Term): DLet =>
  ({ tag: 'DLet', name, args, term });
export const isDLet = (def: Def): def is DLet => def.tag === 'DLet';

export interface DDeclType {
  readonly tag: 'DDeclType';
  readonly name: NameT;
  readonly kind: Kind;
}
export const DDeclType = (name: NameT, kind: Kind): DDeclType =>
  ({ tag: 'DDeclType', name, kind });
export const isDDeclType = (def: Def): def is DDeclType => def.tag === 'DDeclType';

export interface DDeclare {
  readonly tag: 'DDeclare';
  readonly name: NameT;
  readonly type: Type;
}
export const DDeclare = (name: NameT, type: Type): DDeclare =>
  ({ tag: 'DDeclare', name, type });
export const isDDeclare = (def: Def): def is DDeclare => def.tag === 'DDeclare';

export const showDef = (def: Def): string => {
  switch (def.tag) {
    case 'DType': {
      const args = def.args.length > 0 ?
        `${def.args.map(([n, k]) => k ? `(${showName(n)} : ${showKind(k)})` : showName(n)).join(' ')} ` :
        '';
      return `type ${showName(def.name)} ${args}= ${showType(def.type)}`;
    }
    case 'DLet': {
      const args = def.args.length > 0 ? `${def.args.map(showName).join(' ')} ` : '';
      return `let ${showName(def.name)} ${args}= ${showTerm(def.term)}`;
    }
    case 'DDeclType': return `decltype ${showName(def.name)} : ${showKind(def.kind)}`;
    case 'DDeclare': return `declare ${showName(def.name)} : ${showType(def.type)}`;
  }
};

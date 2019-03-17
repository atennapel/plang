import { NameT, showName } from "./names";
import { Type, showType } from "./types";
import { Kind, showKind } from "./kinds";
import { Term, showTerm } from "./terms";

export type Def
  = DType
  | DLet;

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
  }
};

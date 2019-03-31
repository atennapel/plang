import { Name } from './util';
import { Kind, showKind } from './kinds';
import { Type, showTy } from './types';
import { Pat, Term, showTerm, showPat } from './terms';

export type Def
  = DType
  | DLet;

export interface DType {
  readonly tag: 'DType';
  readonly name: Name;
  readonly args: [Name, Kind | null][];
  readonly type: Type;
}
export const DType = (name: Name, args: [Name, Kind | null][], type: Type): DType =>
  ({ tag: 'DType', name, args, type });
export const isDType = (def: Def): def is DType => def.tag === 'DType';

export interface DLet {
  readonly tag: 'DLet';
  readonly name: Name;
  readonly args: Pat[];
  readonly term: Term;
}
export const DLet = (name: Name, args: Pat[], term: Term): DLet =>
  ({ tag: 'DLet', name, args, term });
export const isDLet = (def: Def): def is DLet => def.tag === 'DLet';

export const showDef = (def: Def): string => {
  switch (def.tag) {
    case 'DType': {
      const args = def.args.length > 0 ?
        `${def.args.map(([n, k]) => k ? `(${n} : ${showKind(k)})` : n).join(' ')} ` :
        '';
      return `type ${def.name} ${args}= ${showTy(def.type)}`;
    }
    case 'DLet': {
      const args = def.args.length > 0 ? `${def.args.map(showPat).join(' ')} ` : '';
      return `let ${def.name} ${args}= ${showTerm(def.term)}`;
    }
  }
};

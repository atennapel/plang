import { Name } from './names';
import { Kind, showKind } from './kinds';

export type Type = TVar | TMeta | TApp;

export interface TVar {
  readonly tag: 'TVar';
  readonly name: Name;
};
export const TVar = (name: Name): Type =>
  ({ tag: 'TVar', name });

export interface TMeta {
  readonly tag: 'TMeta';
  readonly name: Name;
  type: Type | null;
};
export const TMeta = (name: Name, type: Type | null): Type =>
  ({ tag: 'TMeta', name, type });

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type;
  readonly right: Type;
  kind: Kind | null;
};
export const TApp = (left: Type, right: Type, kind: Kind | null): Type =>
  ({ tag: 'TApp', left, right, kind });

export type CasesType<R> = {
  TVar: (name: Name) => R;
  TMeta: (name: Name, type: Type | null) => R;
  TApp: (left: Type, right: Type, kind: Kind | null) => R;
};
export const caseType = <R>(val: Type, cs: CasesType<R>): R => {
  switch (val.tag) {
    case 'TVar': return cs.TVar(val.name);
    case 'TMeta': return cs.TMeta(val.name, val.type);
    case 'TApp': return cs.TApp(val.left, val.right, val.kind);
  }
};

export const showType = (type: Type): string => caseType(type, {
  TVar: name => `${name}`,
  TMeta: name => `^${name}`,
  TApp: (left, right) => `(${showType(left)} ${showType(right)})`,
});

export interface Forall { tag: 'Forall', args: [Name, Kind][], type: Type };
export const Forall = (args: [Name, Kind][], type: Type): Forall => ({ tag: 'Forall', args, type });

export const showForall = (forall: Forall) =>
  forall.args.length === 0 ?
    showType(forall.type) :
    `forall ${forall.args.map(([n, k]) => `(${n} : ${showKind(k)})`).join('')}. ${showType(forall.type)}`;

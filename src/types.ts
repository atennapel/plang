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
  readonly kind: Kind;
  type: Type | null;
};
export const TMeta = (name: Name, kind: Kind, type: Type | null = null): Type =>
  ({ tag: 'TMeta', name, kind, type });

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type;
  readonly right: Type;
  kind: Kind | null;
};
export const TApp = (left: Type, right: Type, kind: Kind | null = null): Type =>
  ({ tag: 'TApp', left, right, kind });
export const tapp = (...ts: Type[]): Type => ts.reduce((a, b) => TApp(a, b));

export type CasesType<R> = {
  TVar: (name: Name) => R;
  TMeta: (name: Name, kind: Kind, type: Type | null) => R;
  TApp: (left: Type, right: Type, kind: Kind | null) => R;
};
export const caseType = <R>(val: Type, cs: CasesType<R>): R => {
  switch (val.tag) {
    case 'TVar': return cs.TVar(val.name);
    case 'TMeta': return cs.TMeta(val.name, val.kind, val.type);
    case 'TApp': return cs.TApp(val.left, val.right, val.kind);
  }
};

export const showType = (type: Type): string => caseType(type, {
  TVar: name => `${name}`,
  TMeta: name => `?${name}`,
  TApp: (left, right) =>
    left.tag === 'TApp' && left.left.tag === 'TVar' && /[^a-z]/i.test(left.left.name[0]) ?
      `(${showType(left.right)} ${left.left.name} ${showType(right)})` :
      `(${showType(left)} ${showType(right)})`,
});

export type Subst = { [key: string]: Type };
export const substMeta = (sub: Subst, type: Type): Type => caseType(type, {
  TVar: name => type,
  TMeta: name => sub[name] ? sub[name] : type,
  TApp: (left, right) => TApp(substMeta(sub, left), substMeta(sub, right)),
});
export const substTVar = (sub: Subst, type: Type): Type => caseType(type, {
  TVar: name => sub[name] ? sub[name] : type,
  TMeta: name => type,
  TApp: (left, right) => TApp(substTVar(sub, left), substTVar(sub, right)),
});

export type Free = { [key: string]: TMeta };
export const freeMeta = (type: Type, fr: Free = {}): Free => caseType(type, {
  TVar: name => fr,
  TMeta: name => { fr[name] = type as TMeta; return fr },
  TApp: (left, right) => freeMeta(right, freeMeta(left, fr)),
});

export interface Forall { tag: 'Forall', args: [Name, Kind][], type: Type };
export const Forall = (args: [Name, Kind][], type: Type): Forall => ({ tag: 'Forall', args, type });

export const showForall = (forall: Forall) =>
  forall.args.length === 0 ?
    showType(forall.type) :
    `forall ${forall.args.map(([n, k]) => `(${n} : ${showKind(k)})`).join('')}. ${showType(forall.type)}`;

export const nFun = '->';
export const tFun = TVar(nFun);
export const TFun = (a: Type, b: Type) => TApp(TApp(tFun, a), b);
export const tfun = (...ts: Type[]): Type => ts.reduceRight((a, b) => TFun(b, a));
export const matchTFun = (type: Type): { left: Type, right: Type } | null =>
  type.tag === 'TApp' && type.left.tag === 'TApp' &&
    (type.left.left === tFun || (type.left.left.tag === 'TVar' && type.left.left.name === nFun)) ?
    { left: type.left.right, right: type.right } : null;

export const nEffsEmpty = '{}';
export const tEffsEmpty = TVar(nEffsEmpty);
export const isTEffsEmpty = (type: Type): type is TVar =>
  type === tEffsEmpty || (type.tag === 'TVar' && type.name === nEffsEmpty);

export const nEffsExtend = '|';
export const tEffsExtend = TVar(nEffsExtend);
export const TEffsExtend = (a: Type, b: Type) => TApp(TApp(tEffsExtend, a), b);
export const matchTEffsExtend = (type: Type): { eff: Type, rest: Type } | null =>
  type.tag === 'TApp' && type.left.tag === 'TApp' &&
    (type.left.left === tEffsExtend || (type.left.left.tag === 'TVar' && type.left.left.name === nEffsExtend)) ?
    { eff: type.left.right, rest: type.right } : null;
export const teffs = (es: Type[], rest: Type = tEffsEmpty): Type =>
  es.reduceRight((a, b) => TEffsExtend(b, a), rest);

import { Name, namePart, fresh } from './names';
import { Kind, showKind, eqKind, kType, prettyKind } from './kinds';

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
export const freshMeta = (name: Name = 't', kind: Kind = kType): Type =>
  TMeta(fresh(name), kind);

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type;
  readonly right: Type;
  kind: Kind | null;
};
export const TApp = (left: Type, right: Type, kind: Kind | null = null): Type =>
  ({ tag: 'TApp', left, right, kind });
export const tapp = (...ts: Type[]): Type => ts.reduce((a, b) => TApp(a, b));
export const flattenTApp = (type: Type, ts: Type[] = []): Type[] => {
  if (type.tag === 'TApp') {
    const rec = flattenTApp(type.left, ts);
    ts.push(type.right);
    return ts;
  }
  ts.push(type);
  return ts;
};

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
    left.tag === 'TApp' && left.left.tag === 'TApp' && left.left.left.tag === 'TVar' && left.left.left.name === '->' ?
      (left.right.tag === 'TVar' && left.right.name === '{}' ?
        `(${showType(left.left.right)} -> ${showType(right)})` :
        `(${showType(left.left.right)} -> ${showType(right)}!${showType(left.right)})`) :
    left.tag === 'TApp' && left.left.tag === 'TVar' && /[^a-z]/i.test(left.left.name[0]) && left.left.name !== '->'?
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
export const containsMeta = (m: Name, type: Type): boolean => caseType(type, {
  TVar: name => false,
  TMeta: name => name === m,
  TApp: (left, right) => containsMeta(m, left) || containsMeta(m, right),
});

export type OccMap = { [key: string]: number };
export const occTVar = (type: Type, tvs: Name[], map: OccMap = {}): OccMap => caseType(type, {
  TVar: name => tvs.indexOf(name) >= 0 ? ((map[name] = (map[name] || 0) + 1), map) : map,
  TMeta: name => map,
  TApp: (left, right) => occTVar(right, tvs, occTVar(left, tvs, map)),
});

export interface Forall { tag: 'Forall', args: [Name, Kind][], type: Type };
export const Forall = (args: [Name, Kind][], type: Type): Forall => ({ tag: 'Forall', args, type });

export const showForall = (forall: Forall) =>
  forall.args.length === 0 ?
    showType(forall.type) :
    `forall${forall.args.map(([n, k]) => `(${n} : ${showKind(k)})`).join('')}. ${showType(forall.type)}`;

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
export const flattenTEffs = (type: Type, ts: Type[] = []): { ts: Type[], rest: Type } => {
  const m = matchTEffsExtend(type);
  if (m) {
    ts.push(m.eff);
    const rec = flattenTEffs(m.rest, ts);
    return { ts, rest: rec.rest };
  }
  return { ts, rest: type };
};

export const nFun = '->';
export const tFun = TVar(nFun);
export const TFun = (a: Type, e: Type, b: Type) => TApp(TApp(TApp(tFun, a), e), b);
export const TFunP = (a: Type, b: Type) => TFun(a, tEffsEmpty, b);
export const tfun = (...ts: Type[]): Type => ts.reduceRight((a, b) => TFunP(b, a));
export const matchTFun = (type: Type): { left: Type, eff: Type, right: Type } | null =>
  type.tag === 'TApp' && type.left.tag === 'TApp' && type.left.left.tag === 'TApp' &&
    (type.left.left.left === tFun || (type.left.left.left.tag === 'TVar' && type.left.left.left.name === nFun)) ?
    { left: type.left.left.right, eff: type.left.right, right: type.right } : null;
export const flattenTFun = (type: Type, ts: Type[] = []): { ts: Type[], eff: Type } => {
  const f = matchTFun(type);
  if (f) {
    if (isTEffsEmpty(f.eff)) {
      ts.push(f.left);
      const rec = flattenTFun(f.right, ts);
      return { ts, eff: rec.eff };
    } else {
      ts.push(f.left);
      ts.push(f.right);
      return { ts, eff: f.eff };
    }
  }
  ts.push(type);
  return { ts, eff: tEffsEmpty };
};

const ARR = '->';
export const prettyType = (type: Type): string => {
  const f = matchTFun(type);
  if (f) {
    const fl = flattenTFun(type);
    return `${fl.ts.map(t => matchTFun(t) ? `(${prettyType(t)})` : prettyType(t)).join(` ${ARR} `)}${isTEffsEmpty(fl.eff) ? '' : `!${prettyType(fl.eff)}`}`;
  }
  if (isTEffsEmpty(type)) return '{}';
  const m = matchTEffsExtend(type);
  if (m) {
    const fm = flattenTEffs(type);
    return `{${fm.ts.map(prettyType).join(', ')}${isTEffsEmpty(fm.rest) ? '' : ` | ${prettyType(fm.rest)}`}`;
  }
  if (type.tag === 'TApp') {
    const ta = flattenTApp(type);
    return `${ta.map(t => t.tag === 'TApp' || matchTFun(t) ? `(${prettyType(t)})` : prettyType(t)).join(' ')}`;
  }
  return showType(type);
};

const FORALL = 'forall';
export const prettyForall = (forall: Forall) => {
  if (forall.args.length === 0)
    return prettyType(forall.type);
  const ns = forall.args.map(x => x[0]);
  const map: OccMap = {};
  const sub: Subst = {};
  for (let i = 0; i < ns.length; i++) {
    const on = ns[i];
    const n = namePart(on);
    if (!map[n]) map[n] = 0;
    const j = map[n]++;
    sub[on] = TVar(`${n}${j > 0 ? `\$${j}` : ''}`);
  }
  const nargs = forall.args.map(([x, k]) => [(sub[x] as TVar).name, k] as [Name, Kind]);
  return `${FORALL} ${nargs.map(([n, k]) => eqKind(kType, k) ? `${n}` : `(${n} : ${prettyKind(k)})`).join(' ')}. ${prettyType(substTVar(sub, forall.type))}`;
};

export interface TypeEff {
  readonly type: Type;
  readonly eff: Type;
}
export const typeEff = (type: Type, eff: Type): TypeEff => ({ type, eff });
export const typePure = (type: Type) => typeEff(type, tEffsEmpty);
export const showTypeEff = (type: TypeEff) => `${showType(type.type)}!${showType(type.eff)}`;

import { Name, Id, impossible } from './util';
import { Kind, showKind } from './kinds';
import { log, config } from './config';

export type Type
  = TForall
  | TApp
  | TCon
  | TVar
  | TSkol
  | TMeta;

export interface TForall {
  readonly tag: 'TForall';
  readonly names: Name[];
  readonly kinds: (Kind | null)[];
  readonly type: Type;
}
export const TForall = (
  names: Name[],
  kinds: (Kind | null)[],
  type: Type
): TForall => ({ tag: 'TForall', names, kinds, type });
export const tforall = (ns: [Name, Kind | null][], type: Type) => {
  if (ns.length === 0) return type;
  const [names, kinds] = ns.reduce((c, [x, k]) => {
    c[0].push(x);
    c[1].push(k);
    return c
  }, [[], []] as [Name[], (Kind | null)[]]);
  return TForall(names, kinds, type);
};

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type;
  readonly right: Type;
}
export const TApp = (left: Type, right: Type): TApp =>
  ({ tag: 'TApp', left, right });
export const tappFrom = (ts: Type[]): Type =>
  ts.reduce(TApp);

export interface TCon {
  readonly tag: 'TCon';
  readonly name: Name;
}
export const TCon = (name: Name): TCon => ({ tag: 'TCon', name });

export interface TVar {
  readonly tag: 'TVar';
  readonly name: Name;
}
export const TVar = (name: Name): TVar => ({ tag: 'TVar', name });

export interface TSkol {
  readonly tag: 'TSkol';
  readonly name: Name;
  readonly id: Id;
  readonly kind: Kind;
}
export const TSkol = (name: Name, id: Id, kind: Kind): TSkol =>
  ({ tag: 'TSkol', name, id, kind });

export interface TMeta {
  readonly tag: 'TMeta';
  readonly id: Id;
  readonly kind: Kind;
  name: Name | null;
  type: Type | null;
}
export const TMeta = (id: Id, kind: Kind, name: Name | null = null): TMeta =>
  ({ tag: 'TMeta', id, kind, name, type: null });

export interface TFun {
  readonly tag: 'TApp';
  readonly left: {
    readonly tag: 'TApp';
    readonly left: TCon;
    readonly right: Type;
  }
  readonly right: Type;
}
export const tFun = TCon('->');
export const TFun = (left: Type, right: Type): TFun =>
  TApp(TApp(tFun, left), right) as TFun;
export const isTFun = (ty: Type): ty is TFun =>
  ty.tag === 'TApp' && ty.left.tag === 'TApp' &&
    (ty.left.left === tFun ||
      (ty.left.left.tag === 'TCon' &&
        ty.left.left.name === tFun.name));
export const tfunFrom = (ts: Type[]): Type =>
  ts.reduceRight((x, y) => TFun(y, x));

export const flattenTFun = (t: Type): Type[] => {
  let c = t;
  const r: Type[] = [];
  while (isTFun(c)) {
    r.push(c.left.right);
    c = c.right;
  }
  r.push(c);
  return r;
};

export const flattenTApp = (t: Type): Type[] => {
  let c = t;
  const r: Type[] = [];
  while (c.tag === 'TApp') {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};

export const showTy = (t: Type): string => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TVar') return t.name;
  if (t.tag === 'TMeta') return `?${t.name ? `${t.name}\$` : ''}${t.id}`;
  if (t.tag === 'TSkol') return `'${t.name}\$${t.id}`;
  if (t.tag === 'TForall')
    return `forall ${t.names.map((tv, i) =>
      t.kinds[i] && config.showKinds ?
        `(${tv} : ${showKind(t.kinds[i] as Kind)})` :
        `${tv}`).join(' ')}. ${showTy(t.type)}`;
  if (isTFun(t))
    return flattenTFun(t)
      .map(t => isTFun(t) || t.tag === 'TForall' ? `(${showTy(t)})` : showTy(t))
      .join(' -> ');
  if (t.tag === 'TApp')
    return flattenTApp(t)
      .map(t => t.tag === 'TApp' || t.tag === 'TForall' ? `(${showTy(t)})` : showTy(t))
      .join(' ');
  return impossible('showTy');
};

export type TVMap = { [key: string]: Type };
export const substTVar = (map: TVMap, ty: Type): Type => {
  if (ty.tag === 'TVar') return map[ty.name] || ty;
  if (ty.tag === 'TApp')
    return TApp(substTVar(map, ty.left), substTVar(map, ty.right));
  if (ty.tag === 'TForall') {
    const { names, kinds, type } = ty;
    const m: TVMap = {};
    for (let k in map) if (names.indexOf(k) < 0) m[k] = map[k];
    return TForall(names, kinds, substTVar(m, type));
  }
  return ty;
};

export const tmetas = (
  ty: Type,
  free: TMeta[] = [],
  tms: TMeta[] = []
): TMeta[] => {
  if (ty.tag === 'TMeta') {
    if (free.indexOf(ty) >= 0 || tms.indexOf(ty) >= 0) return tms;
    tms.push(ty);
    return tms;
  }
  if (ty.tag === 'TApp')
    return tmetas(ty.right, free, tmetas(ty.left, free, tms));
  if (ty.tag === 'TForall')
    return tmetas(ty.type, free, tms);
  return tms;
};

export const prune = (ty: Type): Type => {
  if (ty.tag === 'TMeta') {
    if (!ty.type) return ty;
    const t = prune(ty.type);
    ty.type = t;
    return t;
  }
  if (ty.tag === 'TApp')
    return TApp(prune(ty.left), prune(ty.right));
  if (ty.tag === 'TForall')
    return TForall(ty.names, ty.kinds, prune(ty.type));
  return ty;
};

export const occursTMeta = (x: TMeta, t: Type): boolean => {
  if (x === t) return true;
  if (t.tag === 'TApp')
    return occursTMeta(x, t.left) || occursTMeta(x, t.right);
  if (t.tag === 'TForall') return occursTMeta(x, t.type);
  return false;
};

export const tbinders = (ty: Type, bs: Name[] = []): Name[] => {
  if (ty.tag === 'TApp')
    return tbinders(ty.right, tbinders(ty.left, bs));
  if (ty.tag === 'TForall') {
    const names = ty.names;
    for (let i = 0, l = names.length; i < l; i++) {
      const x = names[i];
      if (bs.indexOf(x) < 0) bs.push(x);
    }
    return tbinders(ty.type, bs);
  }
  return bs;
};

export const quantify = (tms: TMeta[], ty: Type): Type => {
  log(() => `quantify ${showTy(ty)} with [${tms.map(showTy).join(', ')}]`)
  const len = tms.length;
  if (len === 0) return ty;
  const used = tbinders(ty);
  const tvs = Array(len);
  const ks = Array(len);
  let i = 0;
  let l = 0;
  let j = 0;
  while (i < len) {
    const x = tms[i].name;
    const v = x && used.indexOf(x) < 0 ? x :
      `${String.fromCharCode(l + 97)}${j > 0 ? j : ''}`;
    if (used.indexOf(v) < 0) {
      used.push(v);
      tms[i].type = TVar(v);
      tvs[i] = v;
      ks[i] = tms[i].kind;
      i++;
    }
    l = (l + 1) % 26;
    if (l === 0) j++;
  }
  return TForall(tvs, ks, prune(ty));
};

import { NameT, showName, Name, eqName, NameMap, getNameMap, simplifyName } from './names';
import { Kind, showKind } from './kinds';
import { NameStore } from './namestore';
import { config } from './config';

export type Type
  = TVar
  | TMeta
  | TApp
  | TForall;

export interface TVar {
  readonly tag: 'TVar';
  readonly name: NameT;
}
export const TVar = (name: NameT): TVar => ({ tag: 'TVar', name });
export const isTVar = (type: Type): type is TVar => type.tag === 'TVar';

export interface TMeta {
  readonly tag: 'TMeta';
  readonly name: NameT;
}
export const TMeta = (name: NameT): TMeta => ({ tag: 'TMeta', name });
export const isTMeta = (type: Type): type is TMeta => type.tag === 'TMeta';

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type
  readonly right: Type;
}
export const TApp = (left: Type, right: Type): TApp => ({ tag: 'TApp', left, right });
export const isTApp = (type: Type): type is TApp => type.tag === 'TApp';
export const tappFrom = (ts: Type[]): Type => ts.reduce(TApp);
export const tapp = (...ts: Type[]): Type => tappFrom(ts);

export interface TForall {
  readonly tag: 'TForall';
  readonly name: NameT;
  readonly kind: Kind | null;
  readonly type: Type;
}
export const TForall = (name: NameT, type: Type): TForall =>
  ({ tag: 'TForall', name, kind: null, type });
export const TForallK = (name: NameT, kind: Kind | null, type: Type): TForall =>
  ({ tag: 'TForall', name, kind, type });
export const isTForall = (type: Type): type is TForall => type.tag === 'TForall';
export const tforall = (ns: NameT[], type: Type): Type =>
  ns.reduceRight((t, n) => TForall(n, t), type);
export const tforallK = (ns: [NameT, Kind | null][], type: Type): Type =>
  ns.reduceRight((t, [n, k]) => TForallK(n, k, t), type);

export const nFun = Name('->');
export const tFun = TVar(nFun);
export const TFun = (left: Type, right: Type): TApp => TApp(TApp(tFun, left), right);
export const tfunFrom = (ts: Type[]): Type => ts.reduceRight((x, y) => TFun(y, x));
export const tfun = (...ts: Type[]): Type => tfunFrom(ts);
export const isTFun = (type: Type): type is TApp =>
  isTApp(type) && isTApp(type.left) && isTVar(type.left.left) && eqName(type.left.left.name, nFun);
export const matchTFun = (type: Type): { left: Type, right: Type } | null =>
  isTFun(type) ? { left: (type.left as TApp).right, right: type.right } : null;

export const flattenTApp = (type: Type): Type[] => {
  let c = type;
  const r: Type[] = [];
  while (isTApp(c)) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};
export const flattenTForall = (type: Type): { args: [NameT, Kind | null][], type: Type } => {
  let c = type;
  const args: [NameT, Kind | null][] = [];
  while (isTForall(c)) {
    args.push([c.name, c.kind || null]);
    c = c.type;
  }
  return { args, type: c };
};
export const flattenTFun = (type: Type): Type[] => {
  let c = type;
  const r: Type[] = [];
  let f = matchTFun(c);
  while (f) {
    r.push(f.left);
    c = f.right;
    f = matchTFun(c);
  }
  r.push(c);
  return r;
};
export const showType = (type: Type): string => {
  switch (type.tag) {
    case 'TVar': return showName(type.name);
    case 'TMeta': return `?${showName(type.name)}`;
    case 'TApp': {
      if (isTFun(type))
        return flattenTFun(type)
          .map(t => {
            const s = showType(t);
            return isTFun(t) || isTForall(t) ? `(${s})` : s;
          })
          .join(' -> ');
      return flattenTApp(type)
        .map(t => {
          const s = showType(t);
          return isTApp(t) || isTForall(t) ? `(${s})` : s;
        })
        .join(' ');
    }
    case 'TForall': {
      const f = flattenTForall(type);
      const args = f.args
        .map(([n, k]) => k && config.showKinds ? `(${showName(n)} : ${showKind(k)})` : showName(n))
        .join(' ');
      return `forall ${args}. ${showType(f.type)}`;
    }
  }
};

export const substTVar = (x: NameT, s: Type, type: Type): Type => {
  switch (type.tag) {
    case 'TVar': return eqName(x, type.name) ? s : type;
    case 'TMeta': return type;
    case 'TApp': {
      const left = substTVar(x, s, type.left);
      const right = substTVar(x, s, type.right);
      return type.left === left && type.right === right ? type : TApp(left, right);
    }
    case 'TForall': {
      if (eqName(x, type.name)) return type;
      const body = substTVar(x, s, type.type);
      return type.type === body ? type : TForallK(type.name, type.kind, body);
    }
  }
};
export const openTForall = (forall: TForall, s: Type): Type =>
  substTVar(forall.name, s, forall.type);

export const containsTMeta = (x: NameT, type: Type): boolean => {
  switch (type.tag) {
    case 'TVar': return false;
    case 'TMeta': return eqName(x, type.name);
    case 'TApp': return containsTMeta(x, type.left) || containsTMeta(x, type.right);
    case 'TForall': return containsTMeta(x, type.type);
  }
};

export const substTMetas = (type: Type, m: NameMap<Type>): Type => {
  switch (type.tag) {
    case 'TVar': return type;
    case 'TMeta': return getNameMap(type.name, m) || type;
    case 'TApp': {
      const left = substTMetas(type.left, m);
      const right = substTMetas(type.right, m);
      return type.left === left && type.right === right ? type : TApp(left, right);
    }
    case 'TForall': {
      const body = substTMetas(type.type, m);
      return type.type === body ? type : TForallK(type.name, type.kind, body);
    }
  }
};

export const isMono = (type: Type): boolean => {
  switch (type.tag) {
    case 'TVar': return true;
    case 'TMeta': return true;
    case 'TApp': return isMono(type.left) && isMono(type.right);
    case 'TForall': return false;
  }
};

export const simplifyType = (type: Type, ns: NameStore = new NameStore()): Type => {
  switch (type.tag) {
    case 'TVar':
    case 'TMeta': return type;
    case 'TApp': {
      const left = simplifyType(type.left, ns);
      const right = simplifyType(type.right, ns);
      return type.left === left && type.right === right ? type : TApp(left, right);
    }
    case 'TForall': {
      const x = simplifyName(ns.fresh(type.name));
      const body = simplifyType(type.type, ns);
      return TForallK(x, type.kind, substTVar(type.name, TVar(x), body));
    }
  }
};

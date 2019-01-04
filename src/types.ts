import { impossible } from './errors';
import { Name, showName, eqName } from './names';
import { Kind, showKind, prettyKind } from './kinds';

export type Type
  = TVar
  | TMeta
  | TFun
  | TApp
  | TForall;

export interface TVar {
  readonly tag: 'TVar';
  readonly name: Name;
}
export const TVar = (name: Name): TVar =>
  ({ tag: 'TVar', name });
export const isTVar = (type: Type): type is TVar =>
  type.tag === 'TVar';
export const matchTVar = (name: Name) => (type: Type): type is TVar =>
  isTVar(type) && eqName(type.name, name);

export interface TMeta {
  readonly tag: 'TMeta';
  readonly name: Name;
}
export const TMeta = (name: Name): TMeta =>
  ({ tag: 'TMeta', name });
export const isTMeta = (type: Type): type is TMeta =>
  type.tag === 'TMeta';
export const matchTMeta = (name: Name) => (type: Type): type is TMeta =>
  isTMeta(type) && eqName(type.name, name);

export interface TFun {
  readonly tag: 'TFun';
  readonly left: Type;
  readonly right: Type;
}
export const TFun = (left: Type, right: Type): TFun =>
  ({ tag: 'TFun', left, right });
export const isTFun = (type: Type): type is TFun =>
  type.tag === 'TFun';
export const tfunFrom = (ts: Type[]): Type =>
  ts.reduceRight((x, y) => TFun(y, x));
export const tfun = (...ts: Type[]): Type =>
  tfunFrom(ts);
export const flattenTFun = (type: Type): Type[] => {
  const r: Type[] = [];
  let c = type;
  while (isTFun(c)) {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
};

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type;
  readonly right: Type;
}
export const TApp = (left: Type, right: Type): TApp =>
  ({ tag: 'TApp', left, right });
export const isTApp = (type: Type): type is TApp =>
  type.tag === 'TApp';
export const tappFrom = (ts: Type[]): Type =>
  ts.reduce(TApp);
export const tapp = (...ts: Type[]): Type =>
  tappFrom(ts);
export const flattenTApp = (type: Type): Type[] => {
  const r: Type[] = [];
  let c = type;
  while (isTApp(c)) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};

export interface TForall {
  readonly tag: 'TForall';
  readonly name: Name;
  readonly kind: Kind;
  readonly cs: Name[];
  readonly type: Type;
}
export const TForall = (name: Name, kind: Kind, cs: Name[], type: Type): TForall =>
  ({ tag: 'TForall', name, kind, cs, type });
export const isTForall = (type: Type): type is TForall =>
  type.tag === 'TForall';
export const tforall = (ns: [Name, Kind, Name[]][], type: Type): Type =>
  ns.reduceRight((t, [n, k, c]) => TForall(n, k, c, t), type);
export const flattenTForall = (type: Type): { args: [Name, Kind, Name[]][], type: Type } => {
  const args: [Name, Kind, Name[]][] = [];
  let c = type;
  while (isTForall(c)) {
    args.push([c.name, c.kind, c.cs]);
    c = c.type;
  }
  return { args, type: c };
};

export const showType = (type: Type): string => {
  if (isTVar(type)) return `${showName(type.name)}`;
  if (isTMeta(type)) return `?${showName(type.name)}`;
  if (isTFun(type)) return `(${showType(type.left)} -> ${showType(type.right)})`;
  if (isTApp(type)) return `(${showType(type.left)} ${showType(type.right)})`;
  if (isTForall(type)) return `(forall(${showName(type.name)} : ${showKind(type.kind)}).${type.cs.length > 0 ? ` ${type.cs.map(showName).join(', ')} =>` : ''} ${showType(type.type)})`;
  return impossible('showType');
};

export const prettyType = (type: Type): string => {
  if (isTVar(type)) return `${showName(type.name)}`;
  if (isTMeta(type)) return `?${showName(type.name)}`;
  if (isTFun(type))
    return flattenTFun(type)
      .map(t => isTFun(t) || isTForall(t) ? `(${prettyType(t)})` : prettyType(t))
      .join(' -> ');
  if (isTApp(type))
    return flattenTApp(type)
      .map(t => isTFun(t) || isTForall(t) || isTApp(t) ? `(${prettyType(t)})` : prettyType(t))
      .join(' ');
  if (isTForall(type)) {
    const f = flattenTForall(type);
    return `forall${f.args.map(([n, k]) => `(${showName(n)} : ${prettyKind(k)})`).join('')}.${type.cs.length > 0 ? ` ${type.cs.map(showName).join(', ')} =>` : ''} ${prettyType(f.type)}`;
  }
  return impossible('prettyType');
};

export const isMono = (type: Type): boolean => {
  if (isTForall(type)) return false;
  if (isTFun(type)) return isMono(type.left) && isMono(type.right);
  if (isTApp(type)) return isMono(type.left) && isMono(type.right);
  return true;
};

export const substTVar = (tv: Name, sub: Type, type: Type): Type => {
  if (isTVar(type)) return eqName(type.name, tv) ? sub : type;
  if (isTMeta(type)) return type;
  if (isTFun(type)) return TFun(substTVar(tv, sub, type.left), substTVar(tv, sub, type.right));
  if (isTApp(type)) return TApp(substTVar(tv, sub, type.left), substTVar(tv, sub, type.right));
  if (isTForall(type)) return eqName(type.name, tv) ? type : TForall(type.name, type.kind, type.cs, substTVar(tv, sub, type.type));
  return impossible('substTVar');
};
export const substTMeta = (tv: Name, sub: Type, type: Type): Type => {
  if (isTVar(type)) return type;
  if (isTMeta(type)) return eqName(type.name, tv) ? sub : type;
  if (isTFun(type)) return TFun(substTMeta(tv, sub, type.left), substTMeta(tv, sub, type.right));
  if (isTApp(type)) return TApp(substTMeta(tv, sub, type.left), substTMeta(tv, sub, type.right));
  if (isTForall(type)) return TForall(type.name, type.kind, type.cs, substTMeta(tv, sub, type.type));
  return impossible('substTMeta');
};
export const openTForall = (type: TForall, sub: Type): Type =>
  substTVar(type.name, sub, type.type);

export const containsTMeta = (tv: Name, type: Type): boolean => {
  if (isTVar(type)) return false;
  if (isTMeta(type)) return eqName(type.name, tv);
  if (isTFun(type)) return containsTMeta(tv, type.left) || containsTMeta(tv, type.right);
  if (isTApp(type)) return containsTMeta(tv, type.left) || containsTMeta(tv, type.right);
  if (isTForall(type)) return containsTMeta(tv, type.type);
  return impossible('containsTMeta');
};

export const freeTMeta = (type: Type, res: Name[] = []): Name[] => {
  if (isTVar(type)) return res;
  if (isTMeta(type)) {
    const x = type.name;
    for (let i = 0, l = res.length; i < l; i++) {
      if (eqName(res[i], x)) return res;
    }
    res.push(x);
    return res;
  }
  if (isTFun(type)) {
    freeTMeta(type.left, res);
    freeTMeta(type.right, res);
    return res;
  }
  if (isTApp(type)) {
    freeTMeta(type.left, res);
    freeTMeta(type.right, res);
    return res;
  }
  if (isTForall(type)) return freeTMeta(type.type, res);
  return impossible('freeTMeta');
};

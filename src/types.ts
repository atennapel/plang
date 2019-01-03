import { impossible } from './errors';
import { Name, showName, eqName } from './names';

export type Type
  = TVar
  | TMeta
  | TFun
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

export interface TForall {
  readonly tag: 'TForall';
  readonly name: Name;
  readonly type: Type;
}
export const TForall = (name: Name, type: Type): TForall =>
  ({ tag: 'TForall', name, type });
export const isTForall = (type: Type): type is TForall =>
  type.tag === 'TForall';
export const tforall = (ns: Name[], type: Type): Type =>
  ns.reduceRight((t, n) => TForall(n, t), type);

export const showType = (type: Type): string => {
  if (isTVar(type)) return `${showName(type.name)}`;
  if (isTMeta(type)) return `?${showName(type.name)}`;
  if (isTFun(type)) return `(${showType(type.left)} -> ${showType(type.right)})`;
  if (isTForall(type)) return `(forall ${showName(type.name)}. ${showType(type.type)})`;
  return impossible('showType');
};

export const isMono = (type: Type): boolean => {
  if (isTForall(type)) return false;
  if (isTFun(type)) return isMono(type.left) && isMono(type.right);
  return true;
};

export const substTVar = (tv: Name, sub: Type, type: Type): Type => {
  if (isTVar(type)) return eqName(type.name, tv) ? sub : type;
  if (isTMeta(type)) return type;
  if (isTFun(type)) return TFun(substTVar(tv, sub, type.left), substTVar(tv, sub, type.right));
  if (isTForall(type)) return eqName(type.name, tv) ? type : TForall(type.name, substTVar(tv, sub, type.type));
  return impossible('substTVar');
};
export const substTMeta = (tv: Name, sub: Type, type: Type): Type => {
  if (isTVar(type)) return type;
  if (isTMeta(type)) return eqName(type.name, tv) ? sub : type;
  if (isTFun(type)) return TFun(substTMeta(tv, sub, type.left), substTMeta(tv, sub, type.right));
  if (isTForall(type)) return TForall(type.name, substTMeta(tv, sub, type.type));
  return impossible('substTMeta');
};
export const openTForall = (type: TForall, sub: Type): Type =>
  substTVar(type.name, sub, type.type);

export const containsTMeta = (tv: Name, type: Type): boolean => {
  if (isTVar(type)) return false;
  if (isTMeta(type)) return eqName(type.name, tv);
  if (isTFun(type)) return containsTMeta(tv, type.left) || containsTMeta(tv, type.right);
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
  if (isTForall(type)) return freeTMeta(type.type, res);
  return impossible('freeTMeta');
};

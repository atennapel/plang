import { NameT, showName, Name, eqName } from './names';
import { Kind, showKind } from './kinds';

export type Type
  = TVar
  | TMeta
  | TApp
  | TForall;

export type TypeTag = Type['tag'];

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
  readonly body: Type;
}
export const TForall = (name: NameT, body: Type): TForall =>
  ({ tag: 'TForall', name, kind: null, body });
export const TForallK = (name: NameT, kind: Kind | null, body: Type): TForall =>
  ({ tag: 'TForall', name, kind, body });
export const isTForall = (type: Type): type is TForall => type.tag === 'TForall';
export const tforall = (ns: NameT[], body: Type): Type =>
  ns.reduceRight((t, n) => TForall(n, t), body);
export const tforallK = (ns: [NameT, Kind | null][], body: Type): Type =>
  ns.reduceRight((t, [n, k]) => TForallK(n, k, t), body);

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
export const flattenTForall = (type: Type): { args: [NameT, Kind | null][], body: Type } => {
  let c = type;
  const args: [NameT, Kind | null][] = [];
  while (isTForall(c)) {
    args.push([c.name, c.kind || null]);
    c = c.body;
  }
  return { args, body: c };
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
        .map(([n, k]) => k ? `(${showName(n)} : ${showKind(k)})` : showName(n))
        .join(' ');
      return `forall ${args}. ${showType(f.body)}`;
    }
  }
};


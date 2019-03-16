import { Kind, showKind } from './kinds';

export type Type<N>
  = TVar<N>
  | TMeta<N>
  | TApp<N>
  | TForall<N>;

export type TypeTag = Type<any>['tag'];

export interface TVar<N> {
  readonly tag: 'TVar';
  readonly name: N;
}
export const TVar = <N>(name: N): TVar<N> => ({ tag: 'TVar', name });
export const isTVar = <N>(type: Type<N>): type is TVar<N> => type.tag === 'TVar';

export interface TMeta<N> {
  readonly tag: 'TMeta';
  readonly name: N;
}
export const TMeta = <N>(name: N): TMeta<N> => ({ tag: 'TMeta', name });
export const isTMeta = <N>(type: Type<N>): type is TMeta<N> => type.tag === 'TMeta';

export interface TApp<N> {
  readonly tag: 'TApp';
  readonly left: Type<N>
  readonly right: Type<N>;
}
export const TApp = <N>(left: Type<N>, right: Type<N>): TApp<N> => ({ tag: 'TApp', left, right });
export const isTApp = <N>(type: Type<N>): type is TApp<N> => type.tag === 'TApp';
export const tappFrom = <N>(ts: Type<N>[]): Type<N> => ts.reduce(TApp);
export const tapp = <N>(...ts: Type<N>[]): Type<N> => tappFrom(ts);

export interface TForall<N> {
  readonly tag: 'TForall';
  readonly name: N
  readonly kind: Kind<N> | null;
  readonly body: Type<N>;
}
export const TForall = <N>(name: N, body: Type<N>): TForall<N> =>
  ({ tag: 'TForall', name, kind: null, body });
export const TForallK = <N>(name: N, kind: Kind<N> | null, body: Type<N>): TForall<N> =>
  ({ tag: 'TForall', name, kind, body });
export const isTForall = <N>(type: Type<N>): type is TForall<N> => type.tag === 'TForall';
export const tforall = <N>(ns: N[], body: Type<N>): Type<N> =>
  ns.reduceRight((t, n) => TForall(n, t), body);
export const tforallK = <N>(ns: [N, Kind<N> | null][], body: Type<N>): Type<N> =>
  ns.reduceRight((t, [n, k]) => TForallK(n, k, t), body);

export const flattenTApp = <N>(type: Type<N>): Type<N>[] => {
  let c = type;
  const r: Type<N>[] = [];
  while (isTApp(c)) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};
export const flattenTForall = <N>(type: Type<N>): { args: [N, Kind<N> | null][], body: Type<N> } => {
  let c = type;
  const args: [N, Kind<N> | null][] = [];
  while (isTForall(c)) {
    args.push([c.name, c.kind || null]);
    c = c.body;
  }
  return { args, body: c };
};
export const showType = <N>(type: Type<N>, showName: (name: N) => string = n => `${n}`): string => {
  switch (type.tag) {
    case 'TVar': return showName(type.name);
    case 'TMeta': return `?${showName(type.name)}`;
    case 'TApp':
      return flattenTApp(type)
        .map(t => {
          const s = showType(t, showName);
          return isTApp(t) || isTForall(t) ? `(${s})` : s;
        })
        .join(' ');
    case 'TForall': {
      const f = flattenTForall(type);
      const args = f.args
        .map(([n, k]) => k ? `(${showName(n)} : ${showKind(k, showName)})` : showName(n))
        .join(' ');
      return `forall ${args}. ${showType(f.body, showName)}`;
    }
  }
};


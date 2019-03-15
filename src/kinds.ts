export type Kind<N>
  = KVar<N>
  | KMeta<N>
  | KFun<N>;

export type KindTag = Kind<any>['tag'];

export interface KVar<N> {
  readonly tag: 'KVar';
  readonly name: N;
}
export const KVar = <N>(name: N): KVar<N> => ({ tag: 'KVar', name });
export const isKVar = <N>(kind: Kind<N>): kind is KVar<N> => kind.tag === 'KVar';

export interface KMeta<N> {
  readonly tag: 'KMeta';
  readonly name: N;
}
export const KMeta = <N>(name: N): KMeta<N> => ({ tag: 'KMeta', name });
export const isKMeta = <N>(kind: Kind<N>): kind is KMeta<N> => kind.tag === 'KMeta';

export interface KFun<N> {
  readonly tag: 'KFun';
  readonly left: Kind<N>;
  readonly right: Kind<N>;
}
export const KFun = <N>(left: Kind<N>, right: Kind<N>): KFun<N> => ({ tag: 'KFun', left, right });
export const isKFun = <N>(kind: Kind<N>): kind is KFun<N> => kind.tag === 'KFun';
export const kfunFrom = <N>(ks: Kind<N>[]): Kind<N> => ks.reduceRight((x, y) => KFun(y, x));
export const kfun = <N>(...ks: Kind<N>[]): Kind<N> => kfunFrom(ks);

export const flattenKFun = <N>(kind: Kind<N>): Kind<N>[] => {
  let c = kind;
  const r: Kind<N>[] = [];
  while (isKFun(c)) {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
};
export const showKind = <N>(kind: Kind<N>, showName: (name: N) => string = n => `${n}`): string => {
   switch (kind.tag) {
     case 'KVar': return showName(kind.name);
     case 'KMeta': return `?${showName(kind.name)}`;
     case 'KFun':
      return flattenKFun(kind)
        .map(k => {
          const s = showKind(k, showName);
          return isKFun(k) ? `(${s})` : s;
        })
        .join(' -> ');
   }
};

type KindTag = 'KCon' | 'KArr';

export interface Kind { tag : KindTag };

interface KCon extends Kind { name : string };
export const isKCon = (k: Kind): k is KCon => k.tag === 'KCon';
export const kcon = (name: string): KCon => ({
  tag: 'KCon',
  name,
});

interface KArr extends Kind { left : Kind, right : Kind };
export const isKArr = (k: Kind): k is KArr => k.tag === 'KArr';
export const karr = (left: Kind, right: Kind): KArr => ({
  tag: 'KArr',
  left,
  right,
});
export const karr_ = (a: Kind, b: Kind, ...rest: Kind[]): KArr => {
  const l = rest.length;
  if(l === 0) return karr(a, b);
  if(l === 1) return karr(a, karr(b, rest[0]));
  let c = rest[l - 1];
  for(let i = l - 2; i >= 0; i--) c = karr(rest[i], c);
  return karr(a, karr(b, c));
};

export const ktype = kcon('Type');
export const krow = kcon('Row');

export const kindEq = (a: Kind, b: Kind): boolean => {
  if(isKCon(a) && isKCon(b)) return a.name === b.name;
  if(isKArr(a) && isKArr(b))
    return kindEq(a.left, b.left) && kindEq(a.right, b.right);
  throw new Error('impossible');
};

export const kindStr = (k: Kind): string => {
  if(isKCon(k)) return k.name;
  if(isKArr(k)) return `(${kindStr(k.left)} -> ${kindStr(k.right)})`;
  throw new Error('impossible');
};

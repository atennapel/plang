export const impossible = () => { throw new Error('impossible') };

export const assocGet = <A extends { equals: (other: A) => boolean }, B>(arr: [A, B][], val: A): B | null => {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i][0].equals(val)) return arr[i][1];
  }
  return null;
};

export const containsDuplicate = <A extends { equals: (other: A) => boolean }>(arr: A[]): boolean => {
  const acc: A[] = [];
  for (let i = 0; i < arr.length; i++) {
    const c = arr[i];
    for (let j = 0; j < acc.length; j++) {
      if (acc[j].equals(c)) return true;
    }
    acc.push(c);
  }
  return false;
};

export const any = <T>(arr: T[], fn: (val: T) => boolean): boolean => {
  for (let i = 0; i < arr.length; i++) {
    const c = arr[i];
    if (fn(c)) return true;
  }
  return false;
};
export const all = <T>(arr: T[], fn: (val: T) => boolean): boolean => {
  for (let i = 0; i < arr.length; i++) {
    const c = arr[i];
    if (!fn(c)) return false;
  }
  return true;
};

export const remove = <T>(arr: T[], fn: (val: T) => boolean): T[] => {
  const ret: T[] = [];
  for (let i = 0; i < arr.length; i++) {
    const c = arr[i];
    if (!fn(c)) ret.push(c);
  }
  return ret;
};

export const objMap = <T, R>(map: {[key: string]: T}, fn: (val: T, key: string) => R): {[key: string]: R} => {
  const r: {[key: string]: R} = {};
  for (let k in map) r[k] = fn(map[k], k);
  return r;
};
export const objMapToArr = <T, R>(map: {[key: string]: T}, fn: (val: T, key: string) => R): R[] => {
  const r: R[] = [];
  for (let k in map) r.push(fn(map[k], k));
  return r;
};
export const objClone = <T>(map: {[key:string]: T}): {[key:string]: T} => {
  const n: {[key:string]: T} = {};
  for(let k in map) n[k] = map[k];
  return n;
};

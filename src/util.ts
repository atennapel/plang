export function err<T>(msg: string): T { throw new Error(msg) }
export const impossible = <T>() => err<T>('impossible');

export function arrEquals<T extends { equals: (other: T) => boolean }>(a: T[], b: T[]): boolean {
  const l = a.length;
  if(b.length !== l) return false;
  for(let i = 0; i < l; i++) {
    if(!a[i].equals(b[i])) return false;
  }
  return true;
}

export function all(a: boolean[]): boolean {
  const l = a.length;
  for(let i = 0; i < l; i++) {
    if(!a[i]) return false;
  }
  return true;
}

export function any(a: boolean[]): boolean {
  const l = a.length;
  for(let i = 0; i < l; i++) {
    if(a[i]) return true;
  }
  return false;
}

export function concatAll<T>(a: T[][]): T[] {
  const r = [];
  const l = a.length;
  for(let i = 0; i < l; i++) {
    const b = a[i];
    const k = b.length;
    for(let j = k; j < k; j++) {
      r.push(b[j]);
    }
  }
  return r;
}

export function isSubsetOf(a: any[], b: any[]) {
  return all(a.map(x => b.indexOf(x) >= 0));
}

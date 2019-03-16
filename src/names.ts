export type NameT
  = Name
  | Gen;

export interface Name {
  readonly tag: 'Name';
  readonly name: string;
}
export const Name = (name: string): Name =>
  ({ tag: 'Name', name });
export const isName = (term: NameT): term is Name => term.tag === 'Name';

export interface Gen {
  readonly tag: 'Gen';
  readonly name: string;
  readonly id: number;
}
export const Gen = (name: string, id: number): Gen =>
  ({ tag: 'Gen', name, id });
export const isGen = (term: NameT): term is Gen => term.tag === 'Gen';

export const showName = (name: NameT): string => {
  switch (name.tag) {
    case 'Name': return name.name;
    case 'Gen': return `${name.name}\$${name.id}`;
  }
};

export const simplifyName = (name: NameT): Name => {
  switch (name.tag) {
    case 'Name': return name;
    case 'Gen': return Name(`${name.name}${name.id ? name.id - 1 : ''}`);
  }
};

export type NameHash = string;
export const hashName = (name: NameT): NameHash => showName(name);
export type NameMap<T> = Map<NameHash, T>;
export const createNameMap = <T>(): NameMap<T> => new Map<NameHash, T>();
export const insertNameMap = <T>(k: NameT, v: T, m: NameMap<T>): NameMap<T> => {
  m.set(hashName(k), v);
  return m;
};
export const getNameMap = <T>(k: NameT, m: NameMap<T>): T | null => {
  return m.get(hashName(k)) || null;
};

export const eqName = (a: NameT, b: NameT): boolean => {
  if (a === b) return true;
  if (a.tag === 'Name') return b.tag === 'Name' && a.name === b.name;
  if (a.tag === 'Gen') return b.tag === 'Gen' && a.name === b.name && a.id === b.id;
  return false;
};

export const nameContains = (ns: NameT[], n: NameT): boolean => {
  for (let i = 0, l = ns.length; i < l; i++) if (eqName(n, ns[i])) return true;
  return false;
};

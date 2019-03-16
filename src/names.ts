export type NameT
  = Name
  | Gen;

export type NameTag = NameT['tag'];

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

export const eqName = (a: NameT, b: NameT): boolean => {
  if (a === b) return true;
  if (a.tag === 'Name') return b.tag === 'Name' && a.name === b.name;
  if (a.tag === 'Gen') return b.tag === 'Gen' && a.name === b.name && a.id === b.id;
  return false;
};


import { impossible } from "./errors";

export type Id = number;

export type Name
  = Plain
  | Gen;

export interface Plain {
  readonly tag: 'Plain';
  readonly name: string;
}
export const Plain = (name: string): Plain =>
  ({ tag: 'Plain', name });
export const isPlain = (name: Name): name is Plain =>
  name.tag === 'Plain';

export interface Gen {
  readonly tag: 'Gen';
  readonly name: string;
  readonly id: Id;
}
export const Gen = (name: string, id: Id): Gen =>
  ({ tag: 'Gen', name, id });
export const isGen = (name: Name): name is Gen =>
  name.tag === 'Gen';

export const showName = (name: Name): string => {
  if (isPlain(name)) return name.name;
  if (isGen(name)) return `${name.name}\$${name.id}`;
  return impossible('showName');
};

export const eqName = (a: Name, b: Name): boolean => {
  if (a === b) return true;
  if (a.name !== b.name) return false;
  if (isPlain(a)) return isPlain(b);
  if (isGen(a)) return isGen(b) && a.id === b.id;
  return impossible('eqName');
};

export const namePart = (name: Name): string => name.name;

export type NameState = { [key: string]: number };
let nameState: NameState = {};
export const resetName = () => { nameState = {} };
export const freshName = (name: Name, state: NameState = nameState) => {
  const part = namePart(name);
  if (!state[part]) state[part] = 0;
  const id = state[part]++;
  return Gen(part, id);
};

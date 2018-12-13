export type Name = string;

let id = 0;
export const resetId = () => { id = 0 };
export const fresh = (name: Name = '_') => `${name.split('$')[0]}\$${id++}`;
export const namePart = (name: Name) => name.split('$')[0];

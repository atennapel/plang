export type Name = string;

export type TName = number;
export let _id: TName = 0;
export const resetTName = () => { _id = 0 };
export const freshTName = (): TName => _id++;

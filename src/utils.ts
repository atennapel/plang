export type ADT<T> = T[keyof T];
export type WithTag = { [tag: string]: { tag: string } };

export type CaseNoCheck = 'CaseNoCheck';
export type CaseCheck = 'CaseCheck';
export type CaseMode = CaseNoCheck | CaseCheck;

export type Cases<T extends WithTag, R, M extends CaseMode = CaseCheck> =
  M extends CaseNoCheck ? { [tag in ADT<T>['tag']]?: (val: T[tag]) => R } & { otherwise: (val: T) => R } :
  M extends CaseCheck ? { [tag in ADT<T>['tag']]: (val: T[tag]) => R } :
  never;

  export const cases = <T extends WithTag>() =>
  <R, M extends CaseMode = CaseCheck>(val: ADT<T>, cs: Cases<T, R, M>): R =>
    ((cs as any)[val.tag] || (cs as any).otherwise)(val);

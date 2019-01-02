export class ImpossibleError extends Error {

  constructor(public readonly msg: string) {
    super(`impossible: ${msg}`);
  }

}
export const impossible = (msg: string = '') => { throw new ImpossibleError(msg) };

export class InferError extends Error {

  constructor(public readonly msg: string) {
    super(msg);
  }

}
export const err = (msg: string = '') => { throw new InferError(msg) };

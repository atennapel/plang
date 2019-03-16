export class InferError extends TypeError {
  constructor(msg: string) { super(msg) }
}

export const infererr = (msg: string) => {
  throw new InferError(msg);
};

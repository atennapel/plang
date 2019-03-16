import { Context } from './context';
import { CKVar, CTVar } from './elems';
import { nType, kfun, kType } from './kinds';
import { nFun } from './types';
import { NameStore } from './namestore';

const initialContext = () =>
  Context.of(
    CKVar(nType),
    CTVar(nFun, kfun(kType, kType, kType)),
  );

export let context = initialContext();
export const resetContext = () => {
  context = initialContext();
};
const stored: Context[] = [];
export const storeContext = (ctx?: Context): Context => {
  const ctx_ = ctx || context;
  stored.push(ctx_.clone());
  return ctx_;
};
export const restoreContext = (): Context => {
  context = stored.pop() as Context;
  return context;
};

export let namestore = new NameStore();

export const infererr = (msg: string) => {
  throw new TypeError(msg);
};

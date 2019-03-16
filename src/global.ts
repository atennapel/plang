import { Context } from './context';
import { CKVar, CTVar } from './elems';
import { nType, kfun, kType } from './kinds';
import { nFun, Type, TApp, TForallK } from './types';
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
export const storeContext = (ctx?: Context): void => {
  const ctx_ = ctx || context;
  stored.push(ctx_.clone());
};
export const restoreContext = (): void => {
  context = stored.pop() || context;
};
export const discardContext = (): void => {
  stored.pop();
};

export let namestore = new NameStore();

export const apply = (type: Type, ctx_?: Context): Type => {
  const ctx = ctx_ || context;
  switch (type.tag) {
    case 'TVar': return type;
    case 'TMeta': {
      const t = ctx.lookup('CTMeta', type.name);
      return t && t.type ? apply(t.type, ctx): type;
    }
    case 'TApp': {
      const left = apply(type.left, ctx);
      const right = apply(type.right, ctx);
      return type.left === left && type.right === right ? type : TApp(left, right);
    }
    case 'TForall': {
      const body = apply(type.type, ctx);
      return type.type === body ? type : TForallK(type.name, type.kind, body);
    }
  }
};

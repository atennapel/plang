import { Type, showType, openTForall, TVar } from './types';
import { Kind, kType, KFun, KMeta } from './kinds';
import { unifyKinds } from './kindUnification';
import { context, applyKind, namestore } from './global';
import { infererr, InferError } from './error';
import { CKMeta, CTVar } from './elems';
import { wfContext } from './wellformedness';

const synthKind = (type: Type): Kind => {
  switch (type.tag) {
    case 'TVar': {
      const e = context.lookup('CTVar', type.name);
      if (!e) return infererr(`undefined tvar ${showType(type)}`);
      return e.kind;
    }
    case 'TMeta': {
      const e = context.lookup('CTMeta', type.name);
      if (!e) return infererr(`undefined tmeta ${showType(type)}`);
      return e.kind;
    }
    case 'TApp': {
      const l = synthKind(type.left);
      const r = synthKind(type.right);
      const kv = namestore.fresh('k');
      const km = KMeta(kv);
      context.enter(kv, CKMeta(kv));
      unifyKinds(l, KFun(r, km));
      const ki = applyKind(km);
      context.leave(kv);
      return ki;
    }
    case 'TForall': {
      const t = namestore.fresh(type.name);
      if (type.kind) {
        context.enter(t, CTVar(t, type.kind));
      } else {
        const k = namestore.fresh(type.name);
        context.enter(t, CKMeta(k), CTVar(t, KMeta(k)));
      }
      const ki = synthKind(openTForall(type, TVar(t)));
      context.leave(t);
      return applyKind(ki);
    }
  }
};

export const inferKind = (type: Type): Kind => {
  console.log(`inferKind ${showType(type)}`);
  const m = namestore.fresh('m');
  context.enter(m);
  try {
    const ki = synthKind(type);
    context.leave(m);
    return applyKind(ki);
  } catch (err) {
    context.leave(m);
    throw err;
  }
};

export const checkKindType = (type: Type): void =>
  unifyKinds(inferKind(type), kType);

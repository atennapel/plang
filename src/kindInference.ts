import { Type, showType, openTForall, TVar, TApp, isTMeta, TForallK } from './types';
import { Kind, kType, KFun, KMeta, eqKind, showKind, isKMeta } from './kinds';
import { unifyKinds } from './kindUnification';
import { context, applyKind, namestore } from './global';
import { infererr } from './error';
import { CKMeta, CTVar } from './elems';
import { showName } from './names';
import { log } from './config';

const elaborateTypeR = (type: Type): [Kind, Type] => {
  log(`elaborateTypeR ${showType(type)}`);
  switch (type.tag) {
    case 'TVar': {
      const e = context.lookup('CTVar', type.name);
      if (!e) return infererr(`undefined tvar ${showType(type)}`);
      return [applyKind(e.kind), type];
    }
    case 'TMeta': {
      const e = context.lookup('CTMeta', type.name);
      if (!e) return infererr(`undefined tmeta ${showType(type)}`);
      return [applyKind(e.kind), type];
    }
    case 'TApp': {
      const [l, tl] = elaborateTypeR(type.left);
      const [r, tr] = elaborateTypeR(type.right);
      const kv = namestore.fresh('k');
      const km = KMeta(kv);
      context.add(CKMeta(kv));
      unifyKinds(l, KFun(r, km));
      return [applyKind(km), TApp(tl, tr)];
    }
    case 'TForall': {
      let k;
      if (type.kind) {
        k = type.kind;
      } else {
        const kn = namestore.fresh(type.name);
        context.add(CKMeta(kn));
        k = KMeta(kn);
      }
      context.enter(type.name, CTVar(type.name, k));
      const [ki, tt] = elaborateTypeR(openTForall(type, TVar(type.name)));
      context.leave(type.name);
      return [applyKind(ki), TForallK(type.name, applyKind(k), tt)];
    }
  }
};

const instKMetaInKind = (kind: Kind): Kind => {
  switch (kind.tag) {
    case 'KVar': return kind;
    case 'KMeta': return kType;
    case 'KFun':
      return KFun(instKMetaInKind(kind.left), instKMetaInKind(kind.right));
  }
}

const instKMeta = (type: Type): Type => {
  switch (type.tag) {
    case 'TVar': return type;
    case 'TMeta': return type;
    case 'TApp': return TApp(instKMeta(type.left), instKMeta(type.right));
    case 'TForall': {
      const k = type.kind ? instKMetaInKind(type.kind) : kType;
      return TForallK(type.name, k, instKMeta(type.type));
    }
  }
};

export const elaborateType = (type: Type): Type => {
  log(`elaborateType ${showType(type)}`);
  const m = namestore.fresh('m');
  context.enter(m);
  const [_, ty] = elaborateTypeR(type);
  context.leave(m);
  const ti = instKMeta(ty);
  log(`result ${showType(ti)}`);
  return ti;
};

export const deriveKind = (type: Type): Kind => {
  switch (type.tag) {
    case 'TVar': {
      const c = context.lookup('CTVar', type.name);
      if (c) return c.kind;
      return infererr(`undefined type ${showName(type.name)}`);
    }
    case 'TMeta': {
      const c = context.lookup('CTMeta', type.name);
      if (c) return c.kind;
      return infererr(`undefined type ?${showName(type.name)}`);
    }
    case 'TApp': {
      const l = deriveKind(type.left);
      const r = deriveKind(type.right);
      if (l.tag !== 'KFun') return infererr(`not a kfun in ${showType(type)}`);
      if (!eqKind(l.left, r)) return infererr(`kind mismatch in ${showType(type)}`);
      return l.right;
    }
    case 'TForall': {
      if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
      const t = namestore.fresh(type.name);
      context.enter(t, CTVar(t, type.kind));
      const k = deriveKind(openTForall(type, TVar(t)));
      context.leave(t);
      return k;
    }
  }
};

export const checkKindType = (type: Type): void => {
  const k = deriveKind(type);
  if (!eqKind(k, kType))
    return infererr(`expected ${showKind(kType)} but got ${showKind(k)} in ${showType(type)}`);
};

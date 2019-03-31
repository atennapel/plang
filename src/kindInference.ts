import {
  TApp,
  TForall,
  substTVar,
  TVar,
  showTy,
  Type,
  TVMap,
} from './types';
import {
  Kind,
  showKind,
  occursKMeta,
  KMeta,
  KFun,
  kType,
} from './kinds';
import {
  terr,
  freshKMeta,
  freshTSkol,
  impossible
} from './util';
import { Env, lookupTCon } from './env';

const bindKMeta = (x: KMeta, k: Kind): void => {
  if (x.kind) return unifyKind(x.kind, k);
  if (k.tag === 'KMeta' && k.kind) return unifyKind(x, k.kind);
  if (occursKMeta(x, k))
    return terr(`${showKind(x)} occurs in ${showKind(k)}`);
  x.kind = k;
};
const unifyKind = (a: Kind, b: Kind): void => {
  if (a === b) return;
  if (a.tag === 'KMeta') return bindKMeta(a, b);
  if (b.tag === 'KMeta') return bindKMeta(b, a);
  if (a.tag === 'KFun' && b.tag === 'KFun') {
    unifyKind(a.left, b.left);
    return unifyKind(a.right, b.right);
  }
  if (a.tag === 'KCon' && b.tag === 'KCon' && a.name === b.name)
    return;
  return terr(`failed to unify kinds: ${showKind(a)} ~ ${showKind(b)}`);
};

const inferKindR = (env: Env, t: Type): [Kind, Type] => {
  if (t.tag === 'TMeta') return [t.kind, t];
  if (t.tag === 'TVar')
    return terr(`tvar ${showTy(t)} in inferKindR`);
  if (t.tag === 'TSkol') return [t.kind, t];
  if (t.tag === 'TCon') {
    const k = lookupTCon(env, t.name);
    if (!k) return terr(`undefined type constructor ${showTy(t)}`);
    return [k, t];
  }
  if (t.tag === 'TApp') {
    const [l, tl] = inferKindR(env, t.left);
    const [r, tr] = inferKindR(env, t.right);
    const km = freshKMeta();
    unifyKind(l, KFun(r, km));
    return [km, TApp(tl, tr)];
  }
  if (t.tag === 'TForall') {
    const { names, type } = t;
    const kinds = t.kinds || [];
    const m: TVMap = {};
    const nks = Array(names.length);
    for (let i = 0, l = names.length; i < l; i++) {
      const ki = kinds[i] || freshKMeta();
      const k = freshTSkol(names[i], ki);
      m[names[i]] = k;
      nks[i] = ki;
    }
    const [km, b] = inferKindR(env, substTVar(m, type));
    return [km, TForall(names, nks, b)];
  }
  return impossible('inferKindR');
};

const defaultKindInKind = (k: Kind): Kind => {
  if (k.tag === 'KCon') return k;
  if (k.tag === 'KMeta') {
    if (k.kind) return defaultKindInKind(k.kind);
    return kType;
  }
  if (k.tag === 'KFun') {
    return KFun(
      defaultKindInKind(k.left),
      defaultKindInKind(k.right)
    );
  }
  return impossible('defaultKindInKind');
};

const defaultKind = (t: Type): Type => {
  if (t.tag === 'TApp')
    return TApp(defaultKind(t.left), defaultKind(t.right));
  if (t.tag === 'TForall') {
    const nks = t.kinds.map(k => k ? defaultKindInKind(k) : kType);
    return TForall(t.names, nks, defaultKind(t.type));
  }
  if (t.tag === 'TSkol')
    return TVar(t.name);
  if (t.tag === 'TMeta')
    return terr(`tmeta ${showTy(t)} in defaultKind`);
  return t;
};

export const inferKind = (env: Env, ty: Type): Type => {
  const [_, ti] = inferKindR(env, ty);
  return defaultKind(ti);
};

export const kindOf = (env: Env, t: Type): Kind => {
  if (t.tag === 'TMeta') return t.kind;
  if (t.tag === 'TSkol') return t.kind;
  if (t.tag === 'TCon')
    return lookupTCon(env, t.name) ||
      terr(`undefined type constructor ${showTy(t)}`);
  if (t.tag === 'TApp') {
    const f = kindOf(env, t.left);
    if (f.tag !== 'KFun')
      return terr(`not a kind fun in left side of type application (${showTy(t)}): ${showKind(f)}`);
    return f.right;
  }
  return terr(`unexpected type ${showTy(t)} in kindOf`);
};

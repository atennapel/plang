const {
  resetId,
  TVar,
  TApp,
  TFun,
  freshTMeta,
  prune,
} = require('./types');
const { unify } = require('./unification');

const extend = (env, x, t) => {
  const n = Object.create(env);
  n[x] = t;
  return n;
};
const inst = (t, map = {}) => {
  if (t.tag === 'TVar')
    return map[t.id] || (map[t.id] = freshTMeta());
  if (t.tag === 'TApp')
    return TApp(inst(t.left, map), inst(t.right, map));
  return t;
};
const gen = t => {
  if (t.tag === 'TMeta') return TVar(t.id);
  if (t.tag === 'TApp')
    return TApp(gen(t.left), gen(t.right));
  return t;
};
const synth = (env, e, skol = {}) => {
  switch (e.tag) {
    case 'Var': {
      if (!env[e.name]) throw new TypeError(`undefined variable ${e.name}`);
      return inst(prune(env[e.name]));
    }
    case 'Abs': {
      const tv = freshTMeta();
      const t = synth(extend(env, e.name, tv), e.body, skol);
      return TFun(prune(tv), t);
    }
    case 'App': {
      const ta = synth(env, e.left, skol);
      const tb = synth(env, e.right, skol);
      const tv = freshTMeta();
      unify(ta, TFun(tb, tv), skol);
      return prune(tv);
    }
  }
};

const simplify = (t, map = {}, next = { id: 0 }) => {
  if (t.tag === 'TVar')
    return map[t.id] || (map[t.id] = TVar(next.id++));
  if (t.tag === 'TApp')
    return TApp(simplify(t.left, map, next), simplify(t.right, map, next));
  return t;
};
const infer = (env, e) => {
  resetId();
  return simplify(gen(synth(env, e)));
};
const inferDefs = (ds, env = {}) => {
  for (let i = 0, l = ds.length; i < l; i++) {
    const [x, e] = ds[i];
    const t = infer(env, e);
    env[x] = t;
  }
  return env;
};

module.exports = {
  infer,
  inferDefs,
};

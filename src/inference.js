const {
  TVar,
  TApp,
  resetTMeta,
  freshTMeta,
  TFun,
  pruneType,
  tmetas,
} = require('./types');
const { kType } = require('./kinds');
const { unify } = require('./unification');
const { checkKindType } = require('./kindInference');

const err = msg => { throw new TypeError(msg) };

const tmetasInEnv = (env, map = {}) => {
  for (let k in env) tmetas(env[k], map);
  return map;
};

const inst = (t, map = {}) => {
  if (t.tag === 'TCon') return t;
  if (t.tag === 'TVar') {
    if (map[t.id]) return map[t.id];
    const tv = freshTMeta(t.kind);
    map[t.id] = tv;
    return tv;
  }
  if (t.tag === 'TMeta') return t;
  if (t.tag === 'TApp') {
    const a = inst(t.left, map);
    const b = inst(t.right, map);
    return a !== t.left || b !== t.right ? TApp(a, b) : t;
  }
};
const gen = (t, tvs = {}, map = {}) => {
  if (t.tag === 'TCon') return t;
  if (t.tag === 'TVar') return t;
  if (t.tag === 'TMeta') {
    if (tvs[t.id]) return t;
    if (map[t.id]) return map[t.id];
    const tv = TVar(t.id, t.kind);
    map[t.id] = tv;
    return tv;
  }
  if (t.tag === 'TApp') {
    const a = gen(t.left, tvs, map);
    const b = gen(t.right, tvs, map);
    return a !== t.left || b !== t.right ? TApp(a, b) : t;
  }
};

const synth = (env, e) => {
  if (e.tag === 'Var') {
    if (!env[e.name]) return err(`undefined variable: ${e.name}`);
    return inst(env[e.name]);
  }
  if (e.tag === 'Abs') {
    const old = env[e.name];
    const tv = freshTMeta(kType);
    env[e.name] = tv;
    const tr = synth(env, e.body);
    if (old) env[e.name] = old;
    else delete env[e.name];
    return TFun(pruneType(tv), tr);
  }
  if (e.tag === 'App') {
    const a = synth(env, e.left);
    const b = synth(env, e.right);
    const r = freshTMeta(kType);
    unify(a, TFun(b, r));
    return pruneType(r);
  }
  if (e.tag === 'Let') {
    const ty = synth(env, e.val);
    const old = env[e.name];
    env[e.name] = gen(ty, tmetasInEnv(env));
    const tr = synth(env, e.body);
    if (old) env[e.name] = old;
    else delete env[e.name];
    return tr;
  }
  return err('unimplemented');
};

const infer = (env, e) => {
  resetTMeta();
  const ty = synth(env, e);
  checkKindType(ty);
  return gen(ty);
};

module.exports = {
  infer,
};

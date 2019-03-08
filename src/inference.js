const { freshTMeta, TFun, pruneType, resetTMeta } = require('./types');
const { kType } = require('./kinds');
const { unify } = require('./unification');
const { checkKindType } = require('./kindInference');

const err = msg => { throw new TypeError(msg) };

const synth = (env, e) => {
  if (e.tag === 'Var') {
    if (!env[e.name]) return err(`undefined variable: ${e.name}`);
    return env[e.name];
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
  return err('unimplemented');
};

const infer = (env, e) => {
  resetTMeta();
  const ty = synth(env, e);
  checkKindType(ty);
  return ty;
};

module.exports = {
  infer,
};

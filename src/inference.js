const {
  resetId,
  TCon,
  TVar,
  TApp,
  TFun,
  tapp,
  freshTMeta,
  prune,
  occursAny,
  showType,
  freeTVars,
} = require('./types');
const { showExpr } = require('./exprs');
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
const escapeCheckType = (tvs, ty, msg) => {
  const tv = occursAny(tvs, prune(ty));
  if (tv) throw new TypeError(msg(tv));
};
const escapeCheckEnv = (tvs, env, expr) => {
  for (let x in env) {
    const ty = prune(env[x]);
    escapeCheckType(tvs, ty,
      tv => `skolem ${showType(tv)} escaped in ${x} : ${showType(ty)} in ${showExpr(expr)}`);
  }
};
const synth = (tenv, env, e, skol = {}) => {
  // console.log(`synth ${e.tag} ${showExpr(e)}`);
  switch (e.tag) {
    case 'Var': {
      if (!env[e.name]) throw new TypeError(`undefined variable ${e.name}`);
      return inst(prune(env[e.name]));
    }
    case 'Abs': {
      const tv = freshTMeta();
      const t = synth(tenv, extend(env, e.name, tv), e.body, skol);
      return TFun(prune(tv), t);
    }
    case 'App': {
      const ta = synth(tenv, env, e.left, skol);
      const tb = synth(tenv, env, e.right, skol);
      const tv = freshTMeta();
      unify(ta, TFun(tb, tv), skol);
      return prune(tv);
    }
    case 'Con': {
      const data = tenv[e.con];
      if (!data) throw new TypeError(`undefined constructor: ${e.con}`);
      const tms = data.tvs.map(() => freshTMeta());
      const etms = data.etvs.map(() => freshTMeta());
      const utms = data.utvs.map(() => freshTMeta());
      const map = {};
      const nskol = Object.create(skol);
      for (let i = 0, l = data.tvs.length; i < l; i++) map[data.tvs[i]] = tms[i];
      for (let i = 0, l = data.etvs.length; i < l; i++) map[data.etvs[i]] = etms[i];
      for (let i = 0, l = data.utvs.length; i < l; i++) {
        map[data.utvs[i]] = utms[i];
        nskol[utms[i].id] = true;
      }
      const ty = synth(tenv, env, e.arg, skol);
      unify(inst(data.type, map), ty, nskol);
      escapeCheckEnv(utms, env, e);
      return tapp(data.tcon, tms.map(prune));
    }
    case 'Decon': {
      const data = tenv[e.con];
      if (!data) throw new TypeError(`undefined constructor: ${e.con}`);
      const tms = data.tvs.map(() => freshTMeta());
      const etms = data.etvs.map(() => freshTMeta());
      const utms = data.utvs.map(() => freshTMeta());
      const map = {};
      const nskol = Object.create(skol);
      for (let i = 0, l = data.tvs.length; i < l; i++) map[data.tvs[i]] = tms[i];
      for (let i = 0, l = data.etvs.length; i < l; i++) {
        map[data.etvs[i]] = etms[i];
        nskol[etms[i].id] = true;
      }
      for (let i = 0, l = data.utvs.length; i < l; i++) map[data.utvs[i]] = utms[i];
      const tr = synth(tenv, extend(env, e.name, inst(data.type, map)), e.body, nskol);
      escapeCheckEnv(etms, env, e);
      escapeCheckType(etms, tr, tv => `skolem ${showType(tv)} escaped in ${showExpr(e)}`);
      utms.forEach(t =>
        escapeCheckType(etms, t, tv => `skolem ${showType(tv)} escaped in ${showExpr(e)}`));
      return TFun(tapp(data.tcon, tms.map(prune)), tr);
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
const infer = (tenv, env, e) => {
  resetId();
  return simplify(gen(synth(tenv, env, e)));
};
const inferDefs = (ds, tenv = {}, env = {}) => {
  for (let i = 0, l = ds.length; i < l; i++) {
    const d = ds[i];
    switch (d.tag) {
      case 'DType':
        tenv[d.name] = {
          tcon: TCon(d.name),
          tvs: [],
          etvs: [],
          utvs: freeTVars(d.type),
          type: d.type,
        };
        break;
      case 'DValue':
        env[d.name] = infer(tenv, env, d.expr);
        break;
    }
  }
  return env;
};

module.exports = {
  infer,
  inferDefs,
};

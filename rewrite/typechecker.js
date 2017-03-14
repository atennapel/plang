var E = require('./exprs');
var T = require('./types');
var U = require('./utils');

var fail = m => new TypeError(m);
var failed = x => x instanceof TypeError;

var free = t => {
  if(t.tag === T.TVar) return U.set(t.id);
  if(t.tag === T.Con) return {};
  if(t.tag === T.TApp) return U.union(t.left, t.right);
  if(t.tag === T.Scheme) return U.without(free(t.type), U.setFrom(t.vars));
  T.terr('Invalid type tag in free: ' + t.tag);
};

var freeEnv = env => U.vals(env).map(free);

var subst = (sub, t) => {
  if(t.tag === T.TVar) return sub[t.id] || t;
  if(t.tag === T.Con) return t;
  if(t.tag === T.TApp) return T.tapp(subst(sub, t.left), subst(sub, t.right));
  if(t.tag === T.Scheme)
    return T.scheme(t.vars, subst(U.without(sub, U.setFrom(t.vars)), t.type));
  T.terr('Invalid type tag in subst: ' + t.tag);
};

var substEnv = (sub, env) => U.omap(t => subst(sub, t), env);

var compose = (s1, s2) => U.union(U.omap(t => subst(s1, t), s2), s1);

var generalize = (env, t) =>
  T.scheme(U.keys(U.without(free(t), freeEnv(env))), t);

var fresh = state =>
  ({
    tvar: T.tvar(state.tvar),
    state: U.clone(state, 'tvar', state.tvar + 1),
  });
var nfresh = (n, state) =>
  ({
    tvars: U.range(1, state.tvar, state.tvar+n-1).map(T.tvar),
    state: U.clone(state, 'tvar', state.tvar + n),
  });

var instantiate = (state, s) => {
  var r = nfresh(s.vars.length, state);
  var nvars = r.tvars;
  var nstate = r.state;
  var sub = U.mapFrom2(s.vars, nvars);
  return {
    type: subst(sub, s.type),
    state: nstate,
  };
};

var occurs = (v, t) => !!free(t)[v.id];

var bind = (v, t) => {
  if(t.tag === T.TVar && v.id === t.id) return {};
  if(occurs(v, t)) return fail(
    'Occurs check failed: ' + T.toString(v) + ' and ' + T.toString(t)
  );
  return U.map(v.id, t);
};

var unify = (a, b) => {
  if(a.tag === T.TVar) return bind(a, b);
  if(b.tag === T.TVar) return bind(b, a);
  if(a.tag === T.Con && b.tag === T.Con && a.name === b.name) return {};
  if(a.tag === T.TApp && b.tag === T.TApp) {
    var s1 = unify(a.left, b.left);
    if(failed(s1)) return s1;
    var s2 = unify(subst(s1, a.right), subst(s1, b.right));
    if(failed(s2)) return s2;
    return compose(s2, s1);
  }
  return fail('Cannot unify ' + T.toString(a) + ' and ' + T.toString(b));
};

var infer = (env, state, e) => {
  // console.log('infer: ' + E.toString(e));
  if(e.tag === E.Var) {
    if(!env[e.name]) T.terr('undefined variable: ' + e.name);
    var r = instantiate(state, env[e.name]);
    return {
      sub: {},
      type: r.type,
      state: r.state,
    };
  }
  if(e.tag === E.Lam) {
    var rv = fresh(state);
    var nenv = U.clone(env, e.arg, T.scheme([], rv.tvar));
    var ri = infer(nenv, rv.state, e.body);
    return {
      sub: ri.sub,
      type: T.tarr(subst(ri.sub, rv.tvar), ri.type),
      state: ri.state,
    };
  }
  if(e.tag === E.App) {
    var rleft = infer(env, state, e.left);
    var rright = infer(substEnv(rleft.sub, env), rleft.state, e.right);
    var rv = fresh(rright.state);
    var su = unify(subst(rright.sub, rleft.type), T.tarr(rright.type, rv.tvar));
    if(failed(su)) throw su;
    return {
      sub: compose(su, compose(rright.sub, rleft.sub)),
      type: subst(su, rv.tvar),
      state: rv.state,
    };
  }
  if(e.tag === E.Let) {
    var rval = infer(env, state, e.val);
    var nenv =
      U.clone(env, e.arg, generalize(substEnv(rval.sub, env), rval.type));
    var rbody = infer(nenv, rval.state, e.body);
    return {
      sub: compose(rbody.sub, rval.sub),
      type: rbody.type,
      state: rbody.state,
    };
  }
  T.terr('Cannot infer: ' + E.toString(e));
};

var initialState = { tvar: 0 };

var runInfer = (env, e) => {
  var r = infer(env, initialState, e);
  return subst(r.sub, r.type);
};

// test
var Bool = T.con('Bool');
var Int = T.con('Int');
var env = {
  one: T.scheme([], Int),
  True: T.scheme([], Bool),
  inc: T.scheme([], T.tarr(Int, Int)),
};
var e = E.lt('id', E.lam('x', E.vr('x')), E.vr('id'));
console.log(E.toString(e));
var t = runInfer(env, e);
console.log(T.toString(t));

module.exports = {
  infer: runInfer,
};

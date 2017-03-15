/**
 * named tvars
 * effects:
 *  value restriction
 *  handlers
 * implicits
 */
var E = require('./exprs');
var T = require('./types');
var K = require('./kinds');
var U = require('./utils');

var fail = m => new TypeError(m);
var failed = x => x instanceof TypeError;

var free = t => {
  if(t.tag === T.TVar) return U.map(t.id, t);
  if(t.tag === T.TCon) return {};
  if(t.tag === T.TApp) return U.union(free(t.left), free(t.right));
  if(t.tag === T.TRowEmpty) return {};
  if(t.tag === T.TRowExtend) return U.union(free(t.type), free(t.rest));
  if(t.tag === T.TScheme)
    return U.without(free(t.type), U.setFrom(t.vars.map(x => x.id)));
  T.terr('Invalid type tag in free: ' + t.tag);
};

var freeEnv = env =>
  U.vals(env).map(free).reduce((o, v) => (o[v.id] = v, o), {});

var subst = (sub, t) => {
  if(t.tag === T.TVar) return sub[t.id] || t;
  if(t.tag === T.TCon) return t;
  if(t.tag === T.TApp) return T.tapp(subst(sub, t.left), subst(sub, t.right));
  if(t.tag === T.TRowEmpty) return t;
  if(t.tag === T.TRowExtend)
    return T.trowextend(t.label, subst(sub, t.type), subst(sub, t.rest));
  if(t.tag === T.TScheme)
    return T.tscheme(t.vars, subst(U.without(sub, U.setFrom(t.vars)), t.type));
  T.terr('Invalid type tag in subst: ' + t.tag);
};

var substEnv = (sub, env) => U.omap(t => subst(sub, t), env);

var compose = (s1, s2) => U.union(U.omap(t => subst(s1, t), s2), s1);

var generalize = (env, t) =>
  T.tscheme(U.vals(U.without(free(t), freeEnv(env))), t);

var fresh = (state, kind, labels) =>
  ({
    tvar: T.tvar(state.tvar, kind, labels),
    state: U.clone(state, 'tvar', state.tvar + 1),
  });

var instantiate = (state, s) => {
  var r = s.vars.reduce((r, v) => {
    var rf = fresh(r.state, v.kind, v.labels);
    r.vars.push(rf.tvar);
    return {
      vars: r.vars,
      state: rf.state,
    };
  }, {vars: [], state});
  var sub = U.mapFrom2(s.vars.map(x => x.id), r.vars);
  return {
    type: subst(sub, s.type),
    state: r.state,
    sub,
  };
};

var unifyKind = (a, b) =>
  K.equals(a, b) ||
    fail('Cannot unify kinds ' + K.toString(a) + ' and ' + K.toString(b));

var occurs = (v, t) => !!free(t)[v.id];

var bind = (state, v, t) => {
  if(t.tag === T.TVar) {
    if(v.id === t.id) return { sub: {}, state };
    var m = fresh(state, v.kind, U.union(v.labels, t.labels));
    return {
      sub: U.map(v.id, m.tvar, t.id, m.tvar),
      state: m.state,
    };
  }
  if(v.kind === K.Row) return bindRow(state, v, t);
  if(occurs(v, t))
    return fail('Occurs check failed: ' + T.toString(v) +
      ' and ' + T.toString(t));
  return { sub: U.map(v.id, t), state };
};

var rowToList = t => {
  if(t.tag === T.TVar) return { row: {}, rest: t };
  if(t.tag === T.TRowEmpty) return { row: {}, rest: null };
  if(t.tag === T.TRowExtend) {
    var r = rowToList(t.rest);
    return { row: U.clone(r.row, t.label, t.type), rest: r.rest };
  }
  T.terr('rowToList failed on ' + T.toString(t));
};

var bindRow = (state, v, t) => {
  var ls1 = v.labels;
  var row = rowToList(t);
  var ls2 = U.set(U.keys(row.row));
  var s1 = U.map(v.id, t);
  if(U.overlaps(ls1, ls2))
    return fail('Repeated labels in ' + T.toString(v) +
      ' and ' + T.toString(t));
  if(!row.rest) return { sub: s1, state };
  var rv = fresh(state, K.Row, U.union(ls1, row.rest.labels));
  var s2 = U.map(row.rest.id, rv.tvar);
  return {
    sub: compose(s2, s1),
    state: rv.state,
  };
};

var rewriteRow = (state, t, label) => {
  if(t.tag === T.TRowEmpty) return fail(label + ' cannot be inserted');
  if(t.tag === T.TRowExtend) {
    if(label === t.label) return {type: t.type, rest: t.rest, sub: {}, state};
    if(t.rest.tag === T.TVar) {
      var rv1 = fresh(state, K.Row, U.set(label));
      var rv2 = fresh(rv1.state, K.Star);
      var r = bindRow(
        rv2.state,
        t.rest,
        T.trowextend(label, rv2.tvar, rv1.tvar)
      );
      return {
        type: rv2.tvar,
        rest: subst(r.sub, T.trowextend(t.label, t.type, rv1.tvar)),
        sub: r.sub,
        state: r.state,
      };
    }
    var r = rewriteRow(state, t.rest, label);
    if(failed(r)) return r;
    return {
      type: r.type,
      rest: T.trowextend(t.label, t.type, r.rest),
      sub: r.sub,
      state: r.state,
    };
  }
  T.terr('rewriteRow failed on ' + T.toString(t));
};

var unify = (state, a, b) => {
  // console.log('unify ' + T.toString(a) + ' and ' + T.toString(b));
  var sk = unifyKind(a.kind, b.kind);
  if(failed(sk))
    return fail('Cannot unify ' + T.toString(a) + ' and ' +
      T.toString(b) + ': ' + sk);
  if(a.tag === T.TVar) return bind(state, a, b);
  if(b.tag === T.TVar) return bind(state, b, a);
  if(a.tag === T.TCon && b.tag === T.TCon && a.name === b.name)
    return { sub: {}, state };
  if(a.tag === T.TApp && b.tag === T.TApp) {
    var s1 = unify(state, a.left, b.left);
    if(failed(s1)) return s1;
    var s2 = unify(s1.state, subst(s1.sub, a.right), subst(s1.sub, b.right));
    if(failed(s2)) return s2;
    return { sub: compose(s2.sub, s1.sub), state: s2.state };
  }
  if(a.tag === T.TRowEmpty && b.tag === T.TRowEmpty)
    return { sub: {}, state };
  if(a.tag === T.TRowExtend && b.tag === T.TRowExtend) {
    var r = rewriteRow(state, b, a.label);
    if(failed(r)) return r;
    var l = rowToList(a.rest);
    if(l.rest && r.sub[l.rest.id])
      return fail('Recursive row type ' +
        T.toString(a) + ' and ' + T.toString(b));
    var r1 = unify(r.state, subst(r.sub, a.type), subst(r.sub, r.type));
    if(failed(r1)) return r1;
    var s = compose(r1.sub, r.sub);
    var r2 = unify(r1.state, subst(s, a.rest), subst(s, r.rest));
    if(failed(r2)) return r2;
    return {
      sub: compose(r2.sub, s),
      state: r2.state,
    };
  }
  return fail('Cannot unify ' + T.toString(a) + ' and ' + T.toString(b));
};

var infer = (env, state, e) => {
  // console.log('infer: ' + E.toString(e));
  if(e.tag === E.Var) {
    if(!env.typings[e.name]) T.terr('undefined variable: ' + e.name);
    var r = instantiate(state, env.typings[e.name]);
    return {
      sub: {},
      type: r.type,
      state: r.state,
    };
  }
  if(e.tag === E.Lam) {
    var rv = fresh(state, K.Star);
    var nenv = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, T.tscheme([], rv.tvar)));
    var ri = infer(nenv, rv.state, e.body);
    return {
      sub: ri.sub,
      type: T.tarr(subst(ri.sub, rv.tvar), ri.type),
      state: ri.state,
    };
  }
  if(e.tag === E.App) {
    var rleft = infer(env, state, e.left);
    var nenv = U.clone(env, 'typings', substEnv(rleft.sub, env.typings));
    var rright = infer(nenv, rleft.state, e.right);
    var rv = fresh(rright.state, K.Star);
    var su = unify(
      rv.state,
      subst(rright.sub, rleft.type),
      T.tarr(rright.type, rv.tvar)
    );
    if(failed(su)) throw su;
    return {
      sub: compose(su.sub, compose(rright.sub, rleft.sub)),
      type: subst(su.sub, rv.tvar),
      state: su.state,
    };
  }
  if(e.tag === E.Let) {
    var rval = infer(env, state, e.val);
    var genv = substEnv(rval.sub, env.typings);
    var nenv = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, generalize(genv, rval.type)));
    var rbody = infer(nenv, rval.state, e.body);
    return {
      sub: compose(rbody.sub, rval.sub),
      type: rbody.type,
      state: rbody.state,
    };
  }
  if(e.tag === E.Letr) {
    var v = fresh(state, K.Star);
    var nenv = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, T.tscheme([], v.tvar)));
    var rval = infer(nenv, v.state, e.val);
    var ta = subst(rval.sub, v.tvar);
    var tb = subst(rval.sub, rval.type);
    if(ta === tb || (ta.tag === T.TVar && tb.tag === T.TVar && ta.id === tb.id))
      T.terr('Recursive unification in letr');
    var u = unify(rval.state, ta, tb);
    if(failed(u)) throw u;
    var sub = compose(u.sub, rval.sub);
    var type = subst(sub, v.tvar);
    var genv = substEnv(sub, env.typings);
    var nenv2 = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, generalize(genv, type)));
    var rbody = infer(nenv2, u.state, e.body);
    return {
      sub: compose(rbody.sub, sub),
      type: rbody.type,
      state: rbody.state,
    };
  }
  if(e.tag === E.Do) {
    var rval = infer(env, state, e.val);
    var reff = fresh(rval.state, K.Row);
    var rpuretype = fresh(reff.state, K.Star);
    var ru1 = unify(
      rpuretype.state,
      rval.type,
      T.tapp(T.TEff, reff.tvar, rpuretype.tvar)
    );
    var sub1 = compose(ru1.sub, rval.sub);
    var newEnv =
      U.clone(env, 'typings', U.clone(env.typings, e.arg,
        T.tscheme([], subst(sub1, rpuretype.tvar))));
    var rbody = infer(newEnv, ru1.state, e.body);
    var sub2 = compose(rbody.sub, sub1);
    var rreff = fresh(rbody.state, K.Row);
    var rt = fresh(rreff.state, K.Star);
    var ru2 = unify(
      rt.state,
      subst(sub2, rbody.type),
      T.tapp(T.TEff, rreff.tvar, rt.tvar)
    );
    var sub3 = compose(ru2.sub, sub2);
    var ru3 = unify(
      ru2.state,
      subst(sub3, reff.tvar),
      subst(sub3, rreff.tvar)
    );
    var sub4 = compose(ru3.sub, sub3);
    return {
      sub: sub4,
      type: subst(sub4, rbody.type),
      state: ru3.state,
    };
  }
  if(e.tag === E.If) {
    var rcond = infer(env, state, e.cond);
    var ru1 = unify(rcond.state, rcond.type, T.Bool);
    if(failed(ru1)) throw ru1;
    var rtrue = infer(env, ru1.state, e.bodyTrue);
    var rfalse = infer(env, rtrue.state, e.bodyFalse);
    var sub =
      compose(rfalse.sub, compose(rtrue.sub, compose(ru1.sub, rcond.sub)));
    var ru2 = unify(rfalse.state,
      subst(sub, rtrue.type), subst(sub, rfalse.type));
    if(failed(ru2)) throw ru2;
    var sub2 = compose(ru2.sub, sub);
    return {
      sub: sub2,
      type: subst(sub2, rtrue.type),
      state: ru2.state,
    };
  }

  if(e.tag === E.RecordEmpty) {
    return {
      sub: {},
      type: T.tapp(T.TRecord, T.trowempty),
      state,
    }
  }
  if(e.tag === E.Select) {
    var v = fresh(state, K.Star);
    var r = fresh(v.state, K.Row, U.set(e.label));
    return {
      sub: {},
      type: T.tarr(
        T.tapp(T.TRecord, T.trowextend(e.label, v.tvar, r.tvar)),
        v.tvar
      ),
      state: r.state,
    };
  }
  if(e.tag === E.Extend) {
    var v = fresh(state, K.Star);
    var r = fresh(v.state, K.Row, U.set(e.label));
    return {
      sub: {},
      type: T.tarr(
        v.tvar,
        T.tarr(
          T.tapp(T.TRecord, r.tvar),
          T.tapp(T.TRecord, T.trowextend(e.label, v.tvar, r.tvar))
        )
      ),
      state: r.state,
    };
  }
  if(e.tag === E.Restrict) {
    var v = fresh(state, K.Star);
    var r = fresh(v.state, K.Row, U.set(e.label));
    return {
      sub: {},
      type: T.tarr(
        T.tapp(T.TRecord, T.trowextend(e.label, v.tvar, r.tvar)),
        T.tapp(T.TRecord, r.tvar)
      ),
      state: r.state,
    };
  }
  if(e.tag === E.RecordUpdate) {
    var v1 = fresh(state, K.Star);
    var v2 = fresh(v1.state, K.Star);
    var r = fresh(v2.state, K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    return {
      sub: {},
      type: T.tarr(
        T.tarr(a, b),
        T.tapp(T.TRecord, T.trowextend(e.label, a, r.tvar)),
        T.tapp(T.TRecord, T.trowextend(e.label, b, r.tvar))
      ),
      state: r.state,
    };
  }

  if(e.tag === E.Inject) {
    var v = fresh(state, K.Star);
    var r = fresh(v.state, K.Row, U.set(e.label));
    return {
      sub: {},
      type: T.tarr(
        v.tvar,
        T.tapp(T.TVariant, T.trowextend(e.label, v.tvar, r.tvar))
      ),
      state: r.state,
    };
  }
  if(e.tag === E.Embed) {
    var v = fresh(state, K.Star);
    var r = fresh(v.state, K.Row, U.set(e.label));
    return {
      sub: {},
      type: T.tarr(
        T.tapp(T.TVariant, r.tvar),
        T.tapp(T.TVariant, T.trowextend(e.label, v.tvar, r.tvar))
      ),
      state: r.state,
    };
  }
  if(e.tag === E.Elim) {
    var v1 = fresh(state, K.Star);
    var v2 = fresh(v1.state, K.Star);
    var r = fresh(v2.state, K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    return {
      sub: {},
      type: T.tarr(
        T.tarr(a, b),
        T.tarr(T.tapp(T.TVariant, r.tvar), b),
        T.tapp(T.TVariant, T.trowextend(e.label, a, r.tvar)),
        b
      ),
      state: r.state,
    };
  }
  if(e.tag === E.VariantUpdate) {
    var v1 = fresh(state, K.Star);
    var v2 = fresh(v1.state, K.Star);
    var r = fresh(v2.state, K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    return {
      sub: {},
      type: T.tarr(
        T.tarr(a, b),
        T.tapp(T.TVariant, T.trowextend(e.label, a, r.tvar)),
        T.tapp(T.TVariant, T.trowextend(e.label, b, r.tvar))
      ),
      state: r.state,
    };
  }

  if(e.tag === E.End) {
    var v = fresh(state, K.Star);
    return {
      sub: {},
      type: T.tarr(T.tapp(T.TVariant, T.trowempty), v.tvar),
      state: v.state,
    };
  }
  if(e.tag === E.Pure) {
    var v = fresh(state, K.Star);
    return {
      sub: {},
      type: T.tarr(T.tapp(T.TEff, T.trowempty, v.tvar), v.tvar),
      state: v.state,
    };
  }
  if(e.tag === E.Return) {
    var v = fresh(state, K.Star);
    var r = fresh(v.state, K.Row);
    return {
      sub: {},
      type: T.tarr(v.tvar, T.tapp(T.TEff, r.tvar, v.tvar)),
      state: r.state,
    };
  }

  if(e.tag === E.Pack) {
    if(!env.newtypes[e.label])
      T.terr('Cannot pack ' + e.label + ', not a valid newtype');
    var newtype = env.newtypes[e.label];
    var type = instantiate(state, newtype.type);
    var args = newtype.args.map(v => type.sub[v.id] || v);
    var ret = args.length > 0?
      T.tapp.apply(null, [newtype.con].concat(args)):
      newtype.con;
    return {
      sub: {},
      type: T.tarr(type.type, ret),
      state: type.state,
    };
  }
  if(e.tag === E.Unpack) {
    if(!env.newtypes[e.label])
      T.terr('Cannot unpack ' + e.label + ', not a valid newtype');
    var newtype = env.newtypes[e.label];
    var type = instantiate(state, newtype.type);
    var args = newtype.args.map(v => type.sub[v.id] || v);
    var ret = args.length > 0?
      T.tapp.apply(null, [newtype.con].concat(args)):
      newtype.con;
    return {
      sub: {},
      type: T.tarr(ret, type.type),
      state: type.state,
    };
  }

  if(e.tag === E.Perform) {
    // a -> Eff {label : a -> b | r/Get} b
    var v1 = fresh(state, K.Star);
    var v2 = fresh(v1.state, K.Star);
    var r = fresh(v2.state, K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    return {
      sub: {},
      type: T.tarr(
        a,
        T.tapp(T.TEff, T.trowextend(e.label, T.tarr(a, b), r.tvar), b)
      ),
      state: r.state,
    };
  }

  T.terr('Cannot infer: ' + E.toString(e));
};

var prepareEnv = env_ => {
  var env = env_ || {};
  env.typings = env.typings || {};
  env.newtypes = env.newtypes || {};
  return env;
};

var initialState = { tvar: 0 };

var runInfer = (e, env, st) => {
  var r = infer(prepareEnv(env), st || initialState, e);
  return subst(r.sub, r.type);
};

module.exports = {
  infer: runInfer,
};

/**
 * type defs
 * class defs
 * inst defs
 * type aliases
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

var fresh = (state, name, kind, labels, value, classes) => {
  var n = state.tvar[name] || 0;
  return {
    tvar: T.tvar(name, name + n, kind, labels, value, classes),
    state: U.clone(state, 'tvar', U.clone(state.tvar, name, n + 1)),
  };
};

var instantiate = (state, s) => {
  var r = s.vars.reduce((r, v) => {
    var rf = fresh(r.state, v.name, v.kind, v.labels, v.value, v.classes);
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

var checkClasses = (env, state_, v, t) => {
  var impls = {};
  var state = state_;
  var sub = {};
  for(var c in v.classes) {
    if(!env.classes[c]) return fail('Undefined class: ' + c);
    var ci = env.classes[c];
    var insts = ci.instances;
    var found = [];
    for(var i = 0, l = insts.length; i < l; i++) {
      var inst = instantiate(state, insts[i]);
      var ru = unify(env, inst.state, t, inst.type);
      if(!failed(ru)) found.push({
        result: ru,
        name: ci.dicts[i],
      });
    }
    if(found.length > 1)
      return fail('Multiple matching instances found for ' + c +
        ' ' + T.toString(t));
    if(found.length === 0)
      return fail('No matching instances found for ' + c + ' ' + T.toString(t));
    impls[c] = {
      name: found[0].name,
      children: found[0].result.impls,
    };
    state = found[0].result.state;
    sub = compose(found[0].result.sub, sub);
  }
  return {
    state,
    sub,
    impls: U.keys(impls).length === 0? null: impls,
  };
};

var occurs = (v, t) => !!free(t)[v.id];

var bind = (env, state, v, t) => {
  if(t.tag === T.TVar) {
    if(v.id === t.id) return { sub: {}, state };
    var m = fresh(state, v.name, v.kind, U.union(v.labels, t.labels),
      v.value || t.value, U.union(v.classes, t.classes));
    return {
      sub: U.map(v.id, m.tvar, t.id, m.tvar),
      state: m.state,
    };
  }
  if(v.kind === K.Row) return bindRow(state, v, t);
  if(occurs(v, t))
    return fail('Occurs check failed: ' + T.toString(v) +
      ' and ' + T.toString(t));
  if(v.value && T.isEff(t))
    return fail('Cannot bind ' + T.toString(v) + ' to ' + T.toString(t)
      + ' because of value restriction');
  var r = checkClasses(env, state, v, t);
  if(failed(r)) return r;
  return {
    sub: compose(U.map(v.id, t), r.sub),
    state: r.state,
    impls: r.impls? U.map(v.id, r.impls): null,
  };
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
  var rv = fresh(state, 'r', K.Row, U.union(ls1, row.rest.labels));
  var s2 = U.map(row.rest.id, rv.tvar);
  return {
    sub: compose(s2, s1),
    state: rv.state,
  };
};

var mergeImpls = (a, b) => {
  if(!a && !b) return null;
  if(!a) return b;
  if(!b) return a;
  return U.union(a, b);
};

var rewriteRow = (state, t, label) => {
  if(t.tag === T.TRowEmpty) return fail(label + ' cannot be inserted');
  if(t.tag === T.TRowExtend) {
    if(label === t.label) return {type: t.type, rest: t.rest, sub: {}, state};
    if(t.rest.tag === T.TVar) {
      var rv1 = fresh(state, 'r', K.Row, U.set(label));
      var rv2 = fresh(rv1.state, 't', K.Star);
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

var unify = (env, state, a, b) => {
  // console.log('unify ' + T.toString(a) + ' and ' + T.toString(b));
  var sk = unifyKind(a.kind, b.kind);
  if(failed(sk))
    return fail('Cannot unify ' + T.toString(a) + ' and ' +
      T.toString(b) + ': ' + sk);
  if(a.tag === T.TVar) return bind(env, state, a, b);
  if(b.tag === T.TVar) return bind(env, state, b, a);
  if(a.tag === T.TCon && b.tag === T.TCon && a.name === b.name)
    return { sub: {}, state };
  if(a.tag === T.TApp && b.tag === T.TApp) {
    var s1 = unify(env, state, a.left, b.left);
    if(failed(s1)) return s1;
    var s2 = unify(env, s1.state, subst(s1.sub, a.right), subst(s1.sub, b.right));
    if(failed(s2)) return s2;
    return {
      sub: compose(s2.sub, s1.sub),
      state: s2.state,
      impls: mergeImpls(s1.impls, s2.impls),
    };
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
    var r1 = unify(env, r.state, subst(r.sub, a.type), subst(r.sub, r.type));
    if(failed(r1)) return r1;
    var s = compose(r1.sub, r.sub);
    var r2 = unify(env, r1.state, subst(s, a.rest), subst(s, r.rest));
    if(failed(r2)) return r2;
    return {
      sub: compose(r2.sub, s),
      state: r2.state,
      impls: mergeImpls(r1.impls, r2.impls),
    };
  }
  return fail('Cannot unify ' + T.toString(a) + ' and ' + T.toString(b));
};

var mergeDicts = function() {
  return Array.prototype.slice.call(arguments).reduce(mergeImpls, null);
};

var infer = (env, state, e) => {
  // console.log('infer: ' + E.toString(e));
  if(e.tag === E.Var) {
    if(!env.typings[e.name]) T.terr('undefined variable: ' + e.name);
    var r = instantiate(state, env.typings[e.name]);
    var expr = E.vr(e.name);
    expr.meta.type = r.type;
    var classes = collectClasses(r.type);
    if(U.keys(classes).length > 0)
      expr.meta.classes = classes;
    return {
      sub: {},
      type: r.type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Lam) {
    var rv = fresh(state, e.arg, K.Star);
    var nenv = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, T.tscheme([], rv.tvar)));
    var ri = infer(nenv, rv.state, e.body);
    var type = T.tarr(subst(ri.sub, rv.tvar), ri.type);
    var expr = E.lam(e.arg, ri.expr);
    expr.meta.type = type;
    return {
      sub: ri.sub,
      type,
      state: ri.state,
      expr,
      dicts: ri.dicts,
    };
  }
  if(e.tag === E.App) {
    var rleft = infer(env, state, e.left);
    var nenv = U.clone(env, 'typings', substEnv(rleft.sub, env.typings));
    var rright = infer(nenv, rleft.state, e.right);
    var rv = fresh(rright.state, 't', K.Star);
    var su = unify(
      env,
      rv.state,
      subst(rright.sub, rleft.type),
      T.tarr(rright.type, rv.tvar)
    );
    if(failed(su)) throw su;
    var type = subst(su.sub, rv.tvar);
    var expr = E.app(rleft.expr, rright.expr);
    expr.meta.type = type;
    return {
      sub: compose(su.sub, compose(rright.sub, rleft.sub)),
      type,
      state: su.state,
      expr,
      dicts: mergeDicts(rright.dicts, su.impls, rleft.dicts),
    };
  }
  if(e.tag === E.Let) {
    var rval = infer(env, state, e.val);
    var genv = substEnv(rval.sub, env.typings);
    var nenv = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, generalize(genv, rval.type)));
    var rbody = infer(nenv, rval.state, e.body);
    var expr = E.lt(e.arg, rval.expr, rbody.expr);
    expr.meta.type = rbody.type;
    return {
      sub: compose(rbody.sub, rval.sub),
      type: rbody.type,
      state: rbody.state,
      expr,
      dicts: mergeDicts(rval.dicts, rbody.dicts),
    };
  }
  if(e.tag === E.Letr) {
    var v = fresh(state, e.arg, K.Star);
    var nenv = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, T.tscheme([], v.tvar)));
    var rval = infer(nenv, v.state, e.val);
    var ta = subst(rval.sub, v.tvar);
    var tb = subst(rval.sub, rval.type);
    if(ta === tb || (ta.tag === T.TVar && tb.tag === T.TVar && ta.id === tb.id))
      T.terr('Recursive unification in letr');
    var u = unify(env, rval.state, ta, tb);
    if(failed(u)) throw u;
    var sub = compose(u.sub, rval.sub);
    var type = subst(sub, v.tvar);
    var genv = substEnv(sub, env.typings);
    var nenv2 = U.clone(env, 'typings',
      U.clone(env.typings, e.arg, generalize(genv, type)));
    var rbody = infer(nenv2, u.state, e.body);
    var expr = E.ltr(e.arg, rval.expr, rbody.expr);
    expr.meta.type = rbody.type;
    return {
      sub: compose(rbody.sub, sub),
      type: rbody.type,
      state: rbody.state,
      expr,
      dicts: mergeDicts(
        rval.dicts,
        u.impls,
        rbody.dicts
      ),
    };
  }
  if(e.tag === E.Do) {
    var rval = infer(env, state, e.val);
    var reff = fresh(rval.state, e.arg, K.Row);
    var rpuretype = fresh(reff.state, 't', K.Star, null, true);
    var ru1 = unify(
      env,
      rpuretype.state,
      rval.type,
      T.tapp(T.TEff, reff.tvar, rpuretype.tvar)
    );
    if(failed(ru1)) throw ru1;
    var sub1 = compose(ru1.sub, rval.sub);
    var newEnv =
      U.clone(env, 'typings', U.clone(env.typings, e.arg,
        T.tscheme([], subst(sub1, rpuretype.tvar))));
    var rbody = infer(newEnv, ru1.state, e.body);
    var sub2 = compose(rbody.sub, sub1);
    var rreff = fresh(rbody.state, 'e', K.Row);
    var rt = fresh(rreff.state, 't', K.Star, null, true);
    var ru2 = unify(
      env,
      rt.state,
      subst(sub2, rbody.type),
      T.tapp(T.TEff, rreff.tvar, rt.tvar)
    );
    if(failed(ru2)) throw ru2;
    var sub3 = compose(ru2.sub, sub2);
    var ru3 = unify(
      env,
      ru2.state,
      subst(sub3, reff.tvar),
      subst(sub3, rreff.tvar)
    );
    if(failed(ru3)) throw ru3;
    var sub4 = compose(ru3.sub, sub3);
    var type = subst(sub4, rbody.type);
    var expr = E.doo(e.arg, rval.expr, rbody.expr);
    expr.meta.type = type;
    return {
      sub: sub4,
      type,
      state: ru3.state,
      expr,
      dicts: mergeDicts(
        rval.dicts,
        ru1.impls,
        rbody.dicts,
        ru2.impls,
        ru3.impls
      ),
    };
  }
  if(e.tag === E.If) {
    var rcond = infer(env, state, e.cond);
    var ru1 = unify(env, rcond.state, rcond.type, T.Bool);
    if(failed(ru1)) throw ru1;
    var rtrue = infer(env, ru1.state, e.bodyTrue);
    var rfalse = infer(env, rtrue.state, e.bodyFalse);
    var sub =
      compose(rfalse.sub, compose(rtrue.sub, compose(ru1.sub, rcond.sub)));
    var ru2 = unify(env, rfalse.state,
      subst(sub, rtrue.type), subst(sub, rfalse.type));
    if(failed(ru2)) throw ru2;
    var sub2 = compose(ru2.sub, sub);
    var type = subst(sub2, rtrue.type);
    var expr = E.if(rcond.expr, rtrue.expr, rfalse.expr);
    expr.meta.type = type;
    return {
      sub: sub2,
      type,
      state: ru2.state,
      expr,
      dicts: mergeDicts(
        rcond.dicts,
        ru1.impls,
        rtrue.dicts,
        rfalse.dicts,
        ru2.impls
      ),
    };
  }

  if(e.tag === E.RecordEmpty) {
    var type = T.tapp(T.TRecord, T.trowempty);
    var expr = E.recordempty();
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state,
      expr,
      dicts: null,
    }
  }
  if(e.tag === E.Select) {
    var v = fresh(state, 't', K.Star);
    var r = fresh(v.state, 'r', K.Row, U.set(e.label));
    var type = T.tarr(
      T.tapp(T.TRecord, T.trowextend(e.label, v.tvar, r.tvar)),
      v.tvar
    );
    var expr = E.select(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Extend) {
    var v = fresh(state, 't', K.Star);
    var r = fresh(v.state, 'r', K.Row, U.set(e.label));
    var type = T.tarr(
      v.tvar,
      T.tarr(
        T.tapp(T.TRecord, r.tvar),
        T.tapp(T.TRecord, T.trowextend(e.label, v.tvar, r.tvar))
      )
    );
    var expr = E.extend(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Restrict) {
    var v = fresh(state, 't', K.Star);
    var r = fresh(v.state, 'r', K.Row, U.set(e.label));
    var type = T.tarr(
      T.tapp(T.TRecord, T.trowextend(e.label, v.tvar, r.tvar)),
      T.tapp(T.TRecord, r.tvar)
    );
    var expr = E.restrict(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.RecordUpdate) {
    var v1 = fresh(state, 'a', K.Star);
    var v2 = fresh(v1.state, 'b', K.Star);
    var r = fresh(v2.state, 'r', K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    var type = T.tarr(
      T.tarr(a, b),
      T.tapp(T.TRecord, T.trowextend(e.label, a, r.tvar)),
      T.tapp(T.TRecord, T.trowextend(e.label, b, r.tvar))
    );
    var expr = E.recordupdate(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }

  if(e.tag === E.Inject) {
    var v = fresh(state, 't', K.Star);
    var r = fresh(v.state, 'r', K.Row, U.set(e.label));
    var type = T.tarr(
      v.tvar,
      T.tapp(T.TVariant, T.trowextend(e.label, v.tvar, r.tvar))
    );
    var expr = E.inject(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Embed) {
    var v = fresh(state, 't', K.Star);
    var r = fresh(v.state, 'r', K.Row, U.set(e.label));
    var type = T.tarr(
      T.tapp(T.TVariant, r.tvar),
      T.tapp(T.TVariant, T.trowextend(e.label, v.tvar, r.tvar))
    );
    var expr = E.embed(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Elim) {
    var v1 = fresh(state, 'a', K.Star);
    var v2 = fresh(v1.state, 'b', K.Star);
    var r = fresh(v2.state, 'r', K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    var type = T.tarr(
      T.tarr(a, b),
      T.tarr(T.tapp(T.TVariant, r.tvar), b),
      T.tapp(T.TVariant, T.trowextend(e.label, a, r.tvar)),
      b
    );
    var expr = E.elim(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.VariantUpdate) {
    var v1 = fresh(state, 'a', K.Star);
    var v2 = fresh(v1.state, 'b', K.Star);
    var r = fresh(v2.state, 'r', K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    var type = T.tarr(
      T.tarr(a, b),
      T.tapp(T.TVariant, T.trowextend(e.label, a, r.tvar)),
      T.tapp(T.TVariant, T.trowextend(e.label, b, r.tvar))
    );
    var expr = E.variantupdate(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }

  if(e.tag === E.End) {
    var v = fresh(state, 't', K.Star);
    var type = T.tarr(T.tapp(T.TVariant, T.trowempty), v.tvar);
    var expr = E.end();
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: v.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Pure) {
    var v = fresh(state, 't', K.Star, null, true);
    var type = T.tarr(T.tapp(T.TEff, T.trowempty, v.tvar), v.tvar);
    var expr = E.pure();
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: v.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.Return) {
    var v = fresh(state, 't', K.Star, null, true);
    var r = fresh(v.state, 'r', K.Row);
    var type = T.tarr(v.tvar, T.tapp(T.TEff, r.tvar, v.tvar));
    var expr = E.retrn();
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
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
    var rtype = T.tarr(type.type, ret);
    var expr = E.pack(e.label);
    expr.meta.type = rtype;
    return {
      sub: {},
      type: rtype,
      state: type.state,
      expr,
      dicts: null,
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
    var rtype = T.tarr(ret, type.type);
    var expr = E.unpack(e.label);
    expr.meta.type = rtype;
    return {
      sub: {},
      type: rtype,
      state: type.state,
      expr,
      dicts: null,
    };
  }

  if(e.tag === E.Perform) {
    var v1 = fresh(state, 'a', K.Star, null, true);
    var v2 = fresh(v1.state, 'b', K.Star, null, true);
    var r = fresh(v2.state, 'r', K.Row, U.set(e.label));
    var a = v1.tvar;
    var b = v2.tvar;
    var type = T.tarr(
      a,
      T.tapp(T.TEff, T.trowextend(e.label, T.tarr(a, b), r.tvar), b)
    );
    var expr = E.perform(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }

  if(e.tag === E.Handle) {
    var ra = fresh(state, 'a', K.Star, null, true);
    var rb = fresh(ra.state, 'b', K.Star, null, true);
    var rt1 = fresh(rb.state, 'x', K.Star, null, true);
    var rt2 = fresh(rt1.state,'y',  K.Star, null, true);
    var rr = fresh(rt2.state, 'r', K.Row, U.set(e.label));
    var a = ra.tvar;
    var b = rb.tvar;
    var r = rr.tvar;
    var t1 = rt1.tvar;
    var t2 = rt2.tvar;
    var type = T.tarr(
      T.tarr(
        a,
        T.tarr(b, T.tapp(T.TEff, r, t2)),
        T.tapp(T.TEff, r, t2)
      ),
      T.tapp(T.TEff, T.trowextend(e.label, T.tarr(a, b), r), t1),
      T.tapp(T.TEff, r, t2)
    );
    var expr = E.handle(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: rr.state,
      expr,
      dicts: null,
    };
  }
  if(e.tag === E.HandleReturn) {
    var ra = fresh(state, 'a', K.Star, null, true);
    var rb = fresh(ra.state, 'b', K.Star, null, true);
    var rr = fresh(rb.state, 'r', K.Row);
    var a = ra.tvar;
    var b = rb.tvar;
    var r = rr.tvar;
    var type = T.tarr(
      T.tarr(a, T.tapp(T.TEff, r, b)),
      T.tapp(T.TEff, r, a),
      T.tapp(T.TEff, r, b)
    );
    var expr = E.handlereturn(e.label);
    expr.meta.type = type;
    return {
      sub: {},
      type,
      state: rr.state,
      expr,
      dicts: null,
    };
  }

  if(e.tag === E.TypeOf) {
    var r = infer(env, state, e.expr);
    var type = T.tapp(T.TType, r.type);
    var expr = E.typeOf(r.expr);
    expr.meta.type = type;
    return {
      sub: r.sub,
      type,
      state: r.state,
      expr,
      dicts: null,
    };
  }

  if(e.tag === E.Anno) {
    var etype = e.decltype;
    var itype = infer(env, state, e.expr);
    var ru = unify(env, itype.state, etype, itype.type);
    var sub = compose(ru.sub, itype.sub);
    var type = subst(sub, etype);
    var expr = E.anno(itype.expr, e.type);
    expr.meta.type = type;
    return {
      sub: sub,
      type,
      state: ru.state,
      expr,
      dicts: mergeDicts(
        itype.dicts,
        ru.impls
      ),
    };
  }

  if(e.tag === E.Str) {
    var expr = E.str(e.val);
    expr.meta.type = T.Str;
    return {
      sub: {},
      type: T.Str,
      state: state,
      expr,
      dicts: null,
    };
  }

  T.terr('Cannot infer: ' + E.toString(e));
};

var prepareEnv = env_ => {
  var env = env_ || {};
  env.typings = env.typings || {};
  env.newtypes = env.newtypes || {};
  env.classes = env.classes || {};
  return env;
};

var initialState = { tvar: {} };

var runInfer = (e, env, st) => {
  var r = infer(prepareEnv(env), st || initialState, e);
  E.each(x => {
    x.meta.type = subst(r.sub, x.meta.type);
    if(x.meta.classes) {
      var cs = x.meta.classes;
      for(var v in cs)
        if(r.sub[v] && r.sub[v].tag === T.TVar)
          cs[r.sub[v].id] = cs[v];
    }
  }, r.expr);
  return {
    type: subst(r.sub, r.type),
    expr: r.expr,
    dicts: r.dicts,
    sub: r.sub
  };
};

var collectClasses = t => U.ofilter(x => x, U.omap(v => {
  var k = U.keys(v.classes);
  return k.length === 0? null: k;
}, free(t)));

module.exports = {
  infer: runInfer,
  collectClasses,
};

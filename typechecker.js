/*
Test unit and (+ 1)
add auto Lazy support

Auto import Prelude
Importing of types
Module system!!

Add Aff (or figure out asynchronous effects)

Add pointed
Maybe subclassing of instances?

Make parser async
Case matching for numbers and unit and arrays
Pattern matching
Handle js reserved names
aliases

Make actual parser
*/
var K = require('./kinds');
var T = require('./types');
var E = require('./exprs');

// util
var clone = (o, key, val) => {
  var n = {};
  for(var k in o) n[k] = o[k];
  if(key) n[key] = val;
  return n;
};

var obj = function() {
  var l = arguments.length;
  if(l % 2 !== 0) throw new Error('Invalid number of arguments for obj');
  var n = {};
  for(var i = 0; i < l; i += 2) n[arguments[i]] = arguments[i + 1];
  return n;
};

var keySet = function(o, without) {
  var n = {};
  for(var k in o) n[k] = true;
  if(without) {
    for(var i = 0, l = without.length; i < l; i++)
      delete n[without[i]];
  }
  return n;
};

var keyIntersect = (a, b) => {
  for(var k in a) if(b[k]) return true;
  for(var k in b) if(a[k]) return true;
  return false;
};

var union = (a, b) => {
  var n = {};
  for(var k in a) n[k] = a[k];
  for(var k in b) n[k] = b[k];
  return n;
};

var logR = (s, f) => function() {
  var r = f.apply(null, arguments);
  console.log(s(r, arguments));
  return r;
};

// helpers
var tapp2 = (left, right) => {
  var kv = K.kvar('k');
  var rv = K.kvar('r');
  unifyKind(pruneKind(left.kind), K.karr(kv, rv));
  unifyKind(pruneKind(right.kind), pruneKind(kv));
  return T.tapp(left, right, pruneKind(rv));
};
var tapp = function() {
  var l = arguments.length;
  if(l < 2) T.terr('tapp needs at least two arguments');
  var c = tapp2(arguments[0], arguments[1]);
  for(var i = 2; i < l; i++) c = tapp2(c, arguments[i]);
  return c;
};

var tarr2 = (left, right) => tapp(T.TArr, left, right);
var tarr = function() {
  var l = arguments.length;
  if(l < 1) T.terr('tarr needs at least 1 argument');
  if(l === 1) return tapp(T.TArr, arguments[0]);
  var c = tarr2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = tarr2(arguments[i], c);
  return c;
};

var trow = T.trow;
var trec = row => tapp(T.TRec, row);
var treco = (o, v) => trec(trow(o, v));

// typechecker
var occursKind = (v, k_) => {
  var k = pruneKind(k_);
  if(v === k || (k.tag === K.KVar && v.id === k.id)) return true;
  if(k.tag === K.KArr) return occursKind(v, k.left) || occursKind(v, k.right);
  return false;
};

var pruneKind = k => {
  if(k.tag === K.KVar && k.instance)
    return k.instance = pruneKind(k.instance);
  else if(k.tag === K.KArr) {
    k.left = pruneKind(k.left);
    k.right = pruneKind(k.right);
  }
  return k;
};

var bindKind = (v, k) => {
  if(v === k || (k.tag === K.KVar && v.id === k.id)) return;
  if(occursKind(v, k))
    T.terr('Recursive kind unification ' + K.toString(v) +
      ' and ' + K.toString(k));
  v.instance = k;
};

var unifyKind = (a_, b_) => {
  var a = pruneKind(a_);
  var b = pruneKind(b_);
  if(a.tag === K.KVar) return bindKind(a, b);
  else if(b.tag === K.KVar) return bindKind(b, a);
  else if(a.tag === K.KCon && b.tag === K.KCon && a.name === b.name) return;
  else if(a.tag === K.KArr && b.tag === K.KArr) {
    unifyKind(a.left, b.left);
    unifyKind(a.right, b.right);
    return;
  }
  T.terr('Cannot unify kinds ' + K.toString(a) + ' and ' + K.toString(b));
};

var checkCon = (env, con) => {
  if(!env.types[con.name]) T.terr('Undefined type: ' + con.name);
  var kind = freshKind(env.types[con.name].con.kind);
  unifyKind(con.kind, kind);
  return prune(con);
};

var checkTypeR = (env, t) => {
  if(t.tag === T.TRowExtend) {
    t.type = checkType(env, t.type);
    t.rest = checkType(env, t.rest);
  } else if(t.tag === T.TApp) {
    t.left = checkType(env, t.left);
    t.right = checkType(env, t.right);
    tapp(checkType(env, t.left), checkType(env, t.right));
  } else if(t.tag === T.TCon) {
    return checkCon(env, t);
  } else if(t.tag === T.TImpl) {
    t.impl = checkType(env, t.impl);
    t.type = checkType(env, t.type);
  }
  return t;
};
var checkType = (env, t) => prune(checkTypeR(env, prune(t)));

var occurs = (v, t_) => {
  var t = prune(t_);
  if(v === t || (t.tag === T.TVar && v.id === t.id)) return true;
  if(t.tag === T.TApp) return occurs(v, t.left) || occurs(v, t.right);
  if(t.tag === T.TRowExtend) return occurs(v, t.type) || occurs(v, t.rest);
  if(t.tag === T.TImpl) return occurs(v, t.impl) || occurs(v, t.type);
  return false;
};

var free = (t_, vars_) => {
  var t = prune(t_);
  var vars = vars_ || {};
  if(t.tag === T.TVar) {
    vars[t.id] = true;
  } else if(t.tag === T.TApp) {
    free(t.left, vars);
    free(t.right, vars);
  } else if(t.tag === T.TRowExtend) {
    free(t.type, vars);
    free(t.rest, vars);
  } else if(t.tag === T.TImpl) {
    free(t.impl, vars);
    free(t.type, vars);
  } else if(t.tag === T.TScheme) {
    free(t.type, vars);
    var tvars = t.vars;
    for(var i = 0, l = tvars.length; i < l; i++)
      delete vars[tvars[i].id];
  }
  return vars;
};

var freeEnv = (env, vars_) => {
  var vars = vars_ || {};
  for(var k in env)
    free(env[k], vars);
  return vars;
};

var subst = (sub, t) => {
  if(t.tag === T.TVar && sub[t.id]) {
    return sub[t.id];
  } else if(t.tag === T.TApp) {
    return T.tapp(subst(sub, t.left), subst(sub, t.right), t.kind)
  } else if(t.tag === T.TRowExtend) {
    return T.trowextend(t.label, subst(sub, t.type), subst(sub, t.rest));
  } else if(t.tag === T.TImpl) {
    return T.timpl(subst(sub, t.impl), subst(sub, t.type));
  } else if(t.tag === T.TScheme) {
    var nsub = clone(sub);
    var vars = t.vars;
    for(var i = 0, l = vars.length; i < l; i++)
      delete nsub[vars[i].id];
    return T.tscheme(vars, subst(nsub, t.type));
  }
  return t;
};

var prune = t => {
  t.kind = pruneKind(t.kind);
  if(t.tag === T.TVar && t.instance)
    return t.instance = prune(t.instance);
  else if(t.tag === T.TApp) {
    t.left = prune(t.left);
    t.right = prune(t.right);
  } else if(t.tag === T.TRowExtend) {
    t.type = prune(t.type);
    t.rest = prune(t.rest);
  } else if(t.tag === T.TImpl) {
    t.impl = prune(t.impl);
    t.type = prune(t.type);
  }
  return t;
};

var rowParts = t => {
  if(t.tag === T.TVar) return {head: {}, tail: t};
  if(t.tag === T.TRowEmpty) return {head: {}, tail: null};
  if(t.tag === T.TRowExtend) {
    var r = rowParts(t.rest);
    return {head: clone(r.head, t.label, t.type), tail: r.tail};
  }
  T.terr('Cannot use rowParts on ' + T.toString(t));
};

var bindRow = (v, t) => {
  var map = rowParts(t);
  if(keyIntersect(map.head, v.lacks))
    T.terr('Repeated labels, ' + v + ' and ' + t);
  if(!map.tail) return v.instance = t;
  var r = map.tail;
  unifyKind(v.kind, r.kind);
  var m = T.tvar(
    r.name, pruneKind(v.kind), union(v.lacks, r.lacks), v.value || r.value);
  v.instance = prune(t);
  r.instance = m;
};

var rewriteRow = (r, l) => {
  if(r.tag === T.TRowEmpty) T.terr('Missing label ' + l + ' in row type');
  if(r.tag === T.TRowExtend) {
    var label = r.label;
    var type = r.type;
    var rest = r.rest;
    if(label === l) return {type, rest};
    if(rest.tag === T.TVar) {
      var beta = T.tvar('r', K.krow, obj(l, true));
      var gamma = T.tvar('a', K.kstar);
      bindRow(rest, T.trowextend(l, gamma, beta));
      return {
        type: gamma,
        rest: prune(T.trowextend(label, type, beta)),
      };
    }
    var res = rewriteRow(rest, l);
    return {
      type: res.type,
      rest: T.trowextend(label, type, res.rest),
    };
  }
  T.terr('Unexpected type in rewriteRow: ' + T.toString(r));
};

var bind = (v, t) => {
  if(v === t || (t.tag === T.TVar && v.id === t.id)) return;
  if(occurs(v, t))
    T.terr('Recursive unification ' + T.toString(v) + ' and ' + T.toString(t));
  if(t.tag === T.TVar) {
    var m = T.tvar(v.name,
      pruneKind(t.kind), union(v.lacks, t.lacks), v.value || t.value);
    v.instance = m;
    t.instance = m;
    return;
  }
  if(v.value && T.isEff(t))
    T.terr('Cannot bind ' + T.toString(v) + ' to ' + T.toString(t) +
      ' because of value restriction');
  v.instance = t;
  return;
};

var checkImpl = (env, impl, type) => {
  unify(env, impl.type, type);
  env.constraints.push({
    type: impl.impl,
    env,
  });
  env.constraintsl++;
  return;
};

var unify = (env, a_, b_) => {
  var a = prune(a_);
  var b = prune(b_);
  unifyKind(a.kind, b.kind);
  a = prune(a);
  b = prune(b);
  if(a.tag === T.TCon) a = checkCon(env, a);
  if(b.tag === T.TCon) b = checkCon(env, b);
  // console.log('unify: ' + T.toString(a) + ' and ' + T.toString(b));
  if(a.tag === T.TVar) return bind(a, b);
  else if(b.tag === T.TVar) return bind(b, a);
  else if(a.tag === T.TRowEmpty && b.tag === T.TRowEmpty) return;
  else if(a.tag === T.TCon && b.tag === T.TCon && a.name === b.name) return;
  else if(a.tag === T.TApp && b.tag === T.TApp) {
    unify(env, a.left, b.left);
    unify(env, a.right, b.right);
    return;
  } else if(a.tag === T.TRowExtend && b.tag === T.TRowExtend) {
    var r = rewriteRow(b, a.label);
    // var m = rowParts(a.rest);
    unify(env, a.type, r.type);
    unify(env, a.rest, r.rest);
    return;
  } else if(a.tag === T.TImpl && b.tag === T.TImpl) {
    unify(env, a.impl, b.impl);
    unify(env, a.type, b.type);
    return;
  } else if(a.tag === T.TImpl) return checkImpl(env, a, b);
  else if(b.tag === T.TImpl) return checkImpl(env, b, a);
  T.terr('Cannot unify ' + T.toString(a) + ' and ' + T.toString(b));
};

var freshKind = (k_, mappings_) => {
  var mappings = mappings_ || {};
  var k = pruneKind(k_);
  if(k.tag === K.KVar) {
    if(!mappings[k.id]) mappings[k.id] = K.kvar(k.name);
    return mappings[k.id];
  } else if(k.tag === K.KArr)
    return K.karr(
      freshKind(k.left, mappings),
      freshKind(k.right, mappings)
    );
  return k;
};

var freshADT = (adt, mappings) => {
  var args = adt.args.map(t => mappings[t.id] =
    T.tvar(t.name, t.kind, t.lacks, t.value));
  return args.length === 0? adt.con: tapp.apply(null, [adt.con].concat(args));
};

var instantiate = scheme => {
  var sub = {};
  scheme.vars.forEach(v =>
    sub[v.id] = T.tvar(v.name, v.kind, v.lacks, v.value));
  return subst(sub, scheme.type);
};

var generalize = (type, env) => {
  var vars = {};
  var freetype = free(type, vars);
  var freevars = freeEnv(env || {});
  for(var id in freevars) delete vars[id];
  var avars = [];
  for(var id in vars) {
    var v = T.getTVar(id);
    if(v) avars.push(v);
  }
  return T.tscheme(avars, type);
};

var getType = (name, env) => {
  if(!env[name]) T.terr('Undefined variable: ' + name);
  return instantiate(env[name]);
}

var consumeConstraints = (e, env) => {
  if(env.constraintsl > 0) {
    e.meta.inst = env.constraints.slice(
      env.constraints.length - env.constraintsl,
      env.constraints.length
    );
    env.constraintsl = 0;
  } else {
    e.meta.inst = [];
  }
};

var infer = (env, e) => {
  // console.log('infer: ' + E.toString(e));
  if(e.tag === E.Var) {
    var type = getType(e.name, env.typings);
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.App) {
    var fntype = infer(env, e.left);
    var argtype = infer(env, e.right);
    if(e.meta.impl) {
      unify(env, fntype, T.timpl(T.tvar('i', K.kstar), T.tvar('t', K.kstar)))
      var impltype = prune(fntype);
      unify(env, impltype.impl, argtype);
      var type = prune(impltype.type);
      e.meta.type = type;
      consumeConstraints(e, env);
      return type;
    } else {
      var restype = T.tvar('r', K.kstar);
      unify(env, fntype, tarr(argtype, restype));
      var type = prune(restype);
      e.meta.type = type;
      consumeConstraints(e, env);
      return type;
    }
  } else if(e.tag === E.Lam) {
    var argtype = T.tvar(e.arg, K.kstar);
    var newEnv = clone(env, 'typings',
      clone(env.typings, e.arg, T.tscheme([], argtype)));
    var restype = infer(newEnv, e.body);
    var type = prune(e.meta.impl?
      T.timpl(argtype, restype):
      tarr(argtype, restype));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Let) {
    if(e.meta.effect) {
      var valtype = infer(env, e.val);
      var eff = T.tvar('e', K.krow);
      var puretype = T.tvar('t', K.kstar, null, true);
      unify(env, valtype, T.teff(eff, puretype));
      puretype = prune(puretype);
      var newEnv =
        clone(env, 'typings', clone(env.typings, e.arg,
          T.tscheme([], puretype)));
      var res = infer(newEnv, e.body);
      var reff = T.tvar('e', K.krow);
      unify(env, res, T.teff(reff, T.tvar('t', K.kstar, null, true)));
      unify(env, eff, reff);
      var type = prune(res);
      e.meta.type = type;
      consumeConstraints(e, env);
      return type;
    } else if(!e.meta.recursive) {
      var valtype = infer(env, e.val);
      var newEnv = clone(env, 'typings',
        clone(env.typings, e.arg, generalize(valtype, env.typings)));
      if(e.meta.impl)
        newEnv = clone(newEnv, 'impl', clone(newEnv.impl, e.arg, true));
      var res = infer(newEnv, e.body);
      var type = prune(res);
      e.meta.type = type;
      consumeConstraints(e, env);
      return type;
    } else {
      var valtype = T.tvar(e.arg, K.kstar);
      var newEnv = clone(env, 'typings',
        clone(env.typings, e.arg, T.tscheme([], valtype)));
      if(e.meta.impl)
        newEnv = clone(newEnv, 'impl', clone(newEnv.impl, e.arg, true));
      var ivaltype = infer(newEnv, e.val);
      unify(env, ivaltype, valtype);
      var newNewEnv =
        clone(newEnv, 'typings',
          clone(env.typings, e.arg, generalize(prune(ivaltype), env.typings)));
      var res = infer(newNewEnv, e.body);
      var type = prune(res);
      e.meta.type = type;
      consumeConstraints(e, env);
      return type;
    }
  } else if(e.tag === E.If) {
    var ctype = infer(env, e.cond);
    unify(env, ctype, T.TBool);
    var ctrue = infer(env, e.bodyTrue);
    var cfalse = infer(env, e.bodyFalse);
    unify(env, ctrue, cfalse);
    var type = prune(ctrue);
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Anno) {
    var etype = checkType(env, e.type);
    var itype = infer(env, e.expr);
    unify(env, etype, itype);
    var type = prune(etype);
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Record) {
    var nmap = {};
    for(var k in e.map) nmap[k] = infer(env, e.map[k]);
    var type = prune(treco(nmap));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.RecordEmpty) {
    var type = T.TUnit;
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.RecordSelect) {
    var t = T.tvar('t', K.kstar);
    var r = T.tvar('r', K.krow, obj(e.label, true));
    var type = tarr(trec(T.trowextend(e.label, t, r)), t);
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.RecordExtend) {
    var t = T.tvar('t', K.kstar);
    var r = T.tvar('r', K.krow, obj(e.label, true));
    var type = tarr(t, tarr(trec(r), trec(T.trowextend(e.label, t, r))));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.RecordRestrict) {
    var t = T.tvar('t', K.kstar);
    var r = T.tvar('r', K.krow, obj(e.label, true));
    var type = tarr(trec(T.trowextend(e.label, t, r)), trec(r));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.RecordUpdate) {
    var a = T.tvar('a', K.kstar);
    var b = T.tvar('b', K.kstar);
    var r = T.tvar('r', K.krow, obj(e.label, true));
    var type = tarr(
      tarr(a, b),
      tarr(
        tapp(T.TRec, T.trowextend(e.label, a, r)),
        tapp(T.TRec, T.trowextend(e.label, b, r))
      )
    );
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Perform) {
    var a = T.tvar('a', K.kstar, null, true);
    var b = T.tvar('b', K.kstar, null, true);
    var eff = T.trowextend(
      e.label,
      tarr(a, b),
      T.tvar('e', K.krow, obj(e.label, true))
    );
    var type = tarr(a, T.teff(eff, b));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Handle) {
    var a = T.tvar('a', K.kstar, null, true);
    var b = T.tvar('b', K.kstar, null, true);
    var c = T.tvar('c', K.krow, keySet(e.map, ['return']));
    var bc = T.teff(c, b);
    var d = c;
    var retfound = false;
    for(var name in e.map) {
      if(name === 'return') {
        var type = infer(env, e.map[name]);
        unify(env, type, tarr(a, bc));
        retfound = true;
      } else {
        var type = infer(env, e.map[name]);
        var ai = T.tvar('ai', K.kstar, null, true);
        var bi = T.tvar('bi', K.kstar, null, true);
        unify(env, type, tarr(ai, tarr(bi, bc), bc));
        d = T.trowextend(name, tarr(ai, bi), d);
      }
    }
    if(!retfound) unify(env, a, b);
    var type = prune(tarr(T.teff(d, a), bc));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Case) {
    var cases = e.map;
    if(Object.keys(cases).length === 0)
      T.terr('Empty case expression');
    var t = T.tvar('t', K.kstar);
    var r = T.tvar('r', K.kstar);
    var placeholder = false;
    var allCases = {};
    var adt = null;
    var adttype = null;
    var mappings = {};
    for(var name in cases) {
      if(name === '_') {
        placeholder = true;
        unify(env,
          tarr(prune(t), prune(r)),
          infer(env, cases[name])
        );
      } else {
        if(!env.cases[name]) T.terr('Undefined constructor: ' + name);
        if(!adt) {
          adt = env.types[env.cases[name]];
          allCases = adt.cases;
          adttype = freshADT(adt, mappings);
          unify(env, t, adttype);
        }
        var caseTypes =
          adt.cases[name].map(t => prune(subst(mappings, t)));
        if(caseTypes.length === 0) caseTypes.push(T.TUnit);
        var fnType = prune(tarr.apply(null, caseTypes.concat([prune(r)])));
        unify(env,
          fnType,
          infer(env, cases[name])
        );
      }
    }
    if(!placeholder) {
      for(var name in allCases)
        if(!cases[name])
          T.terr('Missing case ' + name + ' for type ' + T.toString(prune(t)));
    }
    var type = tarr(prune(t), prune(r));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Type) {
    if(env.types[e.name]) T.terr('Duplicate type definition: ' + e.name);
    for(var k in e.cases)
      if(env.cases[k]) T.terr('Duplicate constructor definition: ' + k);
    var con = T.tcon(e.name);
    env.types[e.name] = {con, args: e.args, cases: e.cases};
    var typings = clone(env.typings);
    var r = e.args.length === 0? con: tapp.apply(null, [con].concat(e.args));
    for(var k in e.cases) {
      env.cases[k] = e.name;
      typings[k] = generalize(e.cases[k].length === 0? r:
        tarr.apply(null, e.cases[k].concat([r])))
    }
    env.types[e.name].con = prune(con);
    var type = prune(infer(clone(env, 'typings', typings), e.body));
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.List) {
    var t = T.tvar('t', K.kstar);
    for(var i = 0, a = e.arr, l = a.length; i < l; i++)
      unify(env, t, infer(env, a[i]));
    var type = tapp(T.TArray, t);
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Int) {
    var type = T.TInt;
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Float) {
    var type = T.TFloat;
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  } else if(e.tag === E.Str) {
    var type = T.TStr;
    e.meta.type = type;
    consumeConstraints(e, env);
    return type;
  }
  T.terr('Cannot infer expr tag: ' + e.tag);
};

var handleConstraints = a => {
  var results = [];
  for(var i = 0, l = a.length; i < l; i++) {
    var c = a[i];
    var impl = prune(c.type);
    var env = c.env;
    var g = generalize(prune(impl));
    var found = [];
    var constraints = env.constraints.slice(0);
    for(var k in env.impl) {
      var t = instantiate(env.typings[k]);
      try {
        env.constraints = constraints;
        env.constraintsl = 0;
        unify(env, instantiate(g), t);
        if(env.constraintsl > 0) {
          found.push([k, env.constraints.slice(
            env.constraints.length - env.constraintsl,
            env.constraints.length
          )]);
        } else {
          found.push([k]);
        }
      } catch(e) {
        if(!(e instanceof TypeError)) throw e;
      }
    }
    if(found.length === 0)
      T.terr('No instance found for ' + T.toString(impl));
    if(found.length > 1)
      T.terr('More than one instance found for ' + T.toString(impl) + ': ' +
        flatten(found).filter(x => typeof x === 'string').join(', '));
    var f =
      flatten(found[0].map(x => Array.isArray(x)? handleConstraints(x): x));
    unify(
      env,
      prune(impl),
      instantiate(env.typings[f[0]])
    );
    results.push(f);
  }
  return results;
};

var flatten = a =>
  a.map(x => Array.isArray(x)? flatten(x): [x])
    .reduce((a, b) => a.concat(b), []);

var runInfer = (e, env_) => {
  var env = makeEnv(env_);
  env.constraints = [];
  env.constraintsl = 0;
  var t = prune(infer(env, e));
  var solved = handleConstraints(env.constraints);
  E.each(e => {
    e.meta.type = prune(e.meta.type);
    e.meta.inst = flatten(
      e.meta.inst.map(i => solved[env.constraints.indexOf(i)])
    );
  }, e);
  return t;
};

var makeEnv = env_ => {
  var env = env_ || {};
  env.typings = env.typings || {};
  env.types = env.types || {};
  env.cases = env.cases || {};
  env.impl = env.impl || {};
  return env;
};

module.exports = {
  infer: runInfer,
  makeEnv,
  generalize,

  tapp,
  tarr,
  trow,
  trec,
  treco,
};

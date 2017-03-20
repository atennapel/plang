var E = require('./exprs');
var U = require('./utils');

var id = 0;
var uniq = () => '_u' + (id++);

function free(e) {
  if(e.tag === E.Var)
    return U.set(e.name);
  if(e.tag === E.App)
    return U.union(free(e.left), free(e.right));
  if(e.tag === E.Lam)
    return U.without(free(e.body), U.map(e.arg, true));
  if(e.tag === E.Let)
    return U.union(free(e.val), U.without(free(e.body), U.map(e.arg, true)));
  if(e.tag === E.Letr)
    return U.without(U.union(free(e.val), free(e.body)), U.map(e.arg, true));
  if(e.tag === E.If)
    return U.union(e.cond, U.union(e.bodyTrue, e.bodyFalse));
  return {};
}

function alphacR(a, b, e) {
  if(e.tag === E.Var)
    return e.arg === a? E.vr(b): e;
  if(e.tag === E.App)
    return E.app(alphac(a, b, e.left), alphac(a, b, e.right));
  if(e.tag === E.Lam) {
    if(e.arg === a) {
      var name = uniq();
      return E.lam(name, alphac(e.arg, name, e.body));
    }
    return E.lam(e.arg, alphac(a, b, e.body));
  }
  if(e.tag === E.Let) {
    if(e.arg === a) {
      var name = uniq();
      return E.lt(name, alphac(a, b, e.val), alphac(e.arg, name, e.body));
    }
    return E.lt(e.arg, alphac(a, b, e.val), alphac(a, b, e.body));
  }
  if(e.tag === E.Letr) {
    if(e.arg === a) {
      var name = uniq();
      return E.ltr(name,
        alphac(e.arg, name, e.body),
        alphac(e.arg, name, e.body)
      );
    }
    return E.ltr(e.arg, alphac(a, b, e.val), alphac(a, b, e.body));
  }
  if(e.tag === E.If)
    return E.iff(
      alphac(a, b, e.cond),
      alphac(a, b, e.bodyTrue),
      alphac(a, b, e.bodyFalse)
    );
  return {};
}

function alphac(a, b, e) {
  var ne = alphacR(a, b, e);
  ne.meta.type = e.meta.type;
  return ne;
}

function substR(v, x, e) {
  if(e.tag === E.Var)
    return e.name === v? x: e;
  if(e.tag === E.App)
    return E.app(subst(v, x, e.left), subst(v, x, e.right));
  if(e.tag === E.Lam) {
    if(v !== e.arg) {
      if(!free(x)[e.arg])
        return E.lam(e.arg, subst(v, x, e.body));
      return substR(v, x, alphac(e.arg, uniq(), e));
    }
    return e;
  }
  if(e.tag === E.Let)
    return E.lt(e.arg, subst(v, x, e.val), subst(v, x, e.body));
  if(e.tag === E.Letr)
    return E.ltr(e.arg, subst(v, x, e.val), subst(v, x, e.body));
  if(e.tag === E.If)
    return E.iff(
      subst(v, x, e.cond),
      subst(v, x, e.bodyTrue),
      subst(v, x, e.bodyFalse)
    );

  if(e.tag === E.TypeOf)
    return e;

  if(e.tag === E.Str)
    return e;

  throw new Error('Cannot subst over ' + e);
}

function subst(v, x, e) {
  var ne = substR(v, x, e);
  ne.meta.type = e.meta.type;
  return ne;
}

function betareduceR(e) {
  if(e.tag === E.App) {
    if(e.left.tag === E.Lam)
      return betareduce(
        subst(e.left.arg, betareduce(e.right), betareduce(e.left.body)));
    return E.app(betareduce(e.left), betareduce(e.right));
  }
  if(e.tag === E.Lam)
    return E.lam(e.arg, betareduce(e.body));
  if(e.tag === E.Let)
    return E.lt(e.arg, betareduce(e.val), betareduce(e.body));
  if(e.tag === E.Letr)
    return E.ltr(e.arg, betareduce(e.val), betareduce(e.body));
  if(e.tag === E.If)
    return E.iff(
      betareduce(e.cond),
      betareduce(e.bodyTrue),
      betareduce(e.bodyFalse)
    );

  return e;
}

function betareduce(e) {
  var ne = betareduceR(e);
  ne.meta.type = e.meta.type;
  return ne;
}

module.exports = {
  betareduce,
}

var E = require('./exprs');
var T = require('./types');
var U = require('./utils');
var tc = require('./typechecker');

function getSubDicts(d) {
  var a =
    U.flatten(
      U.keys(d).sort().map(v => U.keys(d[v]).sort().map(c => getDicts(d[v][c])))
    );
  return a;
}

function getDicts(d) {
  if(d.children)
    return E.app.apply(null, [E.vr(d.name)].concat(getSubDicts(d.children)));
  return E.vr(d.name);
}

function tctransform(e, dicts_) {
  // console.log(E.toString(e) + ' : ' + T.toString(e.meta.type));

  var dicts = dicts_ || {};

  var cs = tc.collectClasses(e.meta.type);
  var ds = U.flatten(U.keys(cs).map(v => cs[v].sort().map(c => {
    if(!dicts[v]) dicts[v] = {};
    if(!dicts[v][c]) {
      var name = '_D_' + v + '_' + c;
      dicts[v][c] = {name};
      return name;
    }
    return null;
  }))).filter(x => x);

  var ne;
  if(e.tag === E.Var) {
    if(e.meta.classes) {
      var a = U.flatten(U.keys(e.meta.classes).sort()
                .map(v => e.meta.classes[v].sort().map(c =>
                  dicts[v] && dicts[v][c] && getDicts(dicts[v][c]) || null)
                  .filter(x => x)));
      ne = E.app.apply(null, [e].concat(a));
    } else {
      ne = e;
    }
  }
  else if(e.tag === E.App)
    ne = E.app(tctransform(e.left, dicts), tctransform(e.right, dicts));
  else if(e.tag === E.Lam)
    ne = E.lam(e.arg, tctransform(e.body, dicts));
  else if(e.tag === E.Let)
    ne = E.lt(e.arg, tctransform(e.val, dicts), tctransform(e.body, dicts));
  else if(e.tag === E.Letr)
    ne = E.ltr(e.arg, tctransform(e.val, dicts), tctransform(e.body, dicts));
  else if(e.tag === E.Do)
    ne = E.doo(e.arg, tctransform(e.val, dicts), tctransform(e.body, dicts));
  else if(e.tag === E.If)
    ne = E.iff(
      tctransform(e.cond, dicts),
      tctransform(e.bodyTrue, dicts),
      tctransform(e.bodyFalse, dicts)
    );
  else if(e.tag === E.Anno)
    ne = tctransform(e.expr, dicts);
  else ne = e;

  if(ds.length > 0)
    ne = E.lam.apply(null, ds.concat(ne));

  ne.meta.type = e.meta.type;

  return ne;
}

module.exports = {
  tctransform,
};

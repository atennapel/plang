var E = require('./exprs');
var T = require('./types');
var U = require('./utils');
var tc = require('./typechecker');

function tctransform(e, dicts_, sub) {
  // console.log(E.toString(e) + ' : ' + T.toString(e.type));

  var dicts = dicts_ || {};

  var cs = tc.collectClasses(e.type);
  var ds = U.flatten(U.keys(cs).map(v => cs[v].sort().map(c => {
    if(!dicts[v]) dicts[v] = {};
    if(!dicts[v][c]) {
      var name = '_D_' + v + '_' + c;
      dicts[v][c] = name;
      return name;
    }
    return null;
  }))).filter(x => x);

  var ne;
  if(e.tag === E.App)
    ne = E.app(tctransform(e.left, dicts, sub), tctransform(e.right, dicts, sub));
  else if(e.tag === E.Lam)
    ne = E.lam(e.arg, tctransform(e.body, dicts, sub));
  else if(e.tag === E.Let)
    ne = E.lt(e.arg, tctransform(e.val, dicts, sub), tctransform(e.body, dicts, sub));
  else if(e.tag === E.Letr)
    ne = E.ltr(e.arg, tctransform(e.val, dicts, sub), tctransform(e.body, dicts, sub));
  else if(e.tag === E.Do)
    ne = E.doo(e.arg, tctransform(e.val, dicts, sub), tctransform(e.body, dicts, sub));
  else if(e.tag === E.If)
    ne = E.iff(
      tctransform(e.cond, dicts, sub),
      tctransform(e.bodyTrue, dicts, sub),
      tctransform(e.bodyFalse, dicts, sub)
    );
  else if(e.tag === E.Anno)
    ne = tctransform(e.expr, dicts, sub);
  else ne = e;

  if(e.classes) {
    var vc = U.flatten(U.keys(e.classes).sort()
      .map(v => e.classes[v].sort().map(c => dicts[checkSub(v, sub)][c])));
    ne = E.app.apply(null, [ne].concat(vc.map(E.vr)));
  }

  if(ds.length > 0)
    ne = E.lam.apply(null, ds.concat(ne));

  ne = E.setType(ne, e.type);

  return ne;
}

function checkSub(v, sub) {
  if(sub[v] && sub[v].tag === T.TVar)
    return sub[v].id;
  return v;
}

module.exports = {
  tctransform,
};

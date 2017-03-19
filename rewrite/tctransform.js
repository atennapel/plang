var E = require('./exprs');
var T = require('./types');
var U = require('./utils');
var tc = require('./typechecker');

function tctransform(e, dicts_) {
  console.log(E.toString(e) + ' : ' + T.toString(e.type));

  var dicts = dicts_ || {};

  var csi = [];
  if(e.dicts) {
    for(var v in e.dicts) {
      if(!dicts[v]) dicts[v] = {};
      for(var c in e.dicts[v]) {
        if(!dicts[v][c]) dicts[v][c] = {};
        dicts[v][c] = e.dicts[v][c];
      }
    }
    csi = U.flatten(U.keys(e.dicts)
      .sort().map(v => U.keys(e.dicts[v]).sort().map(c => e.dicts[v][c])));
  }

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
  if(e.tag === E.App) {
    var csl = tc.collectClasses(e.left.type);
    var dsl = U.flatten(U.keys(csl).map(v => csl[v].sort().map(c => {
      return dicts[v] && dicts[v][c] || null;
    }))).filter(x => x);
    ne = E.app.apply(null,
      [tctransform(e.left, dicts)]
      .concat(dsl.map(E.vr))
      .concat([tctransform(e.right, dicts)]));
  }
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

  if(csi.length > 0)
    ne = E.app.apply(null, [ne].concat(csi.map(E.vr)));
  if(ds.length > 0)
    ne = E.lam.apply(null, ds.concat(ne));

  ne = E.setType(ne, e.type);

  return ne;
}

module.exports = {
  tctransform,
};

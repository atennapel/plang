var E = require('./exprs');
var T = require('./types');
var U = require('./utils');
var tc = require('./typechecker');

var id = 0;
var uniq = function(name) {return '_' + (name || 'u') + (id++)};

function compile(e, dicts_) {
  console.log(E.toString(e) + ' : ' + T.toString(e.type));
  var dicts = dicts_ || {};
  if(e.tag === E.Var)
    return e.name;
  if(e.tag === E.App) {
    console.log(dicts);
    var cs = tc.collectClasses(e.left.type);
    var ds = U.flatten(U.keys(cs).sort().map(v => cs[v].map(c => {
      console.log(v, c);
      if(dicts[v] && dicts[v][c]) return dicts[v][c];
      var is = e.dicts || {};
      if(is[v] && is[v][c]) return is[v][c];
      return null;
    }).map(x => x)));
    var is = e.dicts || {};
    var iss = U.flatten(U.keys(is).sort().map(v => U.keys(is[v]).sort().map(c => is[v][c])));
    return '(' + compile(e.left, dicts) + ')' +
      (ds.length === 0? '': ds.map(x => '(' + x + ')').join('')) +
      (iss.length === 0? '': iss.map(x => '(' + x + ')').join('')) +
      '(' + compile(e.right, dicts) + ')';
  }
  if(e.tag === E.Lam) {
    var cs = tc.collectClasses(e.type);
    var ds =
      U.flatten(U.keys(cs).sort().map(v => cs[v].map(c => {
        if(!dicts[v]) dicts[v] = {};
        if(!dicts[v][c]) return dicts[v][c] = '_' + v + '_' + c;
        return null;
      }).filter(x => x)));
    console.log(ds);
    return (ds.length === 0? '':
      ds.join(' => ') + ' => ') +
      e.arg + ' => (' + compile(e.body, dicts) + ')';
  }
  if(e.tag === E.Let || e.tag === E.Letr)
    return '(function() {var ' + e.arg + '=(' + compile(e.val, dicts) + ');return(' +
      compile(e.body, dicts) + ')})()';
  if(e.tag === E.Do)
    return '_do(' + compile(e.val, dicts) + ', ' +
      e.arg + ' => ' + compile(e.body, dicts) + ')';
  if(e.tag === E.If)
    return '(' + compile(e.cond, dicts) + ' ? ' +
      compile(e.bodyTrue, dicts) + ' : ' +
      compile(e.bodyFalse, dicts) + ')';
  if(e.tag === E.TypeOf)
    return '(' + JSON.stringify(e.type) + ')';

  if(e.tag === E.Anno)
    return compile(e.expr, dicts);

  if(e.tag === E.RecordEmpty)
    return '_unit';

  if(e.tag === E.Select)
    return '_select(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.Extend)
    return '_extend(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.Restrict)
    return '_restrict(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.RecordUpdate)
    return '_recordupdate(' + JSON.stringify(e.label) + ')';

  if(e.tag === E.Inject)
    return '_inject(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.Embed)
    return '_embed(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.Elim)
    return '_elim(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.VariantUpdate)
    return '_variantupdate(' + JSON.stringify(e.label) + ')';

  if(e.tag === E.Handle)
    return '_handle(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.HandleReturn)
    return '_handlereturn';

  if(e.tag === E.End)
    return '_end';
  if(e.tag === E.Pure)
    return '_pure';
  if(e.tag === E.Return)
    return '_return';

  if(e.tag === E.Pack)
    return '_pack(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.Unpack)
    return '_unpack(' + JSON.stringify(e.label) + ')';

  if(e.tag === E.Perform)
    return '_perform(' + JSON.stringify(e.label) + ')';

  throw new Error('Cannot compile ' + e);
}

function compileWithLib(e) {
  var ce = compile(e);
  console.log(ce);
  return require('fs').readFileSync('lib.js', {encoding: 'utf8'}) + ce;
}

module.exports = {
  compile,
  compileWithLib,
};

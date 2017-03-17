var E = require('./exprs');

var id = 0;
var uniq = function() {return '_u' + (id++)};

function compile(e) {
  if(e.tag === E.Var)
    return e.name;
  if(e.tag === E.App)
    return '(' + compile(e.left) + ')(' + compile(e.right) + ')';
  if(e.tag === E.Lam)
    return '(function(' + e.arg + ') {return(' + compile(e.body) + ')})';
  if(e.tag === E.Let || e.tag === E.Letr)
    return '(function() {var ' + e.arg + '=(' + compile(e.val) + ');return(' +
      compile(e.body) + ')})()';
  if(e.tag === E.Do)
    return '_do(' + compile(e.val) + ', ' +
      e.arg + ' => ' + compile(e.body) + ')';
  if(e.tag === E.If)
    return '(' + compile(e.cond) + ' ? ' +
      compile(e.bodyTrue) + ' : ' +
      compile(e.bodyFalse) + ')';
  if(e.tag === E.TypeOf)
    return '(' + JSON.stringify(e.type) + ')';

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
  return require('fs').readFileSync('lib.js', {encoding: 'utf8'}) + compile(e);
}

module.exports = {
  compile,
  compileWithLib,
};

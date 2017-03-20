var E = require('./exprs');
var T = require('./types');
var U = require('./utils');
var tc = require('./typechecker');

var id = 0;
var uniq = function(name) {return '_' + (name || 'u') + (id++)};

function compile(e) {
  if(e.tag === E.Var)
    return e.name;
  if(e.tag === E.App)
    return '(' + compile(e.left) + ')' + '(' + compile(e.right) + ')';
  if(e.tag === E.Lam)
    return e.arg + ' => (' + compile(e.body) + ')';
  if(e.tag === E.Let || e.tag === E.Letr)
    return '(function() {var ' + e.arg + '=(' + compile(e.val) + ');return(' +
      compile(e.body) + ')})()';
  if(e.tag === E.If)
    return '(' + compile(e.cond) + ' ? ' +
      compile(e.bodyTrue) + ' : ' +
      compile(e.bodyFalse) + ')';

  if(e.tag === E.TypeOf)
    return '(' + JSON.stringify(e.meta.type) + ')';

  if(e.tag === E.Str)
    return JSON.stringify(e.val);

  throw new Error('Cannot compile ' + e);
}

function compileWithLib(e, log) {
  var ce = compile(e);
  if(log) console.log(ce);
  return require('fs').readFileSync('lib.js', {encoding: 'utf8'}) + ce;
}

module.exports = {
  compile,
  compileWithLib,
};

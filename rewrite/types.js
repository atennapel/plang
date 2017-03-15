var K = require('./kinds');
var U = require('./utils');

var terr = m => { throw new TypeError(m) };

var TVar = 'TVar';
var tvar = (id, kind, labels, value) => ({
  tag: TVar,
  id,
  labels: labels || {},
  kind: kind || K.Star,
  value: value || false,
});

var TCon = 'Con';
var tcon = (name, kind) => ({
  tag: TCon,
  name,
  kind: kind || K.Star,
});

var TApp = 'TApp';
var tapp2 = (left, right, kind_) => {
  var kind = kind_;
  if(!kind) {
    if(left.kind.tag !== K.KArr || !K.equals(left.kind.left, right.kind))
      terr('Cannot apply ' + toString(left) + ' to ' + toString(right));
    kind = left.kind.right;
  }
  return {
    tag: TApp,
    left,
    right,
    kind: kind || K.Star,
  };
};
var tapp = function() {
  var l = arguments.length;
  if(l < 2) terr('tapp needs at least two arguments');
  var c = tapp2(arguments[0], arguments[1]);
  for(var i = 2; i < l; i++) c = tapp2(c, arguments[i]);
  return c;
};

var TRowEmpty = 'TRowEmpty';
var trowempty = { tag: TRowEmpty, kind: K.Row };

var TRowExtend = 'TRowExtend';
var trowextend = (label, type, rest) => ({
  tag: TRowExtend,
  label,
  type,
  rest,
  kind: K.Row,
});

var trow = (o, v) => {
  var c = v || trowempty;
  for(var k in o) c = trowextend(k, o[k], c);
  return c;
};

var TArr = tcon('->', K.karr(K.Star, K.Star, K.Star));
var tarr2 = (left, right) => tapp(TArr, left, right);
var tarr = function() {
  var l = arguments.length;
  if(l < 1) T.terr('tarr needs at least 1 argument');
  if(l === 1) return tapp(TArr, arguments[0]);
  var c = tarr2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = tarr2(arguments[i], c);
  return c;
};

var TRecord = tcon('Rec', K.karr(K.Row, K.Star));
var TVariant = tcon('Var', K.karr(K.Row, K.Star));
var TEff = tcon('Eff', K.karr(K.Row, K.Star, K.Star));

var isUnit = t =>
  t.tag === TApp && t.left === TRecord && t.right.tag === TRowEmpty;
var isEff = t =>
  t.tag === TApp && t.left.tag === TApp && t.left.left === TEff;

var TScheme = 'TScheme';
var tscheme = (vars, type) => ({
  tag: TScheme,
  vars,
  type,
});

var Bool = tcon('Bool', K.Star);

var toString = t => {
  if(isUnit(t)) return '()';
  if(t.tag === TVar) {
    var labels = U.keys(t.labels);
    return (t.value? "'": '') + t.id +
      (labels.length? '/{' + labels.join(', ') + '}': '');
  }
  if(t.tag === TCon) return '' + t.name;
  if(t.tag === TApp) {
    if(t.left.tag === TApp && t.left.left.tag === TCon &&
      /[^a-z]/i.test(t.left.left.name[0]))
      return '(' + toString(t.left.right) + ' ' +
        t.left.left.name + ' ' + toString(t.right) + ')';
    return '(' + toString(t.left) + ' ' + toString(t.right) + ')';
  }
  if(t.tag === TScheme)
    return 'forall' + (t.vars.length?
        ' ' + t.vars.map(toString).join(' '):
        '') + ' . ' +
      toString(t.type);
  if(t.tag === TRowEmpty) return '{}';
  if(t.tag === TRowExtend)
    return '{' + t.label + ' : ' + toString(t.type) + ' | ' +
      toString(t.rest) + '}';
  terr('Invalid type tag in toString: ' + t.tag);
};

module.exports = {
  TVar,
  tvar,

  TCon,
  tcon,

  TApp,
  tapp,

  TRowEmpty,
  trowempty,

  TRowExtend,
  trowextend,

  trow,

  TArr,
  tarr,

  TRecord,
  TVariant,
  TEff,

  isUnit,
  isEff,

  TScheme,
  tscheme,

  Bool,

  terr,
  toString,
};

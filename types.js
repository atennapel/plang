var K = require('./kinds');

var terr = m => { throw new TypeError(m) };

var TVar = 'TVar';
var id = {};
var fresh = name => {
  if(!id[name]) id[name] = 0;
  return id[name]++;
};
var vars = {};
var tvar = (name_, kind, lacks, value) => {
  var name = name_ || 't';
  var _id = name + fresh(name);
  var v = {
    tag: TVar,
    name,
    id: _id,
    kind: kind || K.kvar(),
    lacks: lacks || {},
    value: value || false,
    instance: null,
  };
  vars[_id] = v;
  return v;
};
var getTVar = id => vars[id] || null;

var TCon = 'TCon';
var tcon = (name, kind) => ({
  tag: TCon,
  name,
  kind: kind || K.kvar(),
});

var TApp = 'TApp';
var tapp = (left, right, kind) => ({
  tag: TApp,
  left,
  right,
  kind: kind || K.kvar(),
});

var TRowEmpty = 'TRowEmpty';
var trowempty = {
  tag: TRowEmpty,
  kind: K.krow,
};

var TRowExtend = 'TRowExtend';
var trowextend = (label, type, rest) => ({
  tag: TRowExtend,
  label,
  type,
  rest,
  kind: K.krow,
});

var trow = (o, v) => {
  var c = v || trowempty;
  for(var k in o) c = trowextend(k, o[k], c);
  return c;
};

var TEff = tcon('Eff', K.karr(K.krow, K.kstar, K.kstar));
var TArr = tcon('->', K.karr(K.kstar, K.kstar, K.kstar));
var TRec = tcon('Rec', K.karr(K.krow, K.kstar));
var TUnit = tapp(TRec, trowempty, K.kstar);

var isUnit = t => t === TUnit ||
  (t.tag === TApp && t.left === TRec && t.right.tag === TRowEmpty);
var isEff = t =>
  t.tag === TApp && t.left.tag === TApp && t.left.left === TEff;

var TBool = tcon('Bool', K.kstar);
var TInt = tcon('Int', K.kstar);
var TFloat = tcon('Float', K.kstar);
var TStr = tcon('Str', K.kstar);
var TArray = tcon('Array', K.karr(K.kstar, K.kstar));

var tarr2 = (a, b) => tapp(tapp(TArr, a, K.karr(K.kstar, K.kstar)), b, K.kstar);
var tarr = function() {
  var l = arguments.length;
  if(l < 1) T.terr('tarr needs at least 1 argument');
  if(l === 1) return tapp(TArr, arguments[0]);
  var c = tarr2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = tarr2(arguments[i], c);
  return c;
};

var teff = (eff, type) =>
  tapp(tapp(TEff, eff, K.karr(K.kstar, K.kstar)), type, K.kstar);

var flattenTApp = t => {
  if(t.tag === TApp) {
    var left = flattenTApp(t.left);
    return {fn: left.fn, args: left.args.concat([t.right])};
  }
  return {fn: t, args: []}
};

var TScheme = 'TScheme';
var tscheme = (vars, type) => ({
  tag: TScheme,
  vars,
  type,
  kind: type.kind,
});

var effectToStringR = t =>
  t.tag === TRowExtend?
    [
      t.label + (isUnit(t.type)? '': ': ' + toString(t.type))
    ].concat(effectToStringR(t.rest)):[]

var effectToString = t =>
  t.tag !== TRowExtend? '':
  '{' + effectToStringR(t).join(', ') + '}';

var effectToStringWithSpace = t => {
  var r = effectToString(t);
  return r? r + ' ': r;
};

var rowToString = (row, first) =>
  row.tag === TRowExtend?
    (first? '': ', ') + row.label + ' : ' + toString(row.type) +
      rowToString(row.rest):
  row.tag === TRowEmpty? '':
  ' | ' + toString(row);
var tappToString = (left, right, toplevel) =>
  left === TRec && right.tag === TRowEmpty? '()':
  left.tag === TApp && left.left.tag === TCon &&
    /[^a-z0-9]/i.test(left.left.name[0])?
    (toplevel? '': '(') +
      (left.right.tag === TApp?
        tappToString(left.right.left, left.right.right, false):
        toString(left.right)) +
      ' ' + toString(left.left) + ' ' + toString(right) +
    (toplevel? '': ')'):
  (toplevel? '': '(') +
    (left.tag === TApp? tappToString(left.left, left.right, true):
      toString(left)) + ' ' +
    (right.tag === TApp? tappToString(right.left, right.right, false):
      toString(right)) +
  (toplevel? '': ')');
var toString = t =>
  isUnit(t)? '()':
  t.tag === TVar?
    (t.value? "'": '') +
    (t.id[t.id.length - 1] === '0'? t.id.slice(0, t.id.length - 1): t.id)
    + (Object.keys(t.lacks).length === 0?
      '':
      '/{' + Object.keys(t.lacks).join(', ') + '}'):
  t.tag === TCon? t.name:
  t.tag === TApp? tappToString(t.left, t.right, true):
  t.tag === TRowEmpty? '{}':
  t.tag === TRowExtend? '{' + rowToString(t, true) + '}':
  t.tag === TScheme? 'forall ' + t.vars.map(toString).join(' ') +
    ' . ' + toString(t.type):
  terr('Not a type tag: ' + t.tag);

module.exports = {
  TVar,
  tvar,
  getTVar,

  TCon,
  tcon,

  TApp,
  tapp,

  TRowEmpty,
  trowempty,

  TRowExtend,
  trowextend,

  trow,

  TUnit,
  isUnit,

  TArr,
  tarr,

  TEff,
  teff,
  isEff,

  TRec,
  TBool,
  TInt,
  TFloat,
  TStr,
  TArray,

  flattenTApp,

  TScheme,
  tscheme,

  terr,
  toString,
  effectToString,
};

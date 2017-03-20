var T = require('./types');

var serr = m => { throw new SyntaxError(m) };

var Var = 'Var';
var vr = name => ({
  tag: Var,
  name,
  meta: {},
});

var App = 'App';
var app2 = (left, right) => ({
  tag: App,
  left,
  right,
  meta: {},
});
var app = function() {
  var l = arguments.length;
  if(l === 1) return arguments[0];
  if(l < 2) serr('app needs at least one argument');
  var c = app2(arguments[0], arguments[1]);
  for(var i = 2; i < l; i++) c = app2(c, arguments[i]);
  return c;
};

var Lam = 'Lam';
var lam2 = (arg, body) => ({
  tag: Lam,
  arg,
  body,
  meta: {},
});
var lam = function() {
  var l = arguments.length;
  if(l < 2) serr('lam needs at least 2 arguments');
  var c = lam2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = lam2(arguments[i], c);
  return c;
};

var Let = 'Let';
var lt = (arg, val, body) => ({
  tag: Let,
  arg,
  val,
  body,
  meta: {},
});

var Letr = 'Letr';
var ltr = (arg, val, body) => ({
  tag: Letr,
  arg,
  val,
  body,
  meta: {},
});

var Do = 'Do';
var doo = (arg, val, body) => ({
  tag: Do,
  arg,
  val,
  body,
  meta: {},
});

var If = 'If';
var iff = (cond, bodyTrue, bodyFalse) => ({
  tag: If,
  cond,
  bodyTrue,
  bodyFalse,
  meta: {},
});

var RecordEmpty = 'RecordEmpty';
var recordempty = () => ({
  tag: RecordEmpty,
  meta: {},
});

var Select = 'Select';
var select = label => ({
  tag: Select,
  label,
  meta: {},
});

var Extend = 'Extend';
var extend = label => ({
  tag: Extend,
  label,
  meta: {},
});

var Restrict = 'Restrict';
var restrict = label => ({
  tag: Restrict,
  label,
  meta: {},
});

var RecordUpdate = 'RecordUpdate';
var recordupdate = label => ({
  tag: RecordUpdate,
  label,
  meta: {},
});

var Inject = 'Inject';
var inject = label => ({
  tag: Inject,
  label,
  meta: {},
});

var Embed = 'Embed';
var embed = label => ({
  tag: Embed,
  label,
  meta: {},
});

var Elim = 'Elim';
var elim = label => ({
  tag: Elim,
  label,
  meta: {},
});

var VariantUpdate = 'VariantUpdate';
var variantupdate = label => ({
  tag: VariantUpdate,
  label,
  meta: {},
});

var Handle = 'Handle';
var handle = label => ({
  tag: Handle,
  label,
  meta: {},
});

var HandleReturn = 'HandleReturn';
var handlereturn = () => ({
  tag: HandleReturn,
  meta: {},
});

var End = 'End';
var end = () => ({
  tag: End,
  meta: {},
});

var Pure = 'Pure';
var pure = () => ({
  tag: Pure,
  meta: {},
});

var Return = 'Return';
var retrn = () => ({
  tag: Return,
  meta: {},
});

var Unpack = 'Unpack';
var unpack = label => ({
  tag: Unpack,
  label,
  meta: {},
});

var Pack = 'Pack';
var pack = label => ({
  tag: Pack,
  label,
  meta: {},
});

var Perform = 'Perform';
var perform = label => ({
  tag: Perform,
  label,
  meta: {},
});

var TypeOf = 'TypeOf';
var typeOf = expr => ({
  tag: TypeOf,
  expr,
  meta: {},
});

var Anno = 'Anno';
var anno = (expr, decltype) => ({
  tag: Anno,
  expr,
  decltype,
  meta: {},
});

var Str = 'Str';
var str = val => ({
  tag: Str,
  val,
});

var each = (f, e) =>
  e.tag === App? (each(f, e.left), each(f, e.right), f(e)):
  e.tag === Lam? (each(f, e.body), f(e)):
  e.tag === Let? (each(f, e.val), each(f, e.body), f(e)):
  e.tag === Letr? (each(f, e.val), each(f, e.body), f(e)):
  e.tag === Do? (each(f, e.val), each(f, e.body), f(e)):
  e.tag === If?
    (each(f, e.cond), each(f, e.bodyTrue), each(f, e.bodyFalse), f(e)):
  e.tag === TypeOf? (each(f, e.expr), f(e)):
  e.tag === Anno? (each(f, e.expr), f(e)):
  f(e);

var toString = e => {
  if(e.tag === Var) return '' + e.name;
  if(e.tag === App)
    return '(' + toString(e.left) + ' ' + toString(e.right) + ')';
  if(e.tag === Lam)
    return '(\\' + e.arg + ' -> ' + toString(e.body) + ')';
  if(e.tag === Let)
    return '(let ' + e.arg + ' = ' + toString(e.val) + ' in ' +
      toString(e.body) + ')';
  if(e.tag === Letr)
    return '(letr ' + e.arg + ' = ' + toString(e.val) + ' in ' +
      toString(e.body) + ')';
  if(e.tag === Do)
    return '(' + e.arg + ' <- ' + toString(e.val) + '; ' +
      toString(e.body) + ')';
  if(e.tag === If)
    return '(if ' + toString(e.cond) + ' then ' + toString(e.bodyTrue) +
      ' else ' + toString(e.bodyFalse) + ')';
  if(e.tag === TypeOf)
    return '(typeof ' + toString(e.expr) + ')';

  if(e.tag === Anno)
    return '(' + toString(e.expr) + ' : ' + T.toString(e.decltype) + ')';

  if(e.tag === RecordEmpty) return '{}';
  if(e.tag === Select) return '.' + e.label;
  if(e.tag === Extend) return '.+' + e.label;
  if(e.tag === Restrict) return '.-' + e.label;
  if(e.tag === RecordUpdate) return '.=' + e.label;

  if(e.tag === Inject) return '@' + e.label;
  if(e.tag === Embed) return '@+' + e.label;
  if(e.tag === Elim) return '?' + e.label;
  if(e.tag === VariantUpdate) return '@=' + e.label;

  if(e.tag === Handle) return 'handle ' + e.label;
  if(e.tag === HandleReturn) return 'handlereturn';

  if(e.tag === End) return 'end';
  if(e.tag === Pure) return 'pure';
  if(e.tag === Return) return 'return';

  if(e.tag === Pack) return 'pack ' + e.label;
  if(e.tag === Unpack) return 'unpack ' + e.label;

  if(e.tag === Perform) return '!' + e.label;

  if(e.tag === Str) return JSON.stringify(e.val);

  serr('Invalid expression tag toString: ' + e.tag);
};

module.exports = {
  Var,
  vr,

  App,
  app,

  Lam,
  lam,

  Let,
  lt,

  Letr,
  ltr,

  Do,
  doo,

  If,
  iff,

  RecordEmpty,
  recordempty,
  Select,
  select,
  Extend,
  extend,
  Restrict,
  restrict,
  RecordUpdate,
  recordupdate,

  Inject,
  inject,
  Embed,
  embed,
  Elim,
  elim,
  VariantUpdate,
  variantupdate,

  Handle,
  handle,

  HandleReturn,
  handlereturn,

  End,
  end,

  Pure,
  pure,

  Return,
  retrn,

  Pack,
  pack,

  Unpack,
  unpack,

  Perform,
  perform,

  TypeOf,
  typeOf,

  Anno,
  anno,

  Str,
  str,

  each,
  serr,
  toString,
};

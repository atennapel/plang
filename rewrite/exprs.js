var serr = m => { throw new SyntaxError(m) };

var Var = 'Var';
var vr = name => ({
  tag: Var,
  name,
});

var App = 'App';
var app2 = (left, right) => ({
  tag: App,
  left,
  right,
});
var app = function() {
  var l = arguments.length;
  if(l < 2) serr('app needs at least two arguments');
  var c = app2(arguments[0], arguments[1]);
  for(var i = 2; i < l; i++) c = app2(c, arguments[i]);
  return c;
};

var Lam = 'Lam';
var lam2 = (arg, body) => ({
  tag: Lam,
  arg,
  body,
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
});

var RecordEmpty = 'RecordEmpty';
var recordempty = { tag: RecordEmpty };

var Select = 'Select';
var select = label => ({
  tag: Select,
  label,
});

var Extend = 'Extend';
var extend = label => ({
  tag: Extend,
  label,
});

var Restrict = 'Restrict';
var restrict = label => ({
  tag: Restrict,
  label,
});

var RecordUpdate = 'RecordUpdate';
var recordupdate = label => ({
  tag: RecordUpdate,
  label,
});

var Inject = 'Inject';
var inject = label => ({
  tag: Inject,
  label,
});

var Embed = 'Embed';
var embed = label => ({
  tag: Embed,
  label,
});

var Elim = 'Elim';
var elim = label => ({
  tag: Elim,
  label,
});

var VariantUpdate = 'VariantUpdate';
var variantupdate = label => ({
  tag: VariantUpdate,
  label,
});

var End = 'End';
var end = { tag: End };

var Unpack = 'Unpack';
var unpack = label => ({
  tag: Unpack,
  label,
});

var Pack = 'Pack';
var pack = label => ({
  tag: Pack,
  label,
});

var toString = e => {
  if(e.tag === Var) return '' + e.name;
  if(e.tag === App)
    return '(' + toString(e.left) + ' ' + toString(e.right) + ')';
  if(e.tag === Lam)
    return '(\\' + e.arg + ' -> ' + toString(e.body) + ')';
  if(e.tag === Let)
    return '(let ' + e.arg + ' = ' + toString(e.val) + ' in ' +
      toString(e.body) + ')';

  if(e.tag === RecordEmpty) return '{}';
  if(e.tag === Select) return '.' + e.label;
  if(e.tag === Extend) return '.+' + e.label;
  if(e.tag === Restrict) return '.-' + e.label;
  if(e.tag === RecordUpdate) return '.=' + e.label;

  if(e.tag === Inject) return '@' + e.label;
  if(e.tag === Embed) return '@+' + e.label;
  if(e.tag === Elim) return '?' + e.label;
  if(e.tag === VariantUpdate) return '@=' + e.label;

  if(e.tag === End) return 'end';

  if(e.tag === Pack) return 'pack ' + e.label;
  if(e.tag === Unpack) return 'unpack ' + e.label;

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

  End,
  end,

  Pack,
  pack,

  Unpack,
  unpack,

  serr,
  toString,
};

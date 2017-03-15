var serr = m => { throw new SyntaxError(m) };

var Var = 'Var';
var vr = name => ({
  tag: Var,
  name,
});

var App = 'App';
var app = (left, right) => ({
  tag: App,
  left,
  right,
});

var Lam = 'Lam';
var lam = (arg, body) => ({
  tag: Lam,
  arg,
  body,
});

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

  if(e.tag === Inject) return '@' + e.label;
  if(e.tag === Embed) return '@+' + e.label;
  if(e.tag === Elim) return '?' + e.label;

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

  Inject,
  inject,
  Embed,
  embed,
  Elim,
  elim,

  serr,
  toString,
};

var T = require('./types');

var serr = m => { throw new SyntaxError(m) };

var Var = 'Var';
var vr = (name, meta) => ({
  tag: Var,
  name,
  meta: meta || {},
});

var App = 'App';
var app2 = (left, right, meta) => ({
  tag: App,
  left,
  right,
  meta: meta || {},
});
var app = function() {
  var l = arguments.length;
  if(l < 2) serr('app needs at least two arguments');
  var c = app2(arguments[0], arguments[1]);
  for(var i = 2; i < l; i++) c = app2(c, arguments[i]);
  return c;
};
var iapp = function() {
  var l = arguments.length;
  if(l < 2) serr('app needs at least two arguments');
  var c = app2(arguments[0], arguments[1], {impl: true});
  for(var i = 2; i < l; i++) c = app2(c, arguments[i], {impl: true});
  return c;
};
var eapp = function() {
  var l = arguments.length;
  if(l < 2) serr('app needs at least two arguments');
  var c = app2(arguments[0], arguments[1], {expl: true});
  for(var i = 2; i < l; i++) c = app2(c, arguments[i], {expl: true});
  return c;
};

var Lam = 'Lam';
var lam2 = (arg, body, meta) => ({
  tag: Lam,
  arg,
  body,
  meta: meta || {},
});
var lam = function() {
  var l = arguments.length;
  if(l < 2) serr('lam needs at least 2 arguments');
  var c = lam2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = lam2(arguments[i], c);
  return c;
};
var ilam = function() {
  var l = arguments.length;
  if(l < 2) serr('lam needs at least 2 arguments');
  var c = lam2(arguments[l - 2], arguments[l - 1], {impl: true});
  for(var i = l - 3; i >= 0; i--) c = lam2(arguments[i], c, {impl: true});
  return c;
};

var Let = 'Let';
var lete3 = (arg, val, body, meta) => ({
  tag: Let,
  arg,
  val,
  body,
  meta: meta || {},
});
function lete() {
  if(arguments.length < 3) serr('lete needs at least 3 arguments');
  var l = arguments.length;
  var c = lete3(arguments[l-3], arguments[l-2], arguments[l-1]);
  for(var i = l-4; i >= 0; i -= 2) c = lete3(arguments[i-1], arguments[i], c);
  return c;
};
function letr() {
  if(arguments.length < 3) serr('letr needs at least 3 arguments');
  var l = arguments.length;
  var c = lete3(arguments[l-3], arguments[l-2], arguments[l-1],
    {recursive: true});
  for(var i = l-4; i >= 0; i -= 2)
    c = lete3(arguments[i-1], arguments[i], c, {recursive: true});
  return c;
};
function ilet() {
  if(arguments.length < 3) serr('lete needs at least 3 arguments');
  var l = arguments.length;
  var c = lete3(arguments[l-3], arguments[l-2], arguments[l-1], {impl: true});
  for(var i = l-4; i >= 0; i -= 2)
    c = lete3(arguments[i-1], arguments[i], c, {impl: true});
  return c;
};
function iletr() {
  if(arguments.length < 3) serr('letr needs at least 3 arguments');
  var l = arguments.length;
  var c = lete3(arguments[l-3], arguments[l-2], arguments[l-1],
    {recursive: true, impl: true});
  for(var i = l-4; i >= 0; i -= 2)
    c = lete3(arguments[i-1], arguments[i], c, {recursive: true, impl: true});
  return c;
};
function doe() {
  if(arguments.length < 3) serr('doe needs at least 3 arguments');
  var l = arguments.length;
  var c = lete3(arguments[l-3], arguments[l-2], arguments[l-1], {effect: true});
  for(var i = l-4; i >= 0; i -= 2)
    c = lete3(arguments[i-1], arguments[i], c, {effect: true});
  return c;
};

var Type = 'Type';
var type = (name, args, cases, body, meta) => ({
  tag: Type,
  name,
  args,
  cases,
  body,
  meta: meta || {},
});

var If = 'If';
var iff3 = (cond, bodyTrue, bodyFalse, meta) => ({
  tag: If,
  cond,
  bodyTrue,
  bodyFalse,
  meta: meta || {},
});
var iff = function() {
  if(arguments.length % 2 === 0 || arguments.length < 3)
    serr('iff needs at least 3 arguments and an uneven amount of arguments');
  var c = arguments[arguments.length - 1]
  for(var a = arguments, i = a.length - 2; i >= 0; i -= 2)
    c = iff3(a[i - 1], a[i], c);
  return c;
};

var Handle = 'Handle';
var handle = (map, meta) => ({
  tag: Handle,
  map,
  meta: meta || {},
});

var Case = 'Case';
var casee = (map, meta) => ({
  tag: Case,
  map,
  meta: meta || {},
});

var Anno = 'Anno';
var anno = (expr, type, meta) => ({
  tag: Anno,
  expr,
  type,
  meta: meta || {},
});

var Record = 'Record';
var record = (map, meta) => ({
  tag: Record,
  map,
  meta: meta || {},
});
var reco = record;
var reca = function(a) {
  var l = a.length;
  if(l % 2 !== 0) serr('Invalid number of arguments for reca');
  var c = {};
  for(var i = 0; i < l; i += 2) c[a[i]] = a[i + 1];
  return record(c);
};
var rec = function() {return reca(arguments)};

var List = 'List';
var list = (arr, meta) => ({
  tag: List,
  arr,
  meta: meta || {},
});

var RecordEmpty = 'RecordEmpty';
var recordempty = meta => ({
  tag: RecordEmpty,
  meta: meta || {},
});

var RecordSelect = 'RecordSelect';
var recordselect = (label, meta) => ({
  tag: RecordSelect,
  label,
  meta: meta || {},
});

var RecordExtend = 'RecordExtend';
var recordextend = (label, meta) => ({
  tag: RecordExtend,
  label,
  meta: meta || {},
});

var RecordRestrict = 'RecordRestrict';
var recordrestrict = (label, meta) => ({
  tag: RecordRestrict,
  label,
  meta: meta || {},
});

var RecordUpdate = 'RecordUpdate';
var recordupdate = (label, meta) => ({
  tag: RecordUpdate,
  label,
  meta: meta || {},
});

var Perform = 'Perform';
var perform = (label, meta) => ({
  tag: Perform,
  label,
  meta: meta || {},
});

var Int = 'Int';
var int = (val, meta) => ({
  tag: Int,
  val,
  meta: meta || {},
});

var Float = 'Float';
var float = (val, meta) => ({
  tag: Float,
  val,
  meta: meta || {},
});

var Str = 'Str';
var str = (val, meta) => ({
  tag: Str,
  val,
  meta: meta || {},
});

var lamToString = (arg, body, toplevel) =>
  (toplevel? '(\\': '') + arg +
  (body.tag === Lam?
    ' ' + lamToString(body.arg, body.body, false):
    ' -> ' + toString(body)) +
  (toplevel? ')': '');
var appToString = (left, right, toplevel) =>
  (toplevel? '': '(') +
    (left.tag === App? appToString(left.left, left.right, true):
      toString(left)) + ' ' +
    (right.tag === App? appToString(right.left, right.right, false):
      toString(right)) +
  (toplevel? '': ')');
var toString = e =>
  e.tag === Var? e.name:
  e.tag === App?
    (e.meta.expl?
      '(' + toString(e.left) + ' {{' + toString(e.right) + '}})':
      e.meta.impl?
      '(' + toString(e.left) + ' {' + toString(e.right) + '})':
      '(' + toString(e.left) + ' ' + toString(e.right) + ')'):
  e.tag === Lam?
    e.meta.impl?
      '(\\' + e.arg + ' => ' + toString(e.body) + ')':
      lamToString(e.arg, e.body, true):
  e.tag === Let? '(' +
    (e.meta.effect? 'do ': (e.meta.impl? 'i': '') + 'let' +
      (e.meta.recursive? 'r ': ' ')) + e.arg +
    ' = ' + toString(e.val) + ' in ' + toString(e.body) + ')':
  e.tag === If?
    '(if ' + toString(e.cond) + ' then ' +
      toString(e.bodyTrue) + ' else ' +
      toString(e.bodyFalse) + ')':
  e.tag === Anno? '(' + toString(e.expr) + ' : ' + T.toString(e.type) + ')':
  e.tag === Record?
    '{' +
      Object.keys(e.map).map(k => k + ' = ' + toString(e.map[k])).join(', ') +
    '}':
  e.tag === Handle?
    'handle {' +
      Object.keys(e.map).map(k => k + ' -> ' + toString(e.map[k])).join(', ') +
    '}':
  e.tag === Case?
    'case {' +
      Object.keys(e.map).map(k => k + ' -> ' + toString(e.map[k])).join(', ') +
    '}':
  e.tag === Type?
    'type ' + e.name + (e.args.length > 0? ' ' +
      e.args.map(T.toString).join(' '): '') + ' = ' +
    Object.keys(e.cases)
      .map(k => k + ' ' +
        e.cases[k].map(t => '(' + T.toString(t) + ')').join(' ')).join(' | ') +
    ' in ' + toString(e.body):
  e.tag === List? '[' + e.arr.map(toString).join(', ') + ']':
  e.tag === RecordEmpty? '{}':
  e.tag === RecordSelect? '.' + e.label:
  e.tag === RecordExtend? '.+' + e.label:
  e.tag === RecordRestrict? '.-' + e.label:
  e.tag === RecordUpdate? ':' + e.label:
  e.tag === Perform? '!' + e.label:
  e.tag === Int? '' + e.val:
  e.tag === Float? '' + e.val:
  e.tag === Str? JSON.stringify(e.val):
  serr('Not a expr tag: ' + e.tag);

var eachRecord = (f, o) => { for(var k in o) f(o[k]) };
var each = (f, e) =>
  e.tag === App? (each(f, e.left), each(f, e.right), f(e)):
  e.tag === Lam? (each(f, e.body), f(e)):
  e.tag === Let? (each(f, e.val), each(f, e.body), f(e)):
  e.tag === If?
    (each(f, e.cond), each(f, e.bodyTrue), each(f, e.bodyFalse), f(e)):
  e.tag === Anno? (each(f, e.expr), f(e)):
  e.tag === Record? (eachRecord(f, e.map), f(e)):
  e.tag === List? (e.arr.forEach(x => each(f, x)), f(e)):
  e.tag === Handle? (eachRecord(f, e.map), f(e)):
  e.tag === Case? (eachRecord(f, e.map), f(e)):
  e.tag === Type? (each(f, e.body), f(e)):
  f(e);

var mapRecord = (f, o) => {
  var n = {};
  for(var k in o) n[k] = map(f, o[k]);
  return n;
};
var map = (f, e) =>
  e.tag === App? f(app2(map(f, e.left), map(f, e.right), e.meta)):
  e.tag === Lam? f(lam2(e.arg, map(f, e.body), e.meta)):
  e.tag === Let? f(lete3(e.arg, map(f, e.val), map(f, e.body), e.meta)):
  e.tag === If?
    f(iff3(map(f, e.cond), map(f, e.bodyTrue), map(f, e.bodyFalse), e.meta)):
  e.tag === Anno? f(anno(map(f, e.expr), e.type, e.meta)):
  e.tag === Type? f(type(e.name, e.args, e.cases, map(f, e.body), e.meta)):
  e.tag === Record? f(record(mapRecord(f, e.map), e.meta)):
  e.tag === List? f(list(e.arr.map(x => map(f, x)), e.meta)):
  e.tag === Handle? f(handle(mapRecord(f, e.map), e.meta)):
  e.tag === Case? f(handle(mapRecord(f, e.map), e.meta)):
  f(e);

module.exports = {
  Var,
  vr,

  App,
  app,
  iapp,
  eapp,

  Lam,
  lam,
  ilam,

  Let,
  lete,
  letr,
  ilet,
  iletr,
  doe,

  Type,
  type,

  If,
  iff,

  Handle,
  handle,

  Case,
  casee,

  Anno,
  anno,

  Record,
  record,
  reca,
  reco,
  rec,

  List,
  list,

  RecordEmpty,
  recordempty,
  RecordSelect,
  recordselect,
  RecordExtend,
  recordextend,
  RecordRestrict,
  recordrestrict,
  RecordUpdate,
  recordupdate,

  Perform,
  perform,

  Int,
  int,
  Float,
  float,
  Str,
  str,

  serr,
  toString,
  each,
  map,
};

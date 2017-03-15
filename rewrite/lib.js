var _id = function(x) {return x};
var _unit = {};
var _end = function(x) { throw new Error('Unhandled tag: ' + x.tag) };
var _select = function(label) {return function(x) {return x[label]}};
var _extend = function(label) {return function(v) {return function(x) {
  var n = {};
  for(var k in x) n[k] = x[k];
  n[label] = v;
  return n;
}}};
var _restrict = function(label) {return function(x) {
  var n = {};
  for(var k in x) if(k !== label) n[k] = x[k];
  return n;
}};
var _recordupdate = function(label) {return function(f) {return function(x) {
  var n = {};
  for(var k in x) n[k] = k === label? f(x[k]): x[k];
  return n;
}}};
var _inject = function(label) {return function(v) {
  return {tag: label, val: v};
}};
var _embed = function(label) {return _id};
var _elim = function(label) {return function(f1) {return function(f2) {
  return function(x) {
    return x.tag === label? f1(x.val): f2(x);
  };
}}};
var _variantupdate = function(label) {return function(f) {return function(x) {
  return x.tag === label? _inject(label)(f(x.val)): x;
}}};
var _pack = function(label) {return _id};
var _unpack = function(label) {return _id};

var True = true;
var False = false;

var zero = 0;
var one = 1;
var add = function(x) {return function(y) {return x + y}};

var set = v => {
  var n = {};
  n[v] = true;
  return n;
};

var setFrom = a => {
  var n = {};
  for(var i = 0, l = a.length; i < l; i++) n[a[i]] = true;
  return n;
};

var union = (a, b) => {
  var n = {};
  for(var k in a) n[k] = a[k];
  for(var k in b) n[k] = b[k];
  return n;
};

var without = (a, b) => {
  var n = {};
  for(var k in a) if(!b[k]) n[k] = a[k];
  return n;
};

var omap = (f, o) => {
  var n = {};
  for(var k in o) n[k] = f(o[k], k);
  return n;
};

var ofilter = (f, o) => {
  var n = {};
  for(var k in o)
    if(f(o[k], k))
      n[k] = o[k];
  return n;
};

var keys = o => {
  var a = [];
  for(var k in o) a.push(k);
  return a;
};

var vals = o => {
  return keys(o).map(k => o[k]);
};

var clone = function(o) {
  var n = {};
  for(var k in o) n[k] = o[k];
  for(var i = 1, l = arguments.length; i < l; i += 2)
    n[arguments[i]] = arguments[i+1];
  return n;
};

var range = (s_, a, b) => {
  if(a === b || s_ === 0) return [a];
  var s = Math.abs(s_);
  var r = [];
  if(a > b) {
    for(var i = a; i >= b; i -= s) r.push(i);
  } else {
    for(var i = a; i <= b; i += s) r.push(i);
  }
  return r;
};

var mapFrom2 = (ks, vs) => {
  var n = {};
  for(var i = 0, l = ks.length; i < l; i++) n[ks[i]] = vs[i];
  return n;
};

var mapFrom = (a) => {
  var n = {};
  for(var i = 0, l = a.length; i < l; i += 2) n[a[i]] = a[i+1];
  return n;
};

var map = function() {return mapFrom(arguments)};

var overlaps = (a, b) => {
  for(var k in a) if(b[k]) return true;
  for(var k in b) if(a[k]) return true;
  return false;
};

var flatten = a => a.reduce((x, y) => x.concat(y), []);

module.exports = {
  set: set,
  setFrom,
  union,
  without,
  omap,
  ofilter,
  keys,
  vals,
  clone,
  range,
  mapFrom2,
  mapFrom,
  map,
  overlaps,
  flatten,
};

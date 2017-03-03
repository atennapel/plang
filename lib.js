var _unit = {};

// combinators
function id(x) {return x}
function k(x) {return function(y) {return x}}
function flip(f) {return function(x) {return function(y) {return f(y)(x)}}}

// exprs
function _sel(label) {return function(o) {return o[label]}}

function _extend(label) {return function(x) {return function(o) {
  var n = {};
  for(var k in o) n[k] = o[k];
  n[label] = x;
  return n;
}}}

function _restrict(label) {return function(o) {
  var n = {};
  for(var k in o)
    if(k !== label)
      n[k] = o[k];
  return n;
}}

function _update(label) {return function(f) {return function(x) {
  var n = {};
  for(var k in x) n[k] = x[k];
  n[label] = f(n[label]);
  return n;
}}}

// effects
function _Cont(label, val, cont) {
  this._fc = true;
  this.label = label;
  this.val = val;
  this.cont = cont;
}
_Cont.prototype.toString = function(f) {
  return 'Cont ' + this.label + ' ' + (f || str)(this.val);
};

function _Ret(val) {
  this._fcr = true;
  this.val = val;
}
_Ret.prototype.toString = function(f) {
  return 'Return ' + (f || str)(this.val);
};

function ret(x) {return new _Ret(x)}
function pure(x) {return x.val}

function _perform(label) {return function(v) {
  return new _Cont(label, v, ret);
}}

function _throw(msg) {throw new Error(msg)}

function _do(val, cb) {
  return val instanceof _Cont?
    new _Cont(val.label, val.val, x => _do(val.cont(x), cb)):
    val instanceof _Ret? cb(val.val):
    _throw('Effect handler does not use Return');
}

function _handle(o) {
  return function(v) {
    if(v instanceof _Ret) return o['return']? o['return'](v.val): v;
    if(o[v.label]) {
      return o[v.label](v.val)(y => _handle(o)(v.cont(y)));
    } else return new _Cont(v.label, v.val, x => _handle(o)(v.cont(x)));
  };
}

var get = _perform('Get')(_unit);
var set = _perform('Set');
function state(v) {
  return function(x) {
    var state = v;
    var c = x;
    while(!(c instanceof _Ret)) {
      if(c.label === 'Get') c = c.cont(state);
      else if(c.label === 'Set') {
        state = c.val;
        c = c.cont(_unit);
      } else _throw('Invalid use of state');
    }
    return c.val;
  };
}

function _FO(tag, vals) {
  this._fo = true;
  this.tag = tag;
  this.vals = vals || [];
}
_FO.prototype.toString = function(f) {
  return this.vals.length === 0? this.tag:
    '(' + this.tag + ' ' + this.vals.map(f || str).join(' ') + ')';
};

function _case(map) {
  return function(x) {
    if(typeof x === 'boolean') {
      if(x) {
        return (map['True'] || map['_'])(_unit);
      } else {
        return (map['False'] || map['_'])(_unit);
      }
    }
    var f = map[x.tag] || map['_'];
    if(f) {
      if(x.vals.length === 0) return f(_unit);
      return x.vals.reduce((f, v) => f(v), f);
    }
    throw _throw('Unhandled case: ' + x.tag);
  };
}

// constants
var True = true;
var False = false;

// util
function str(x) {
  var t = typeof x;
  if(
    t === 'number' ||
    t === 'boolean' ||
    t === 'undefined' ||
    x === null
  ) return '' + x;
  if(t === 'string') return x;
  if(t === 'function') return '[Function]';
  if(x instanceof _Cont || x instanceof _Ret || x instanceof _FO)
    return x.toString(str);
  if(x instanceof Promise)
    return '(Promise)';
  if(Array.isArray(x))
    return '[' + x.map(str).join(', ') + ']';
  var r = [];
  for(var k in x) r.push(k + ': ' + str(x[k]));
  return '{' + r.join(', ') + '}';
}

function eq(a) {return function(b) {
  var t = typeof a;
  if(
    t === 'number' ||
    t === 'boolean' ||
    t === 'undefined' ||
    t === 'string' ||
    t === 'function' ||
    a === null
  ) return a === b;
  if(Array.isArray(a)) {
    if(!Array.isArray(b) || a.length !== b.length) return False;
    for(var i = 0, l = a.length; i < l; i++)
      if(!eq(a[i])(b[i])) return False;
    return True;
  }
  for(var k in a) if(!eq(a[k])(b[k])) return False;
  for(var k in b) if(!eq(a[k])(b[k])) return False;
  return True;
}}

// operators
function gt(a) {return function(b) {return (a > b)}}
function lt(a) {return function(b) {return (a < b)}}
function geq(a) {return function(b) {return (a >= b)}}
function leq(a) {return function(b) {return (a <= b)}}

function not(a) {return (!(a))}
function or(a) {return function(b) {return ((a) || (b))}}
function and(a) {return function(b) {return ((a) && (b))}}

function neg(a) {return -a}
function floor(a) {return Math.floor(a)}

function add(a) {return function(b) {return a + b}}
function sub(a) {return function(b) {return a - b}}
function mul(a) {return function(b) {return a * b}}
function div(a) {return function(b) {return a / b}}
function rem(a) {return function(b) {return a % b}}

// array
function arrSize(a) {return a.length}

function arrSingleton(v) {return [v]}

function arrRange(s_) {return function(a) {return function(b) {
  if(a === b || s_ === 0) return [a];
  var s = Math.abs(s_);
  var r = [];
  if(a > b) {
    for(var i = a; i >= b; i -= s) r.push(i);
  } else {
    for(var i = a; i <= b; i += s) r.push(i);
  }
  return r;
}}}

function arrMap(f) {return function(a) {
  var l = a.length;
  var r = Array(l);
  for(var i = 0; i < l; i++)
    r[i] = f(a[i]);
  return r;
}}

function arrFilter(f) {return function(a) {
  var r = [];
  for(var i = 0, l = a.length; i < l; i++)
    if(f(a[i])) r.push(a[i]);
  return r;
}}

function arrFoldl(f) {return function(v) {return function(a) {
  var l = a.length;
  var c = v;
  for(var i = 0; i < l; i++)
    c = f(c)(a[i]);
  return c;
}}}

function arrFoldr(f) {return function(v) {return function(a) {
  var l = a.length;
  var c = v;
  for(var i = a.length - 1; i >= 0; i--)
    c = f(c)(a[i]);
  return c;
}}}

function arrConcat(a) {return function(b) {
  var la = a.length;
  if(la === 0) return b;
  var lb = b.length;
  if(lb === 0) return a;
  var r = Array(la + lb);
  for(var i = 0; i < la; i++) r[i] = a[i];
  for(var i = 0; i < lb; i++) r[la + i] = b[i];
  return r;
}}

// io
var _log = _perform('Log');
var _prompt = _perform('Prompt');
var _alert = _perform('Alert');
var _io = _handle({
  Log: s => k => {console.log(s); return k(_unit)},
  Alert: s => k => {alert(s); return k(_unit)},
  Prompt: s => k => k(prompt(s) || ''),
});

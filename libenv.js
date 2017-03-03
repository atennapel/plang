var T = require('./types');
var K = require('./kinds');

var obj = function() {
  var l = arguments.length;
  if(l % 2 !== 0) throw new Error('Invalid number of arguments for obj');
  var n = {};
  for(var i = 0; i < l; i += 2) n[arguments[i]] = arguments[i + 1];
  return n;
};

var tarr = T.tarr;
var tapp = T.tapp;
var tfun = T.tfun;
var Unit = T.TUnit;
var Bool = T.TBool;
var Float = T.TFloat;
var Int = T.TInt;
var Str = T.TStr;

var Eff = T.TEff;
var TArr = T.TArr;
var Rec = T.TRec;

var Arr = T.TArray;

var a = T.tvar('a', K.kstar);
var b = T.tvar('b', K.kstar);
var c = T.tvar('c', K.kstar);

var v = T.tvar('v', K.kstar, null, true);
var w = T.tvar('w', K.kstar, null, true);

var ioE = T.tvar('e', K.krow, obj('Log', true, 'Prompt', true, 'Alert', true));
var fetchE = T.tvar('e', K.krow, obj('Fetch', true));

var env = {
  typings: {
    str: tarr(a, Str),
    eq: tarr(a, tarr(a, Bool)),

    id: tarr(a, a),
    k: tarr(a, tarr(b, a)),
    flip: tarr(
      tarr(a, tarr(b, c)),
      tarr(b, tarr(a, c))
    ),

    gt: tarr(a, tarr(a, Bool)),
    lt: tarr(a, tarr(a, Bool)),
    geq: tarr(a, tarr(a, Bool)),
    leq: tarr(a, tarr(a, Bool)),

    not: tarr(Bool, Bool),
    or: tarr(Bool, tarr(Bool, Bool)),
    and: tarr(Bool, tarr(Bool, Bool)),

    neg: tarr(Float, Float),
    add: tarr(Float, tarr(Float, Float)),
    sub: tarr(Float, tarr(Float, Float)),
    mul: tarr(Float, tarr(Float, Float)),
    div: tarr(Float, tarr(Float, Float)),
    rem: tarr(Float, tarr(Float, Float)),
    floor: tarr(Float, Float),

    ret: tarr(a, T.teff(T.tvar('e', K.krow), a)),
    pure: tarr(T.teff(T.trowempty, a), a),

    get: T.teff(
      T.trow({Get: tarr(Unit, v)}, T.tvar('e', K.krow, obj('Get', true))),
      v
    ),
    set: tarr(v, T.teff(
      T.trow({Set: tarr(v, Unit)}, T.tvar('e', K.krow, obj('Set', true))),
      v
    )),
    state: tarr(v, T.teff(T.trow({
      Get: tarr(Unit, v),
      Set: tarr(v, Unit)
    }), w), w),

    _log: tarr(Str, T.teff(
      T.trow({Log: tarr(Str, Unit)}, T.tvar('e', K.krow, obj('Log', true))),
      Unit
    )),
    _alert: tarr(Str, T.teff(
      T.trow({Alert: tarr(Str, Unit)}, T.tvar('e', K.krow, obj('Alert', true))),
      Unit
    )),
    _prompt: tarr(Str, T.teff(
      T.trow({Prompt: tarr(Str, Str)}, T.tvar('e', K.krow, obj('Prompt', true))),
      Str
    )),
    _io: tarr(T.teff(T.trow({
      Log: tarr(Str, Unit),
      Prompt: tarr(Str, Str),
      Alert: tarr(Str, Unit),
    }, ioE), w), T.teff(ioE, w)),

    _fetch: tarr(T.teff(T.trow({
      Fetch: tarr(Str, Str),
    }, fetchE), w), T.teff(fetchE, w)),

    True: Bool,
    False: Bool,

    arrSize: tarr(tapp(Arr, a, K.kstar), Float),
    arrMap: tarr(tarr(a, b), tapp(Arr, a, K.kstar), tapp(Arr, b, K.kstar)),
    arrFoldl: tarr(tarr(b, a, b), b, tapp(Arr, a, K.kstar), b),
    arrFoldr: tarr(tarr(b, a, b), b, tapp(Arr, a, K.kstar), b),
    arrConcat: tarr(tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar)),
    arrSingleton: tarr(a, tapp(Arr, a, K.kstar)),
    arrRange: tarr(Float, Float, Float, tapp(Arr, Float, K.kstar)),
    arrFilter: tarr(tarr(a, Bool), tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar)),
  },
  types: {
    Bool: {con: Bool, args: [], cases: {True: [], False: []}},
    Float: {con: Float, args: [], cases: {}},
    Int: {con: Int, args: [], cases: {}},
    Str: {con: Str, args: [], cases: {}},

    Eff: {con: Eff, args: [], cases: {}},
    "->": {con: TArr, args: [], cases: {}},
    Rec: {con: Rec, args: [], cases: {}},

    Array: {con: Arr, args: [], cases: {}},
  },
  cases: {
    True: 'Bool',
    False: 'Bool',
  },
};

module.exports = {
  env,
};

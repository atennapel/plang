var T = require('./types');
var K = require('./kinds');
var tc = require('./typechecker');

var obj = function() {
  var l = arguments.length;
  if(l % 2 !== 0) throw new Error('Invalid number of arguments for obj');
  var n = {};
  for(var i = 0; i < l; i += 2) n[arguments[i]] = arguments[i + 1];
  return n;
};

var mapo = function(f, o) {
  var n = {};
  for(var k in o)
    n[k] = f(o[k]);
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
var Lazy = T.TLazy;
var Rec = T.TRec;

var Arr = T.TArray;

var a = T.tvar('a', K.kstar);
var b = T.tvar('b', K.kstar);
var c = T.tvar('c', K.kstar);

var v = T.tvar('v', K.kstar, null, true);
var w = T.tvar('w', K.kstar, null, true);

var ioE = T.tvar('e', K.krow, obj('Log', true, 'Prompt', true, 'Alert', true));
var fetchE1 = T.tvar('e', K.krow, obj('Fetch', true));
var fetchE2 = T.tvar('e', K.krow, obj('Fetch', true, 'Err', true));

var env = {
  typings: mapo(tc.generalize, {
    _str: tarr(a, Str),
    _eq: tarr(a, tarr(a, Bool)),

    _gt: tarr(a, tarr(a, Bool)),
    _lt: tarr(a, tarr(a, Bool)),
    _geq: tarr(a, tarr(a, Bool)),
    _leq: tarr(a, tarr(a, Bool)),

    id: tarr(a, a),
    k: tarr(a, tarr(b, a)),
    flip: tarr(
      tarr(a, tarr(b, c)),
      tarr(b, tarr(a, c))
    ),

    lazy: tarr(tarr(Unit, a), tapp(Lazy, a, K.kstar)),
    force: tarr(tapp(Lazy, a, K.kstar), a),

    not: tarr(Bool, Bool),
    or: tarr(Bool, tarr(Bool, Bool)),
    and: tarr(Bool, tarr(Bool, Bool)),

    negFloat: tarr(Float, Float),
    addFloat: tarr(Float, tarr(Float, Float)),
    subFloat: tarr(Float, tarr(Float, Float)),
    mulFloat: tarr(Float, tarr(Float, Float)),
    divFloat: tarr(Float, tarr(Float, Float)),
    remFloat: tarr(Float, tarr(Float, Float)),

    negInt: tarr(Int, Int),
    addInt: tarr(Int, tarr(Int, Int)),
    subInt: tarr(Int, tarr(Int, Int)),
    mulInt: tarr(Int, tarr(Int, Int)),
    divInt: tarr(Int, tarr(Int, Int)),
    remInt: tarr(Int, tarr(Int, Int)),

    floor: tarr(Float, Float),
    intToFloat: tarr(Int, Float),
    floatToInt: tarr(Float, Int),
    floatToString: tarr(Float, Str),
    intToString: tarr(Int, Str),

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

    _fetch: tarr(Str, T.teff(
      T.trow({Fetch: tarr(Str, Str)}, T.tvar('e', K.krow, obj('Fetch', true))),
      Str
    )),
    _doFetch: tarr(
      T.teff(T.trow({
        Fetch: tarr(Str, Str),
      }, fetchE1), w),
      T.teff(T.trow({
        Err: tarr(Str, a),
      }, fetchE2), w)
    ),

    True: Bool,
    False: Bool,

    strAppend: tarr(Str, Str, Str),

    arrSize: tarr(tapp(Arr, a, K.kstar), Float),
    arrMap: tarr(tarr(a, b), tapp(Arr, a, K.kstar), tapp(Arr, b, K.kstar)),
    arrFoldl: tarr(tarr(b, a, b), b, tapp(Arr, a, K.kstar), b),
    arrFoldr: tarr(tarr(b, a, b), b, tapp(Arr, a, K.kstar), b),
    arrAppend: tarr(tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar)),
    arrSingleton: tarr(a, tapp(Arr, a, K.kstar)),
    arrRange: tarr(Float, Float, Float, tapp(Arr, Float, K.kstar)),
    arrFilter: tarr(tarr(a, Bool), tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar)),
    arrJoin: tarr(Str, tapp(Arr, Str, K.kstar), Str),
    _arrEq: tarr(tarr(a, a, Bool), tapp(Arr, a, K.kstar), tapp(Arr, a, K.kstar), Bool),
  }),
  types: {
    Bool: {con: Bool, args: [], cases: {True: [], False: []}},
    Float: {con: Float, args: [], cases: {}},
    Int: {con: Int, args: [], cases: {}},
    Str: {con: Str, args: [], cases: {}},

    Eff: {con: Eff, args: [], cases: {}},
    "->": {con: TArr, args: [], cases: {}},
    Rec: {con: Rec, args: [], cases: {}},

    Array: {con: Arr, args: [], cases: {}},
    Lazy: {con: Lazy, args: [], cases: {}},
  },
  cases: {
    True: 'Bool',
    False: 'Bool',
  },
  impl: {
  },
};

module.exports = {
  env,
};

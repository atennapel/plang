var parser = require('./parser');
var compiler = require('./compiler');
var typechecker = require('./typechecker');

var E = require('./exprs');
var T = require('./types');
var K = require('./kinds');

var t = T.tvar('t', 't0');
var show = T.tvar('show', 'show0', K.Star, null, false, {Show: true});

var Bool = T.Bool;
var Int = T.tcon('Int');
var Str = T.tcon('Str');
var List = T.tcon('List', K.karr(K.Star, K.Star));

var env = {
  typings: {
    True: T.tscheme([], Bool),
    False: T.tscheme([], Bool),

    zero: T.tscheme([], Int),
    one: T.tscheme([], Int),

    add: T.tscheme([], T.tarr(Int, Int, Int)),

    show: T.tscheme([show], T.tarr(show, Str)),
  },
  newtypes: {
    List: {
      con: List,
      args: [t],
      type: T.tscheme([t],
          T.tapp(T.TVariant, T.trow({
            Nil: T.tapp(T.TRecord, T.trowempty),
            Cons: T.tapp(T.TRecord, T.trow({
              0: t,
              1: T.tapp(List, t),
            })),
          }))),
    },
  },
  classes: {
    Show: {
      instances: [T.tscheme([], Int)],
      dicts: ['_D_Show_0'],
    }
  },
};

var state = {tvar: {t: 1, show: 1}};

var inp = require('fs').readFileSync(process.argv[2], {encoding: 'utf8'});
var parsed = parser.parse(inp);
console.log(E.toString(parsed));
var type = typechecker.infer(parsed, env, state);
console.log(T.toString(type) + ' : ' + K.toString(type.kind));
var compiled = compiler.compileWithLib(parsed);
var evalled = eval(compiled);
console.log(evalled);
console.log(JSON.stringify(evalled, null, 2));
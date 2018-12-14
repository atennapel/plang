import { TVar, Forall, tfun, tapp, TFun, teffs, isTEffsEmpty, showForall, prettyForall } from "./types";
import { Var, showExpr } from "./exprs";
import { extendContextMut, initial } from "./context";
import { kType, kfun, kEff, kEffs } from "./kinds";
import { inferGen } from "./inference";
import compileToJS from "./javascriptBackend";
import parse from "./parser";
import { throwEither } from "./either";

const v = Var;
const tv = TVar;

const _context = extendContextMut(initial,
  {},
  {
    Void: kType,
    Bool: kType,

    Pair: kfun(kType, kType, kType),

    List: kfun(kType, kType),

    Flip: kEff,
    State: kEff,
  },
  {
    Flip: { flip: true },
    State: { get: true, put: true },
  },
  {
    flip: { eff: 'Flip', param: tv('Unit'), ret: tv('Bool') },
    get: { eff: 'State', param: tv('Unit'), ret: tv('Bool') },
    put: { eff: 'State', param: tv('Bool'), ret: tv('Unit') },
  },
  {
    Void: Forall([['t', kType]], tfun(tv('Void'), tv('t'))),
    True: Forall([], tv('Bool')),
    False: Forall([], tv('Bool')),

    Pair: Forall([['a', kType], ['b', kType]], tfun(tv('a'), tv('b'), tapp(tv('Pair'), tv('a'), tv('b')))),
    fst: Forall([['a', kType], ['b', kType]], tfun(tapp(tv('Pair'), tv('a'), tv('b')), tv('a'))),
    snd: Forall([['a', kType], ['b', kType]], tfun(tapp(tv('Pair'), tv('a'), tv('b')), tv('b'))),
    
    not: Forall([], tfun(tv('Bool'), tv('Bool'))),
    
    id: Forall([['t', kType]], tfun(tv('t'), tv('t'))),
    cnst: Forall([['a', kType], ['b', kType]], tfun(tv('a'), tv('b'), tv('a'))),

    Nil: Forall([['t', kType]], tapp(tv('List'), tv('t'))),
    Cons: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')), tapp(tv('List'), tv('t')))),

    flip: Forall([], TFun(tv('Unit'), teffs([tv('Flip')]), tv('Bool'))),
    get: Forall([], TFun(tv('Unit'), teffs([tv('State')]), tv('Bool'))),
    put: Forall([], TFun(tv('Bool'), teffs([tv('State')]), tv('Unit'))),

    fix: Forall([['t', kType]], tfun(tfun(tv('t'), tv('t')), tv('t'))),
    caseList: Forall([['t', kType], ['r', kType], ['e', kEffs]], tfun(tv('r'), tfun(tv('t'), TFun(tapp(tv('List'), tv('t')), tv('e'), tv('r'))), TFun(tapp(tv('List'), tv('t')), tv('e'), tv('r')))),
  },
);

function _show(x: any): string {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag :
    Array.isArray(x.val) ? `(${x._tag} ${x.val.map(_show).join(' ')})` :
    `(${x._tag} ${_show(x.val)})`;
  if (x._cont) return `${x.op}(${_show(x.val)})`;
  return '' + x;
}

let _ctx = _context;
export default function _run(i: string, cb: (output: string, err?: boolean) => void): void {
  try {
    console.log(i);
    const p = parse(i);
    console.log(showExpr(p));
    const result = throwEither(inferGen(_ctx, p));
    console.log(`${showForall(result)}`);
    const c = compileToJS(p);
    console.log(c);
    const res = eval(c);
    cb(`${_show(res)} : ${prettyForall(result)}`);
  } catch(e) {
    console.log(e);
    cb(''+e, true);
  }
}

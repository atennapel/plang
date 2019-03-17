import { showType, TVar, tfun, tforallK, tapp, tappFrom } from './types';
import { compile, compileDefs } from './compiler';
import { infer, inferDefs } from './inference';
import { parse, parseType, parseDefs } from './parser';
import { context } from './global';
import { CTVar, CVar } from './elems';
import { Name, showName } from './names';
import { kType, kfun, kfunFrom, Kind } from './kinds';
import { showTerm } from './terms';

const _show = (x: any): string => {
  if (typeof x === 'function') return '[Fn]';
  if (typeof x === 'string') return JSON.stringify(x);
  if (Array.isArray(x)) return `[${x.map(_show).join(', ')}]`;
  if (typeof x === 'object' && typeof x._tag === 'string') {
    if (x._tag === 'Pair') return `(Pair ${_show(x.val[0])} ${_show(x.val[1])})`;
    return x.val === null ? x._tag : `(${x._tag} ${_show(x.val)})`;
  }
  return '' + x;
};

const _Bool = Name('Bool');
const _Nat = Name('Nat');
const _List = Name('List');
const _t = Name('t');
const _tv = TVar(_t);
context.add(
  CTVar(_Bool, kType),
  CVar(Name('primTrue'), TVar(_Bool)),
  CVar(Name('primFalse'), TVar(_Bool)),

  CTVar(_Nat, kType),
  CVar(Name('primZ'), TVar(_Nat)),
  CVar(Name('primS'), tfun(TVar(_Nat), TVar(_Nat))),

  CTVar(_List, kfun(kType, kType)),
  CVar(Name('primNil'), tforallK([[_t, kType]], tapp(TVar(_List), _tv))),
  CVar(Name('primCons'), tforallK([[_t, kType]], tfun(_tv, tapp(TVar(_List), _tv), tapp(TVar(_List), _tv)))),
);

const _env: string = typeof global === 'undefined' ? 'window' : 'global';
export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  if (_s === ':ctx') return _cb(`${context}`);
  if (_s.startsWith(':def ')) {
    try {
      const _rest = _s.slice(4).trim();
      const _ds = parseDefs(_rest);
      inferDefs(_ds);
      const _c = compileDefs(_ds, n => `${_env}['${n}']`);
      eval(`(() => {${_c}})()`);
      return _cb(`defined ${_ds.map(d => showName(d.name)).join(' ')}`);
    } catch (err) {
      return _cb(`${err}`, true);
    }
  }
  try {
    const _e = parse(_s);
    // console.log(showTerm(_e));
    const _t = infer(_e);
    // console.log(showType(_t));
    const _c = compile(_e);
    // console.log(_c);
    const _v = eval(_c);
    // console.log(_v);
    return _cb(`${_show(_v)} : ${showType(_t)}`);
  } catch (err) {
    return _cb('' + err, true);
  }
};


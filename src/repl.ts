import { showType, TVar, tfun, tforallK, tapp, tappFrom } from './types';
import { compile } from './compiler';
import { infer } from './inference';
import { parse, parseType } from './parser';
import { context } from './global';
import { CTVar, CVar } from './elems';
import { Name } from './names';
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
const _r = Name('r');
const _rv = TVar(_r);
context.add(
  CTVar(_Bool, kType),
  CVar(Name('True'), TVar(_Bool)),
  CVar(Name('False'), TVar(_Bool)),
  CVar(Name('if'), tforallK([[Name('t'), kType]], tfun(TVar(_Bool), TVar(Name('t')), TVar(Name('t')), TVar(Name('t'))))),

  CTVar(_Nat, kType),
  CVar(Name('Z'), TVar(_Nat)),
  CVar(Name('S'), tfun(TVar(_Nat), TVar(_Nat))),
  CVar(Name('caseNat'), tforallK([[Name('t'), kType]], tfun(TVar(Name('t')), tfun(TVar(_Nat), TVar(Name('t'))), TVar(_Nat), TVar(Name('t'))))),
  CVar(Name('iterNat'), tforallK([[Name('t'), kType]], tfun(TVar(Name('t')), tfun(TVar(Name('t')), TVar(Name('t'))), TVar(_Nat), TVar(Name('t'))))),
  CVar(Name('recNat'), tforallK([[Name('t'), kType]], tfun(TVar(Name('t')), tfun(TVar(_Nat), TVar(Name('t')), TVar(Name('t'))), TVar(_Nat), TVar(Name('t'))))),

  CTVar(_List, kfun(kType, kType)),
  CVar(Name('Nil'), tforallK([[_t, kType]], tapp(TVar(_List), _tv))),
  CVar(Name('Cons'), tforallK([[_t, kType]], tfun(_tv, tapp(TVar(_List), _tv), tapp(TVar(_List), _tv)))),
  CVar(Name('caseList'), tforallK([[_t, kType], [_r, kType]], tfun(_rv, tfun(_tv, tapp(TVar(_List), _tv), _rv), tapp(TVar(_List), _tv), _rv))),
  CVar(Name('iterList'), tforallK([[_t, kType], [_r, kType]], tfun(_rv, tfun(_tv, _rv, _rv), tapp(TVar(_List), _tv), _rv))),
  CVar(Name('recList'), tforallK([[_t, kType], [_r, kType]], tfun(_rv, tfun(_tv, tapp(TVar(_List), _tv), _rv, _rv), tapp(TVar(_List), _tv), _rv))),
);

const _env: any = typeof global === 'undefined' ? window : global;
const _id = <T>(x: T) => x;
export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  if (_s === ':ctx') return _cb(`${context}`);
  if (_s.startsWith(':newtype ')) {
    try {
      const _parts = _s.slice(8).trim().split('=');
      const _args = _parts[0].split(/\s+/g).filter(_id);
      const _ty = parseType(_parts.slice(1).join('='));
      const _tname = _args[0];
      const _targs = _args.slice(1).map(Name);
      if (context.lookup('CTVar', Name(_tname)))
        throw new TypeError(`type ${_tname} is already defined`);
      if (context.lookup('CVar', Name(_tname)))
        throw new TypeError(`${_tname} is already defined`);
      if (context.lookup('CVar', Name(`un${_tname}`)))
        throw new TypeError(`un${_tname} is already defined`);
      context.add(
        CTVar(Name(_tname), kfunFrom(_targs.map(_ => kType).concat([kType]))),
        CVar(Name(_tname), tforallK(_targs.map(n => [n, kType] as [Name, Kind]), tfun(_ty, tappFrom([TVar(Name(_tname))].concat(_targs.map(TVar)))))),
        CVar(Name(`un${_tname}`), tforallK(_targs.map(n => [n, kType] as [Name, Kind]), tfun(tappFrom([TVar(Name(_tname))].concat(_targs.map(TVar))), _ty))),
      );
      _env[_tname] = _env[`un${_tname}`] = _id;
      return _cb(`defined ${_tname} and un${_tname}`);
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


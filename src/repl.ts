import { config, log } from './config';
import { initialEnv, showEnv } from './env';
import { showTerm } from './terms';
import { showTy, TCon, TForall, tfunFrom, TVar, tappFrom, Type } from './types';
import { compile, compileName } from './compiler';
import { infer } from './inference';
import { parse, parseType } from './parser';
import { kType, kfunFrom, Kind, pruneKind } from './kinds';
import { inferKind } from './kindInference';
import { freshKMeta } from './util';

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

const _env = initialEnv;
const _global = typeof global === 'undefined' ? 'window' : 'global';
export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  try {
    if (_s === ':env' || _s === ':e')
      return _cb(showEnv(_env));
    if (_s === ':showkinds' || _s === ':k') {
      config.showKinds = !config.showKinds;
      return _cb(`showKinds: ${config.showKinds}`);
    }
    if (_s === ':debug' || _s === ':d') {
      config.debug = !config.debug;
      return _cb(`debug: ${config.debug}`);
    }
    if (_s.startsWith(':showBool ')) {
      const _rest = _s.slice(9);
      const _e = parse(_rest);
      log(showTerm(_e));
      const _t = infer(_env, _e);
      log(showTy(_t));
      if (_t.tag !== 'TCon' || _t.name !== 'Bool')
        throw new Error(`not a Bool in showBool`);
      const _c = compile(_e);
      log(_c);
      const _v = eval(`${_c}(true)(false)`);
      return _cb(`${_show(_v)}`);
    }
    if (_s.startsWith(':showNat ')) {
      const _rest = _s.slice(8);
      const _e = parse(_rest);
      log(showTerm(_e));
      const _t = infer(_env, _e);
      log(showTy(_t));
      if (_t.tag !== 'TCon' || _t.name !== 'Nat')
        throw new Error(`not a Nat in showNat`);
      const _c = compile(_e);
      log(_c);
      const _v = eval(`${_c}(x => x + 1)(0)`);
      return _cb(`${_show(_v)}`);
    }
    if (_s.startsWith(':let ')) {
      const _parts = _s.slice(4).trim().split('=');
      const _name = _parts[0].trim();
      const _rest = _parts.slice(1).join('=');
      if (!/^[a-z][a-zA-Z0-9]*$/.test(_name))
        throw new Error(`invalid name for let: ${_name}`);
      const _e = parse(_rest);
      log(showTerm(_e));
      const _t = infer(_env, _e);
      log(showTy(_t));
      const _c = compile(_e);
      log(_c);
      const _v = eval(`${_global}['${compileName(_name)}'] = ${_c}`);
      log(_v);
      _env.global[_name] = _t;
      return _cb(`${_name} = ${_show(_v)} : ${showTy(_t)}`);
    }
    if (_s.startsWith(':type ')) {
      const _parts = _s.slice(5).trim().split('=');
      const _parts0 = _parts[0].trim().split(/\s+/g);
      if (_parts0.length === 0)
        throw new Error('empty name');
      const _name = _parts0[0];
      const _args = _parts0.slice(1);
      const _rest = _parts.slice(1).join('=');
      if (!/^[A-Z][a-zA-Z0-9]*$/.test(_name))
        throw new Error(`invalid name for type: ${_name}`);
      const _t = parseType(_rest);
      _env.tcons[_name] = freshKMeta();
      const _b = tfunFrom([_t, tappFrom([TCon(_name) as Type]
        .concat(_args.map(TVar)))]);
      const _ty = _args.length === 0 ? _b :
        TForall(_args, _args.map(() => null), _b);
      const _ti = inferKind(_env, _ty);
      _env.global[_name] = _ti;
      _env.tcons[_name] = pruneKind(_env.tcons[_name]);
      eval(`${_global}['${compileName(_name)}'] = x => x`);
      return _cb(`type ${_name} defined`);
    }
    const _e = parse(_s);
    log(showTerm(_e));
    const _t = infer(_env, _e);
    log(showTy(_t));
    const _c = compile(_e);
    log(_c);
    const _v = eval(_c);
    log(_v);
    return _cb(`${_show(_v)} : ${showTy(_t)}`);
  } catch (err) {
    return _cb('' + err, true);
  }
};

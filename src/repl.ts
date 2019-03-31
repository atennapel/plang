import { config, log } from './config';
import { initialEnv, showEnv } from './env';
import { showTerm } from './terms';
import { showTy, TCon, TForall, tfunFrom, TVar, tappFrom, Type } from './types';
import { compile, compileName, compileDefs } from './compiler';
import { infer, inferDefs } from './inference';
import { parse, parseType, parseDefs } from './parser';
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
    if (_s.startsWith(':load ') || _s.startsWith(':l ')) {
      const _rest = (_s.startsWith(':load') ? _s.slice(5) : _s.slice(2)).trim();
      fetch(`lib/${_rest}`)
        .then(res => res.text())
        .then(_rest => {
          const _ds = parseDefs(_rest);
          inferDefs(_env, _ds);
          const _c = compileDefs(_ds, n => `${_global}['${n}']`);
          eval(`(() => {${_c}})()`);
          return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
        })
        .catch(_err => _cb(`${_err}`, true));
      return;
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
    if (_s.startsWith(':let ') || _s.startsWith(':type ')) {
      const _rest = _s.slice(1);
      const _ds = parseDefs(_rest);
      inferDefs(_env, _ds);
      const _c = compileDefs(_ds, n => `${_global}['${n}']`);
      eval(`(() => {${_c}})()`);
      return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
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
  } catch (_err) {
    return _cb(`${_err}`, true);
  }
};

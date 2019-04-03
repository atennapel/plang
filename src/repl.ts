import { config, log } from './config';
import { initialEnv, showEnv } from './env';
import { showTerm } from './terms';
import { showTy, Type } from './types';
import { compile, compileDefs } from './compiler';
import { infer, inferDefs } from './inference';
import { parse, parseDefs } from './parser';
import { runState, runVal, showState, showVal, Env } from './machine';
import List from './List';

const _showR = (x: any): string => {
  if (typeof x === 'function') return '[Fn]';
  if (typeof x === 'string') return JSON.stringify(x);
  if (Array.isArray(x)) return `[${x.map(_showR).join(', ')}]`;
  if (typeof x === 'object' && typeof x._tag === 'string') {
    if (x._tag === 'Pair') return `(Pair ${_showR(x.val[0])} ${_showR(x.val[1])})`;
    return x.val === null ? x._tag : `(${x._tag} ${_showR(x.val)})`;
  }
  return '' + x;
};
const _show = (x: any, t: Type): string => {
  if (t.tag === 'TCon' && t.name === 'Bool')
    return x('true')('false');
  if (t.tag === 'TCon' && t.name === 'Nat')
    return `${x((x: number) => x + 1)(0)}`;
  if (t.tag === 'TCon' && t.name === 'Str') {
    const r: string[] = [];
    x(r)((n: any) => (r: string[]) => {
      r.push(
        String.fromCharCode(n((x: number) => x + 1)(0)),
      );
      return r;
    });
    return JSON.stringify(r.reverse().join(''));
  }
  if (t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === 'List') {
    const ty = t.right;
    const r: string[] = [];
    x(r)((y: any) => (r: string[]) => {
      r.push(_show(y, ty));
      return r;
    });
    return `[${r.reverse().join(', ')}]`;
  }
  return _showR(x);
};

const _env = initialEnv;
const _venv: Env = List.nil();
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
    if (_s.startsWith(':load ') || _s.startsWith(':l ') || _s === ':l' || _s === ':load') {
      const _rest = (_s.startsWith(':load') ? _s.slice(5) : _s.slice(2)).trim();
      fetch(`lib/${_rest || 'prelude.p'}`)
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
    if (_s.startsWith(':let ') || _s.startsWith(':type ')) {
      const _rest = _s.slice(1);
      const _ds = parseDefs(_rest);
      inferDefs(_env, _ds);
      const _c = compileDefs(_ds, n => `${_global}['${n}']`);
      eval(`(() => {${_c}})()`);
      return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
    }
    if (_s.startsWith(':cek ')) {
      const _rest = _s.slice(4);
      const _e = parse(_rest);
      const _st = runState(_e);
      return _cb(showState(_st));
    }
    if (_s.startsWith(':cekv ')) {
      const _rest = _s.slice(5);
      const _e = parse(_rest);
      const _st = runVal(_e);
      return _cb(showVal(_st));
    }
    if (_s.startsWith(':t')) {
      const _rest = _s.slice(2);
      const _e = parse(_rest);
      const _t = infer(_env, _e);
      return _cb(showTy(_t));
    }
    const _e = parse(_s);
    log(showTerm(_e));
    const _t = infer(_env, _e);
    log(showTy(_t));
    const _c = compile(_e);
    log(_c);
    const _v = eval(_c);
    log(_v);
    return _cb(`${_show(_v, _t)} : ${showTy(_t)}`);
  } catch (_err) {
    return _cb(`${_err}`, true);
  }
};

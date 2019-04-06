import { config, log } from './config';
import { initialEnv, showEnv } from './env';
import { showTerm } from './terms';
import { showTy, Type } from './types';
import { infer, inferDefs } from './inference';
import { parse, parseDefs } from './parser';
import {
  runVal,
  Env,
  runEnv,
  Val,
  showMTerm,
  Clos,
  State,
  steps,
  MApp,
  MConst,
  MVar,
  MAbs,
  MAdd,
  mapp,
  mabs,
  MEmit,
} from './machine';
import List from './List';
import { Name } from './util';

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

const matchTCon = (t: Type, name: Name) => t.tag === 'TCon' && t.name === name;
const _showVal = (v: Val, t: Type): string => {
  const isChar = matchTCon(t, 'Char');
  if (isChar || matchTCon(t, 'Nat')) {
    const cl = v as Clos;
    const env = cl.env.append(_venv);
    const st = State(mapp(MVar('cataNat'), cl.abs, MAbs('x', MAdd(MVar('x'), MConst(1))), MConst(0)), env);
    const res = steps(st);
    const n = (res.term as MConst).val;
    return isChar ? `'${JSON.stringify(String.fromCharCode(n)).slice(1, -1)}'` : `${n}`;
  }
  if (matchTCon(t, 'Bool')) {
    const cl = v as Clos;
    const res = steps(State(MApp(MApp(cl.abs, MConst(1)), MConst(0)), cl.env));
    return `${(res.term as MConst).val ? 'true' : 'false'}`;
  }
  if (matchTCon(t, 'Str')) {
    const cl = v as Clos;
    const env = cl.env.append(_venv);
    const r: number[] = [];
    const _s = (x: any) => mapp(MVar('cataNat'), x, MAbs('x', MAdd(MVar('x'), MConst(1))), MConst(0));
    const st = mapp(MVar('cataList'), cl.abs, MConst(0), mabs(['h', 'r'], MEmit(n => r.push(n), _s(MVar('h')))));
    const res = steps(State(st, env));
    return JSON.stringify(r.reverse().map(n => String.fromCharCode(n)).join(''));
  }
  if (v.tag === 'VConst') return `${v.val}`;
  if (v.tag === 'Clos') return `Closure(${showMTerm(v.abs)})`;
  return '?';
};

const _env = initialEnv;
let _venv: Env = List.nil();
// const _global = typeof global === 'undefined' ? 'window' : 'global';
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
          //const _c = compileDefs(_ds, n => `${_global}['${n}']`);
          //eval(`(() => {${_c}})()`);
          _venv = runEnv(_ds, _venv);
          return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
        })
        .catch(_err => _cb(`${_err}`, true));
      return;
    }
    if (_s.startsWith(':let ') || _s.startsWith(':type ')) {
      const _rest = _s.slice(1);
      const _ds = parseDefs(_rest);
      inferDefs(_env, _ds);
      //const _c = compileDefs(_ds, n => `${_global}['${n}']`);
      //eval(`(() => {${_c}})()`);
      _venv = runEnv(_ds, _venv);
      return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
    }
    /*
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
    */
    if (_s.startsWith(':t')) {
      const _rest = _s.slice(2);
      const _e = parse(_rest);
      const _t = infer(_env, _e);
      return _cb(showTy(_t));
    }
    const _e = parse(_s);
    log(() => showTerm(_e));
    const _t = infer(_env, _e);
    log(() => showTy(_t));
    const _v = runVal(_e, _venv);
    /*const _c = compile(_e);
    log(_c);
    const _v = eval(_c);
    log(_v);*/
    return _cb(`${_showVal(_v, _t)} : ${showTy(_t)}`);
  } catch (_err) {
    console.log(_err);
    return _cb(`${_err}`, true);
  }
};

import { config, log } from './config';
import { initialEnv, showEnv } from './env';
import { showTerm } from './terms';
import { showTy, Type, TCon, TApp } from './types';
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
  MVar,
  MAbs,
  mapp,
  mabs,
  MPairC,
  MAtom,
  MTerm,
  MPair,
  stepsVal,
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

const matchTCon = (t: Type, name: Name): t is TCon =>
  t.tag === 'TCon' && t.name === name;
const matchTApp = (t: Type, name: Name): t is TApp =>
  t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === name;
const reify = (v: Val, t: Type): any => {
  if (matchTCon(t, 'Nat')) {
    const cl = v as Clos;
    const env = cl.env.append(_venv);
    const st = State(mapp(MVar('cataNat'), cl.abs, MAbs('x', MPairC(MAtom('S'), MVar('x'))), MAtom('Z')), env);
    let c = stepsVal(st);
    let n = 0;
    while (c.tag === 'VPair') {
      n++;
      c = c.right;
    }
    return n;
  }
  if (matchTCon(t, 'Bool')) {
    const cl = v as Clos;
    const env = cl.env.append(_venv);
    const st = State(mapp(MVar('cond'), cl.abs, MAtom('T'), MAtom('F')), env);
    let c = stepsVal(st);
    return c.tag === 'VAtom' && c.val === 'T'; 
  }
  if (matchTCon(t, 'Char')) {
    const n = reify(v, TCon('Nat'));
    return String.fromCharCode(n);
  }
  if (matchTApp(t, 'List')) {
    const cl = v as Clos;
    const env = cl.env.append(_venv);
    const st = State(mapp(MVar('cataList'), cl.abs, MAtom('Nil'), mabs(['h', 'r'], MPairC(MVar('h'), MVar('r')))), env);
    let c = stepsVal(st);
    const r: Val[] = [];
    while (c.tag === 'VPair') {
      r.push(c.left);
      c = c.right;
    }
    return r;
  }
  if (matchTCon(t, 'Str')) {
    const l = reify(v, TApp(TCon('List'), TCon('Nat')));
    return l.map((v: Val) => String.fromCharCode(reify(v, TCon('Nat')))).join('');
  }
  if (v.tag === 'Clos') return `*closure*`;
  return '?';
};
const _showVal = (v: Val, t: Type): string => {
  if (matchTCon(t, 'Unit')) return '()';
  if (matchTCon(t, 'Nat')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Bool')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Char')) return `'${JSON.stringify(reify(v, t)).slice(1, -1)}'`;
  if (matchTApp(t, 'List')) return `[${reify(v, t).map((x: Val) => _showVal(x, t.right)).join(', ')}]`;
  if (matchTCon(t, 'Str')) return JSON.stringify(reify(v, t));
  if (v.tag === 'Clos') return `*closure*`;
  return '?';
};

const _env = initialEnv;
let _venv: Env = List.nil();
// const _global = typeof window === 'undefined' ? 'global' : 'window';
const isBrowser = typeof window !== 'undefined';
const load = (lib: string, cb: (err: Error | null, file: string) => void): void => {
  if (isBrowser) {
    fetch(`lib/${lib || 'prelude'}.p`)
      .then(x => x.text()).then(x => cb(null, x))
      .catch(err => cb(err, ''));
  } else {
    require('fs').readFile(`./lib/${lib || 'prelude'}.p`, 'utf8', cb);
  }
};
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
      load(_rest, (err, _rest) => {
        if (err) return _cb(`${err}`, true);
        const _ds = parseDefs(_rest);
        inferDefs(_env, _ds);
        _venv = runEnv(_ds, _venv);
        return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
      });
      return;
    }
    _s = _s + '\n';
    if (_s.startsWith(':let ') || _s.startsWith(':type ')) {
      const _rest = _s.slice(1);
      const _ds = parseDefs(_rest);
      inferDefs(_env, _ds);
      _venv = runEnv(_ds, _venv);
      return _cb(`defined ${_ds.map(d => d.name).join(' ')}`);
    }
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
    return _cb(`${_showVal(_v, _t)} : ${showTy(_t)}`);
  } catch (_err) {
    log(() => _err);
    return _cb(`${_err}`, true);
  }
};

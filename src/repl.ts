import { config, log } from './config';
import { Env as TEnv, showEnv, getInitialEnv } from './env';
import { showTerm, App, LitNat } from './terms';
import { showTy, Type, TCon, TApp } from './types';
import { infer, inferDefs } from './inference';
import { parse, parseDefs, ImportMap } from './parser';
import {
  runVal,
  Env,
  runEnv,
  Val,
  Clos,
  State,
  MVar,
  MAbs,
  mapp,
  mabs,
  MPairC,
  MAtom,
  stepsVal,
  VPair,
  VAtom,
  stepCount,
  resetStepCount,
  GEnv,
  showVal,
} from './machine';
import { Nil, append } from './List';
import { Name } from './util';

const HELP = `
  commands :help :env :showKinds :debug :time :reset :let :type :import :t :perf
`.trim();

export type ReplState = { importmap: ImportMap, tenv: TEnv, venv: GEnv };
const cenv: ReplState = {
  importmap: {},
  tenv: getInitialEnv(),
  venv: {},
};

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
const matchTApp2 = (t: Type, name: Name): t is TApp =>
  t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
    t.left.left.name === name;
const reify = (v: Val, t: Type): any => {
  if (matchTCon(t, 'Nat')) {
    const cl = v as Clos;
    const st = State(mapp(MVar('cataNat'), cl.abs, MAbs('x', MPairC(MAtom('S'), MVar('x'))), MAtom('Z')), cl.env);
    let c = stepsVal(st, cenv.venv);
    let n = 0;
    while (c.tag === 'VPair') {
      n++;
      c = c.right;
    }
    return n;
  }
  if (matchTCon(t, 'BNat')) {
    const cl = v as Clos;
    const st = State(mapp(MVar('cataBNat'), cl.abs, MAtom('Z'), MAbs('x', MPairC(MAtom('T'), MVar('x'))), MAbs('x', MPairC(MAtom('TI'), MVar('x')))), cl.env);
    let c = stepsVal(st, cenv.venv);
    const ar = [];
    while (c.tag === 'VPair') {
      let a = (c.left as VAtom).val;
      ar.push(a === 'T' ? 0 : a === 'TI' ? 1 : 0);
      c = c.right;
    }
    let n = 0;
    for (let i = ar.length - 1; i >= 0; i--) n = (n * 2) + ar[i];
    return n;
  }
  if (matchTCon(t, 'Bool')) {
    const cl = v as Clos;
    const st = State(mapp(MVar('cond'), cl.abs, MAtom('T'), MAtom('F')), cl.env);
    let c = stepsVal(st, cenv.venv);
    return c.tag === 'VAtom' && c.val === 'T'; 
  }
  if (matchTCon(t, 'Char')) {
    const n = reify(v, TCon('Nat'));
    return String.fromCharCode(n);
  }
  if (matchTApp(t, 'List')) {
    const cl = v as Clos;
    const st = State(mapp(MVar('cataList'), cl.abs, MAtom('Nil'), mabs(['h', 'r'], MPairC(MVar('h'), MVar('r')))), cl.env);
    let c = stepsVal(st, cenv.venv);
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
  if (matchTApp2(t, 'Pair')) {
    const cl = v as Clos;
    const st = State(mapp(cl.abs, mabs(['x', 'y'], MPairC(MVar('x'), MVar('y')))), cl.env);
    const c = stepsVal(st, cenv.venv) as VPair;
    return [c.left, c.right];
  }
  if (matchTApp2(t, 'Sum')) {
    const cl = v as Clos;
    const st = State(mapp(cl.abs, mabs(['x'], MPairC(MAtom('L'), MVar('x'))), mabs(['x'], MPairC(MAtom('R'), MVar('x')))), cl.env);
    const c = stepsVal(st, cenv.venv) as VPair;
    const tag = (c.left as VAtom).val;
    return [tag, c.right];
  }
  if (v.tag === 'Clos') return `*closure*`;
  return '?';
};
const _showVal = (v: Val, t: Type): string => {
  if (matchTCon(t, 'Unit')) return '()';
  if (matchTCon(t, 'Nat')) return `${reify(v, t)}`;
  if (matchTCon(t, 'BNat')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Bool')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Char')) return `'${JSON.stringify(reify(v, t)).slice(1, -1)}'`;
  if (matchTApp(t, 'List')) return `[${reify(v, t).map((x: Val) => _showVal(x, t.right)).join(', ')}]`;
  if (matchTCon(t, 'Str')) return JSON.stringify(reify(v, t));
  if (matchTApp2(t, 'Pair')) {
    const [a, b] = reify(v, t);
    const sa = _showVal(a, (t.left as any).right);
    const sb = _showVal(b, t.right);
    return `(${sa}, ${sb})`;
  }
  if (matchTApp2(t, 'Sum')) {
    const [tag, val] = reify(v, t);
    const str = tag === 'L' ?
      _showVal(val, (t.left as any).right) : _showVal(val, t.right);
    return `(${tag} ${str})`;
  }
  if (v.tag === 'Clos') return `*closure*`;
  return '?';
};

export const init = () => {};
export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  try {
    if (_s === ':help' || _s === ':h')
      return _cb(HELP);
    if (_s === ':env' || _s === ':e')
      return _cb(showEnv(cenv.tenv));
    if (_s === ':showkinds' || _s === ':k') {
      config.showKinds = !config.showKinds;
      return _cb(`showKinds: ${config.showKinds}`);
    }
    if (_s === ':debug' || _s === ':d') {
      config.debug = !config.debug;
      return _cb(`debug: ${config.debug}`);
    }
    if (_s === ':time') {
      config.time = !config.time;
      return _cb(`time: ${config.time}`);
    }
    if (_s === ':reset' || _s === ':r') {
      cenv.importmap = {};
      cenv.tenv = getInitialEnv();
      cenv.venv = {};
      return _cb(`environment reset`);
    }
    _s = _s + '\n';
    if (_s.startsWith(':let ') || _s.startsWith(':type ') || _s.startsWith(':import ')) {
      const _rest = _s.slice(1);
      let ptime = Date.now();
      parseDefs(_rest, cenv.importmap).then(_ds => {
        ptime = Date.now() - ptime;
        let itime = Date.now();
        inferDefs(cenv.tenv, _ds);
        itime = Date.now() - itime;
        resetStepCount();
        let etime = Date.now();
        runEnv(_ds, cenv.venv);
        const esteps = stepCount;
        etime = Date.now() - etime;
        return _cb(`defined ${_ds.map(d => d.name).join(' ')}${config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/total:${ptime+itime+etime}ms(${esteps}steps))` : ''}`);
      }).catch(err => _cb(`${err}`, true));
      return;
    }
    if (_s.startsWith(':perf ')) {
      const rest = _s.slice(6);
      const p = parse(rest);
      const res: {
        val: number,
        evaltime: number,
        reify: number,
        evalSteps: number,
        reifySteps: number,
        total: number,
        totalSteps: number,
      }[] = [];
      for (let i = 0; i < 100; i++) {
        const e = App(p, LitNat(i, false));
        const t = infer(cenv.tenv, e);
        resetStepCount();
        let et = Date.now();
        const v = runVal(e, cenv.venv);
        et = Date.now() - et;
        const esteps = stepCount;
        resetStepCount();
        let vt = Date.now();
        const r = _showVal(v, t);
        vt = Date.now() - vt;
        const vsteps = stepCount;
        res.push({ val: i, evaltime: et, reify: vt, evalSteps: esteps, reifySteps: vsteps, total: et+vt, totalSteps: esteps+vsteps });
      }
      return _cb(res.map(({ val, evaltime, reify, evalSteps, reifySteps, total, totalSteps }) =>
        [val, evaltime, evalSteps, reify, reifySteps, total, totalSteps].join(',')).join('\n'));
    }
    if (_s.startsWith(':t ')) {
      const _rest = _s.slice(3);
      let ptime = Date.now();
      const _e = parse(_rest);
      ptime = Date.now() - ptime;
      let itime = Date.now();
      const _t = infer(cenv.tenv, _e);
      itime = Date.now() - itime;
      return _cb(`${showTy(_t)}${config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/total:${ptime+itime}ms)` : ''}`);
    }
    let ptime = Date.now();
    const _e = parse(_s);
    ptime = Date.now() - ptime;
    log(() => showTerm(_e));
    let itime = Date.now();
    const _t = infer(cenv.tenv, _e);
    itime = Date.now() - itime;
    log(() => showTy(_t));
    resetStepCount();
    let etime = Date.now();
    const _v = runVal(_e, cenv.venv);
    etime = Date.now() - etime;
    const esteps = stepCount;
    resetStepCount();
    let rtime = Date.now();
    const rv = _showVal(_v, _t);
    const rsteps = stepCount;
    rtime = Date.now() - rtime;
    return _cb(`${rv} : ${showTy(_t)}${config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/reification:${rtime}ms(${rsteps}steps)/total:${ptime+itime+etime+rtime}ms(${esteps+rsteps}steps))` : ''}`);
  } catch (_err) {
    log(() => _err);
    return _cb(`${_err}`, true);
  }
};


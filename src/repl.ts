import { config, log } from './config';
import { Env as TEnv, showEnv, getInitialEnv } from './env';
import { showTerm } from './terms';
import { showTy, Type, tforall, tfunFrom, TVar, TCon, TApp } from './types';
import { infer, inferDefs, tNat } from './inference';
import { parse, parseDefs, ImportMap } from './parser';
import { Def, showDef, findDef, findDefType } from './definitions';
import { kType } from './kinds';
import { GEnv, MClos, showMClos, makeClos, LNil, MAbs, MApp, MBVar, resetStepCount, stepCount, MState, MTop, mapp, MFVar, steps, MExec, showMState } from './machine';
import { reduceDefs, reduceTerm } from './compilerMachine';
import { Name, impossible } from './util';

const HELP = `
  commands :help :env :showKinds :debug :time :reset :let :type :import :t :perf :showdefs :showdef :showtype :eval
`.trim();

export type ReplState = { importmap: ImportMap, tenv: TEnv, venv: GEnv, defs: Def[] };
const cenv: ReplState = {
  importmap: {},
  tenv: getInitialEnv(),
  venv: {},
  defs: [],
};

const _part = MAbs(MApp(MBVar(1), MAbs(MApp(MApp(MBVar(1), MBVar(1)), MBVar(0)))));
const _ycomb = MAbs(MApp(_part, _part));
const _yval = makeClos(_ycomb, LNil);
const setupEnv = () => {
  cenv.tenv.global.unsafeFix = tforall([['t', kType]], tfunFrom([tfunFrom([TVar('t'), TVar('t')]), TVar('t')]));
  cenv.venv.unsafeFix = _yval;
};
setupEnv();

const _id = MAbs(MBVar(0));
const _iterBNat = MFVar('iterBNat');
const _if = MFVar('if');
const _casePair = MFVar('casePair');
const _caseSum = MFVar('caseSum');
const _foldr = MFVar('foldr');

const matchTCon = (t: Type, name: Name): t is TCon =>
  t.tag === 'TCon' && t.name === name;
const matchTApp = (t: Type, name: Name): t is TApp =>
  t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === name;
const matchTApp2 = (t: Type, name: Name): t is TApp =>
  t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
    t.left.left.name === name;
const reify = (v: MClos, t: Type): any => {
  if (matchTCon(t, 'Nat')) {
    let n = 0n;
    const mt = mapp(
      _iterBNat,
      v.abs,
      MAbs(_id),
      MAbs(MApp(MApp(MBVar(0), _id), MExec('twice', () => { n *= 2n; return true }, _id))),
      MAbs(MApp(MApp(MBVar(0), _id), MExec('twicePlusOne', () => { n = (n * 2n) + 1n; return true }, _id))),
    );
    const st = MState(mt, v.env, MTop);
    steps(cenv.venv, st);
    return n;
  }
  if (matchTCon(t, 'Int')) {
    const [a, b] = reify(v, TApp(TApp(TCon('Pair'), TCon('Nat')), TCon('Nat')));
    const na = reify(a, TCon('Nat'));
    const nb = reify(b, TCon('Nat'));
    return na - nb;
  }
  if (matchTCon(t, 'Rat')) {
    const [a, b] = reify(v, TApp(TApp(TCon('Pair'), TCon('Int')), TCon('Nat')));
    const na = reify(a, TCon('Int'));
    const nb = reify(b, TCon('Nat'));
    return [na, nb];
  }
  if (matchTCon(t, 'Bool')) {
    let b = false;
    const mt = mapp(
      _if,
      v.abs,
      MAbs(MExec('true', () => { b = true; return true }, _id)),
      _id,
    );
    const st = MState(mt, v.env, MTop);
    steps(cenv.venv, st);
    return b;
  }
  if (matchTApp2(t, 'Pair')) {
    let p: [any, any] = [null, null];
    const mt = mapp(
      _casePair,
      v.abs,
      MAbs(MAbs(mapp(
        _id,
        MExec('fst', st => { p[0] = makeClos(st.term as MAbs, st.env); return true }, MBVar(1)),
        MExec('snd', st => { p[1] = makeClos(st.term as MAbs, st.env); return true }, MBVar(0))))),
    );
    const st = MState(mt, v.env, MTop);
    steps(cenv.venv, st);
    return p;
  }
  if (matchTApp2(t, 'Sum')) {
    let s: [boolean, any] = [false, null];
    const mt = mapp(
      _caseSum,
      v.abs,
      MAbs(MExec('inl', st => { s[0] = true; s[1] = makeClos(st.term as MAbs, st.env); return true }, MBVar(0))),
      MAbs(MExec('inr', st => { s[1] = makeClos(st.term as MAbs, st.env); return true }, MBVar(0))),
    );
    const st = MState(mt, v.env, MTop);
    steps(cenv.venv, st);
    return s;
  }
  if (matchTApp(t, 'List')) {
    const a: MClos[] = [];
    const mt = mapp(
      _foldr,
      MAbs(MAbs(MExec('push', st => { a.push(makeClos(st.term as MAbs, st.env)); return true }, MBVar(1)))),
      _id,
      v.abs,
    );
    const st = MState(mt, v.env, MTop);
    steps(cenv.venv, st);
    return a.reverse();
  }
  if (matchTCon(t, 'Str')) {
    const l = reify(v, TApp(TCon('List'), TCon('Nat')));
    return l.map((v: MClos) => String.fromCharCode(Number(reify(v, TCon('Nat'))))).join('');
  }
  return impossible('reify');
};
const showVal = (v: MClos, t: Type): string => {
  if (matchTCon(t, 'Unit')) return '()';
  if (matchTCon(t, 'Bool')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Nat')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Int')) return `${reify(v, t)}`;
  if (matchTCon(t, 'Rat')) {
    const [a, b] = reify(v, t);
    return `${a}/${b}`;
  }
  if (matchTCon(t, 'Char')) return `'${JSON.stringify(String.fromCharCode(Number(reify(v, tNat)))).slice(1, -1)}'`;
  if (matchTApp2(t, 'Pair')) {
    const [a, b] = reify(v, t);
    const sa = showVal(a, (t.left as any).right);
    const sb = showVal(b, t.right);
    return `(${sa}, ${sb})`;
  }
  if (matchTApp2(t, 'Sum')) {
    const [a, b] = reify(v, t);
    const s = a ? showVal(b, (t.left as any).right) : showVal(b, t.right);
    return `(${a ? 'L' : 'R'} ${s})`;
  }
  if (matchTApp(t, 'List')) {
    return `[${reify(v, t).map((x: MClos) => showVal(x, t.right)).join(', ')}]`;
  }
  if (matchTCon(t, 'Str')) return JSON.stringify(reify(v, t));
  return showMClos(v);
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
      cenv.defs = [];
      setupEnv();
      return _cb(`environment reset`);
    }
    if (_s === ':showdefs') {
      return _cb(cenv.defs.map(showDef).join('\n'));
    }
    if (_s.startsWith(':showdef ')) {
      const name = _s.slice(8).trim();
      const def = findDef(cenv.defs, name);
      if (!def) return _cb(`def not found: ${name}`);
      return _cb(showDef(def));
    }
    if (_s.startsWith(':showtype')) {
      const name = _s.slice(9).trim();
      const def = findDefType(cenv.defs, name);
      if (!def) return _cb(`type not found: ${name}`);
      return _cb(showDef(def));
    }
    _s = _s + '\n';
    if (_s.startsWith(':let ') || _s.startsWith(':type ') || _s.startsWith(':import ')) {
      const _rest = _s.slice(1);
      let ptime = Date.now();
      const importmap = Object.assign({}, cenv.importmap);
      parseDefs(_rest, importmap).then(_ds => {
        ptime = Date.now() - ptime;
        let itime = Date.now();
        inferDefs(cenv.tenv, _ds);
        itime = Date.now() - itime;
        resetStepCount();
        let etime = Date.now();
        cenv.defs = cenv.defs.concat(_ds);
        reduceDefs(cenv.venv, _ds);
        const esteps = stepCount;
        etime = Date.now() - etime;
        cenv.importmap = importmap;
        return _cb(`defined ${_ds.map(d => d.name).join(' ')}${config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/total:${ptime+itime+etime}ms(${esteps}steps))` : ''}`);
      }).catch(err => _cb(`${err}`, true));
      return;
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
    if (_s.startsWith(':eval ')) {
      const rest = _s.slice(5);
      let ptime = Date.now();
      const _e = parse(rest);
      ptime = Date.now() - ptime;
      resetStepCount();
      let etime = Date.now();
      const _v = reduceTerm(cenv.venv, _e);
      etime = Date.now() - etime;
      const esteps = stepCount;
      return _cb(`${showMClos(_v)}${config.time ? ` (parsing:${ptime}ms/evaluation:${etime}ms(${esteps}steps))` : ''}`);
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
    const _v = reduceTerm(cenv.venv, _e);
    etime = Date.now() - etime;
    const esteps = stepCount;
    resetStepCount();
    let rtime = Date.now();
    const rv = showVal(_v, _t);
    const rsteps = stepCount;
    rtime = Date.now() - rtime;
    return _cb(`${rv} : ${showTy(_t)}${config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/reification:${rtime}ms(${rsteps}steps)/total:${ptime+itime+etime+rtime}ms(${esteps+rsteps}steps))` : ''}`);
  } catch (_err) {
    log(() => _err);
    return _cb(`${_err}`, true);
  }
};


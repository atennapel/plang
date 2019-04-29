import { config, log } from './config';
import { Env as TEnv, showEnv, getInitialEnv } from './env';
import { showTerm } from './terms';
import { showTy, tforall, tfunFrom, TVar } from './types';
import { infer, inferDefs } from './inference';
import { parse, parseDefs, ImportMap } from './parser';
import { Def, showDef, findDef, findDefType } from './definitions';
import { kType } from './kinds';
import { GEnv, showMClos, makeClos, LNil, MAbs, MApp, MBVar, resetStepCount, stepCount } from './machine';
import { reduceDefs, reduceTerm } from './compilerMachine';
import { showReifyClos } from './reification';

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
    const rv = showReifyClos(_v, _t, cenv.venv);
    const rsteps = stepCount;
    rtime = Date.now() - rtime;
    return _cb(`${rv} : ${showTy(_t)}${config.time ? ` (parsing:${ptime}ms/typechecking:${itime}ms/evaluation:${etime}ms(${esteps}steps)/reification:${rtime}ms(${rsteps}steps)/total:${ptime+itime+etime+rtime}ms(${esteps+rsteps}steps))` : ''}`);
  } catch (_err) {
    log(() => _err);
    return _cb(`${_err}`, true);
  }
};

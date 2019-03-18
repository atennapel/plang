import { showType } from './types';
import { compile, compileDefs } from './compiler';
import { infer, inferDefs } from './inference';
import { parse, parseDefs } from './parser';
import { context } from './global';
import { config, log } from './config';
import { showName } from './names';
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

const _prelude = require('fs').readFileSync(__dirname + '/../../lib/prelude.p', 'utf8');

const _env: string = typeof global === 'undefined' ? 'window' : 'global';
export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  if (_s === ':c' || _s === ':ctx' || _s === ':context') return _cb(`${context}`);
  if (_s === ':showkinds' || _s === ':k') {
    config.showKinds = !config.showKinds;
    return _cb(`showKinds: ${config.showKinds}`);
  }
  if (_s === ':logging' || _s === ':l') {
    config.logging = !config.logging;
    return _cb(`logging: ${config.logging}`);
  }
  if (_s.startsWith(':def ')) {
    try {
      const _rest = _s.slice(4).trim();
      const _ds = parseDefs(_rest);
      const _nds = inferDefs(_ds);
      const _c = compileDefs(_nds, n => `${_env}['${n}']`);
      eval(`(() => {${_c}})()`);
      return _cb(`defined ${_nds.map(d => showName(d.name)).join(' ')}`);
    } catch (err) {
      return _cb(`${err}`, true);
    }
  }
  if (_s === ':p' || _s === ':prelude') {
    try {
      const _ds = parseDefs(_prelude);
      const _nds = inferDefs(_ds);
      const _c = compileDefs(_nds, n => `${_env}['${n}']`);
      eval(`(() => {${_c}})()`);
      return _cb(`defined ${_nds.map(d => showName(d.name)).join(' ')}`);
    } catch (err) {
      return _cb(`${err}`, true);
    }
  }
  try {
    const _e = parse(_s);
    log(showTerm(_e));
    const [_t, _ne] = infer(_e);
    log(showType(_t));
    const _c = compile(_ne);
    log(_c);
    const _v = eval(_c);
    log(_v);
    return _cb(`${_show(_v)} : ${showType(_t)}`);
  } catch (err) {
    return _cb('' + err, true);
  }
};

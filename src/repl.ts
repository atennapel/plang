import { config, log } from './config';
import { initialEnv } from './env';
import { showTerm } from './terms';
import { showTy } from './types';
import { compile } from './compiler';
import { infer } from './inference';
import { parse } from './parser';

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
export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  if (_s === ':showkinds' || _s === ':k') {
    config.showKinds = !config.showKinds;
    return _cb(`showKinds: ${config.showKinds}`);
  }
  if (_s === ':debug' || _s === ':d') {
    config.debug = !config.debug;
    return _cb(`debug: ${config.debug}`);
  }
  try {
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

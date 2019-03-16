import { showType } from './types';
import { compile } from './compiler';
import { infer } from './inference';
import { parseTerm } from './parser';

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

export const run = (_s: string, _cb: (msg: string, err?: boolean) => void) => {
  try {
    const _e = parseTerm(_s);
    // console.log(showExpr(_e));
    const _t = infer(_e);
    // console.log(showType(_t));
    const _c = compile(_e);
    // console.log(_c);
    const _v = eval(_c);
    // console.log(_v);
    return _cb(`${_show(_v)} : ${showType(_t)}`);
  } catch (err) {
    return _cb('' + err, true);
  }
};


const { prettyType } = require('./types');
const { showExpr } = require('./exprs');
const { parse } = require('./parser');
const { compile } = require('./compiler');
const { infer } = require('./inference');

const _show = x => {
  if (typeof x === 'function') return '[Fn]';
  if (typeof x === 'string') return JSON.stringify(x);
  if (Array.isArray(x)) return `[${x.map(_show).join(', ')}]`;
  if (typeof x === 'object' && typeof x._tag === 'string') {
    if (x._tag === 'Pair') return `(Pair ${_show(x.val[0])} ${_show(x.val[1])})`;
    return x.val === null ? x._tag : `(${x._tag} ${_show(x.val)})`;
  }
  return '' + x;
};

const _env = {};
const _run = (_s, _cb) => {
  try {
    const _e = parse(_s);
    // console.log(showExpr(_e));
    const _t = infer(_env, _e);
    // console.log(showType(_t));
    const _c = compile(_e);
    // console.log(_c);
    const _v = eval(_c);
    // console.log(_v);
    return _cb(`${_show(_v)} : ${prettyType(_t)}`);
  } catch (err) {
    return _cb('' + err, true);
  }
};

module.exports = {
  run: _run,
};

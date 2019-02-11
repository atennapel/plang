const { showType } = require('./types');
const { showExpr } = require('./exprs');
const { parseExprTop } = require('./parser');
const { compile } = require('./compiler');
const { infer } = require('./inference');

const _show = x => {
  if (typeof x === 'function') return '[Fn]';
  if (typeof x === 'string') return JSON.stringify(x);
  if (Array.isArray(x)) return `[${x.map(_show).join(', ')}]`;
  return '' + x;
};

const _run = (_s, _cb) => {
  try {
    const _e = parseExprTop(_s);
    console.log(showExpr(_e));
    const _t = infer({}, {}, _e);
    console.log(showType(_t));
    const _c = compile(_e);
    console.log(_c);
    const _v = eval(_c);
    console.log(_v);
    return _cb(`${_show(_v)} : ${showType(_t)}`);
  } catch(err) {
    return _cb('' + err, true);
  }
};

module.exports = {
  run: _run,
};

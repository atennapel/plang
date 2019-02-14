const { showType } = require('./types');
const { showExpr } = require('./exprs');
const { parseDefs, parseExprTop } = require('./parser');
const { compile, compileDefsWeb } = require('./compiler');
const { infer, inferDefs } = require('./inference');

const _show = x => {
  if (typeof x === 'function') return '[Fn]';
  if (typeof x === 'string') return JSON.stringify(x);
  if (Array.isArray(x)) return `[${x.map(_show).join(', ')}]`;
  return '' + x;
};

const _tenv = {};
const _env = {};
const _run = (_s, _cb) => {
  if (_s.startsWith(':load ')) {
    const _ss = _s.slice(5).trim();
    try {
      return fetch(`https://atennapel.github.io/plang/lib/${_ss}.p`)
        .then(x => x.text())
        .then(_ss => {
          try {
            const _ds = parseDefs(_ss);
            console.log(_ds);
            inferDefs(_ds, _tenv, _env);
            console.log(_tenv, _env);
            const _c = compileDefsWeb(_ds);
            console.log(_c);
            eval(_c);
            return _cb('done');
          } catch (err) {
            return _cb('' + err, true);
          }
        })
        .catch(e => _cb(''+e, true));
    } catch (err) {
      return _cb('' + err, true);
    }
  }if (_s.startsWith(':loadfull ')) {
    const _ss = _s.slice(9).trim();
    try {
      return fetch(_ss)
        .then(x => x.text())
        .then(_ss => {
          try {
            const _ds = parseDefs(_ss);
            console.log(_ds);
            inferDefs(_ds, _tenv, _env);
            console.log(_tenv, _env);
            const _c = compileDefsWeb(_ds);
            console.log(_c);
            eval(_c);
            return _cb('done');
          } catch (err) {
            return _cb('' + err, true);
          }
        })
        .catch(e => _cb(''+e, true));
    } catch (err) {
      return _cb('' + err, true);
    }
  } else if (_s.startsWith(':loadfile ')) {
    const _f = _s.slice(9).trim();
    require('fs').readFile(`./lib/${_f}.p`, 'utf8', (err, _ss) => {
      try {
        if (err) throw err;
        const _ds = parseDefs(_ss);
        console.log(_ds);
        inferDefs(_ds, _tenv, _env);
        console.log(_tenv, _env);
        const _c = compileDefsWeb(_ds);
        console.log(_c);
        eval(_c);
        return _cb('done');
      } catch (err) {
        return _cb('' + err, true);
      }
    });
  } else if (_s.startsWith(':nat ')) {
    const _ss = _s.slice(4).trim();
    try {
      const _e = parseExprTop(_ss);
      console.log(showExpr(_e));
      const _t = infer(_tenv, _env, _e);
      console.log(showType(_t));
      const _c = compile(_e);
      console.log(_c);
      const _v = eval(_c);
      console.log(_v);
      return _cb(`${_show(_v(x => x + 1)(0))} : ${showType(_t)}`);
    } catch (err) {
      return _cb('' + err, true);
    }
  } else if (_s.startsWith(':let ')) {
    try {
      const _ss = _s.slice(4).trim();
      const _ds = parseDefs(_ss);
      console.log(_ds);
      inferDefs(_ds, _tenv, _env);
      console.log(_tenv, _env);
      const _c = compileDefsWeb(_ds);
      console.log(_c);
      eval(_c);
      return _cb('done');
    } catch (err) {
      return _cb('' + err, true);
    }
  } else {
    try {
      const _e = parseExprTop(_s);
      console.log(showExpr(_e));
      const _t = infer(_tenv, _env, _e);
      console.log(showType(_t));
      const _c = compile(_e);
      console.log(_c);
      const _v = eval(_c);
      console.log(_v);
      return _cb(`${_show(_v)} : ${showType(_t)}`);
    } catch (err) {
      return _cb('' + err, true);
    }
  }
};

module.exports = {
  run: _run,
};

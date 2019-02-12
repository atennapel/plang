(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
const keywords = `
do
if
in
for
let
new
try
var
case
else
enum
eval
null
this
true
void
with
await
break
catch
class
const
false
super
throw
while
yield
delete
export
import
public
return
static
switch
typeof
default
extends
finally
package
private
continue
debugger
function
arguments
interface
protected
implements
instanceof
`.trim().split(/\s+/);

const compileName = x => keywords.indexOf(x) >= 0 ? `${x}_` : x;

const compile = e => {
  switch (e.tag) {
    case 'Var': return compileName(e.name);
    case 'Abs':
      return `(${compileName(e.name)} => ${compile(e.body)})`;
    case 'App':
      return `${compile(e.left)}(${compile(e.right)})`;
    case 'Con': return compile(e.arg);
    case 'Decon':
      return `(${compileName(e.name)} => ${compile(e.body)})`;
  }
};

const compileDefs = ds =>
  ds.map(d => d.tag === 'DType' ? null : `const ${compileName(d.name)} = ${compile(d.expr)};`)
    .filter(x => x !== null)
    .join('\n');

const compileDefsWeb = ds => '(() => {' +
  ds.map(d => d.tag === 'DType' ? null : `window['${compileName(d.name)}'] = ${compile(d.expr)};`)
    .filter(x => x !== null)
    .join('\n') + '})()';

module.exports = {
  compileDefs,
  compileDefsWeb,
  compile,
};

},{}],2:[function(require,module,exports){
const DType = (name, tvs, utvs, etvs, type) => ({ tag: 'DType', name, tvs, utvs, etvs, type });
const DValue = (name, expr) => ({ tag: 'DValue', name, expr });

module.exports = {
  DType,
  DValue,
};

},{}],3:[function(require,module,exports){
const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });
const Con = (con, arg) => ({ tag: 'Con', con, arg });
const Decon = (con, name, body) => ({ tag: 'Decon', con, name, body });

const showExpr = e => {
  switch (e.tag) {
    case 'Var': return e.name;
    case 'Abs': return `(\\${e.name}. ${showExpr(e.body)})`;
    case 'App':
      return `(${showExpr(e.left)} ${showExpr(e.right)})`;
    case 'Con': return `(${e.con} ${showExpr(e.arg)})`;
    case 'Decon': return `(\\${e.con} ${e.name}. ${showExpr(e.body)})`;
  }
};

module.exports = {
  Var,
  Abs,
  App,
  Con,
  Decon,
  showExpr,
};

},{}],4:[function(require,module,exports){
const {
  resetId,
  TCon,
  TVar,
  TApp,
  TFun,
  tapp,
  freshTMeta,
  prune,
  occursAny,
  showType,
  freeTVars,
} = require('./types');
const { showExpr } = require('./exprs');
const { unify } = require('./unification');

const extend = (env, x, t) => {
  const n = Object.create(env);
  n[x] = t;
  return n;
};
const inst = (t, map = {}) => {
  if (t.tag === 'TVar')
    return map[t.id] || (map[t.id] = freshTMeta());
  if (t.tag === 'TApp')
    return TApp(inst(t.left, map), inst(t.right, map));
  return t;
};
const gen = t => {
  if (t.tag === 'TMeta') return TVar(t.id);
  if (t.tag === 'TApp')
    return TApp(gen(t.left), gen(t.right));
  return t;
};
const escapeCheckType = (tvs, ty, msg) => {
  const tv = occursAny(tvs, prune(ty));
  if (tv) throw new TypeError(msg(tv));
};
const escapeCheckEnv = (tvs, env, expr) => {
  for (let x in env) {
    const ty = prune(env[x]);
    escapeCheckType(tvs, ty,
      tv => `skolem ${showType(tv)} escaped in ${x} : ${showType(ty)} in ${showExpr(expr)}`);
  }
};
const synth = (tenv, env, e, skol = {}) => {
  // console.log(`synth ${e.tag} ${showExpr(e)}`);
  switch (e.tag) {
    case 'Var': {
      if (!env[e.name]) throw new TypeError(`undefined variable ${e.name}`);
      return inst(prune(env[e.name]));
    }
    case 'Abs': {
      const tv = freshTMeta();
      const t = synth(tenv, extend(env, e.name, tv), e.body, skol);
      return TFun(prune(tv), t);
    }
    case 'App': {
      const ta = synth(tenv, env, e.left, skol);
      const tb = synth(tenv, env, e.right, skol);
      const tv = freshTMeta();
      unify(ta, TFun(tb, tv), skol);
      return prune(tv);
    }
    case 'Con': {
      const data = tenv[e.con];
      if (!data) throw new TypeError(`undefined constructor: ${e.con}`);
      const tms = data.tvs.map(() => freshTMeta());
      const etms = data.etvs.map(() => freshTMeta());
      const utms = data.utvs.map(() => freshTMeta());
      const map = {};
      const nskol = Object.create(skol);
      for (let i = 0, l = data.tvs.length; i < l; i++) map[data.tvs[i]] = tms[i];
      for (let i = 0, l = data.etvs.length; i < l; i++) map[data.etvs[i]] = etms[i];
      for (let i = 0, l = data.utvs.length; i < l; i++) {
        map[data.utvs[i]] = utms[i];
        nskol[utms[i].id] = true;
      }
      const ty = synth(tenv, env, e.arg, skol);
      unify(inst(data.type, map), ty, nskol);
      escapeCheckEnv(utms, env, e);
      return tapp(data.tcon, tms.map(prune));
    }
    case 'Decon': {
      const data = tenv[e.con];
      if (!data) throw new TypeError(`undefined constructor: ${e.con}`);
      const tms = data.tvs.map(() => freshTMeta());
      const etms = data.etvs.map(() => freshTMeta());
      const utms = data.utvs.map(() => freshTMeta());
      const map = {};
      const nskol = Object.create(skol);
      for (let i = 0, l = data.tvs.length; i < l; i++) map[data.tvs[i]] = tms[i];
      for (let i = 0, l = data.etvs.length; i < l; i++) {
        map[data.etvs[i]] = etms[i];
        nskol[etms[i].id] = true;
      }
      for (let i = 0, l = data.utvs.length; i < l; i++) map[data.utvs[i]] = utms[i];
      const tr = synth(tenv, extend(env, e.name, inst(data.type, map)), e.body, nskol);
      escapeCheckEnv(etms, env, e);
      escapeCheckType(etms, tr, tv => `skolem ${showType(tv)} escaped in ${showExpr(e)}`);
      utms.forEach(t =>
        escapeCheckType(etms, t, tv => `skolem ${showType(tv)} escaped in ${showExpr(e)}`));
      return TFun(tapp(data.tcon, tms.map(prune)), tr);
    }
  }
};

const simplify = (t, map = {}, next = { id: 0 }) => {
  if (t.tag === 'TVar')
    return map[t.id] || (map[t.id] = TVar(next.id++));
  if (t.tag === 'TApp')
    return TApp(simplify(t.left, map, next), simplify(t.right, map, next));
  return t;
};
const infer = (tenv, env, e) => {
  resetId();
  return simplify(gen(synth(tenv, env, e)));
};
const inferDefs = (ds, tenv = {}, env = {}) => {
  for (let i = 0, l = ds.length; i < l; i++) {
    const d = ds[i];
    switch (d.tag) {
      case 'DType':
        tenv[d.name] = {
          tcon: TCon(d.name),
          tvs: d.tvs,
          etvs: d.etvs,
          utvs: d.utvs,
          type: d.type,
        };
        break;
      case 'DValue':
        env[d.name] = infer(tenv, env, d.expr);
        break;
    }
  }
  return env;
};

module.exports = {
  infer,
  inferDefs,
};

},{"./exprs":3,"./types":7,"./unification":8}],5:[function(require,module,exports){
const {
  Var,
  Abs,
  App,
  Con,
  Decon,
  showExpr,
} = require('./exprs');
const {
  TCon,
  TVar,
  TApp,
  TFun,
  TFunC,
  tfuns,
  freeTVars,
} = require('./types');
const {
  DType,
  DValue,
} = require('./defs');

const SYMBOLS = '()\\.=';
const START = 0;
const NAME = 1;
const tokenize = s => {
  let state = START;
  const r = [];
  let t = '';
  for (let i = 0, l = s.length; i <= l; i++) {
    const c = s[i] || ' ';
    const next = s[i+1] || '';
    if (state === START) {
      if (c + next === '->') r.push(c + next), i++;
      else if (SYMBOLS.indexOf(c) >= 0) r.push(c);
      else if (/[a-z]/i.test(c)) t += c, state = NAME;
      else if (/\s/.test(c)) continue;
      else throw new SyntaxError(`unexpected char ${c}`);
    } else if (state === NAME) {
      if (!/[a-z]/i.test(c))
        r.push(t), t = '', i--, state = START;
      else t += c;
    }
  }
  if (state !== START)
    throw new SyntaxError('invalid end state');
  return r;
};

const matchfn = (a, fn) => {
  if (a.length && fn(a[a.length - 1]))
    return true;
  return false;
};
const match = (a, x) => {
  if (a.length && a[a.length - 1] === x) {
    a.pop();
    return true;
  }
  return false;
};

const tvar = (map, x) => map[x] || (map[x] = TVar(map._id++));

const parseType = (a, tvmap = { _id: 0 }, tvs = [], utvs = [], etvs = []) => {
  if (a.length === 0) throw new SyntaxError('empty type');
  if (match(a, '(')) {
    const es = [];
    while (true) {
      if (a.length === 0) throw new SyntaxError('missing )');
      if (match(a, ')')) break;
      es.push(parseType(a, tvmap, tvs, utvs, etvs));
    }
    if (es.length === 0) throw new SyntaxError('empty');
    if (es.indexOf(TFunC) === -1) return es.reduce(TApp);
    const r = [];
    let c = [];
    while (es.length > 0) {
      if (es[es.length - 1] === TFunC) {
        es.pop();
        r.push(c);
        c = [];
      } else c.push(es.pop());
    }
    r.push(c);
    r.reverse();
    if (r.length === 2 && r[0].length === 0) return TApp(TFunC, r[1].reduce(TApp));
    return tfuns(r.map(a => {
      if (a.length === 0) throw new SyntaxError('empty');
      return a.reverse().reduce(TApp);
    }));
  } else if(match(a, '->')) return TFunC;
  else if (matchfn(a, x => !/[a-z]/i.test(x[0])))
    throw new SyntaxError(`unexpected ${a.pop()}`);
  const x = a.pop();
  if (!/[a-z]/.test(x[0])) return TCon(x);
  const tv = tvar(tvmap, x);
  if (tvs.indexOf(tv.id) === -1) {
    let target = x[0] === 'x' && x.length > 1 ? etvs : utvs;
    if (target.indexOf(tv.id) === -1) target.push(tv.id);
  }
  return tv;
};

const parseTypeTop = (x, a) => {
  if (match(a, '\\')) {
    const args = [];
    while (!match(a, '.')) args.push(parseName(a));
    if (args.length === 0)
      throw new SyntaxError('empty type parameter list');
    let map = { _id: 0 };
    const tvs = [];
    for (let i = 0, l = args.length; i < l; i++) {
      if (/[A-Z]/.test(args[i][0]))
        throw new SyntaxError(`constructor in type parameter: ${args[i]}`);
      const tv = tvar(map, args[i]);
      tvs.push(tv.id);
    }
    a.push('('); a.unshift(')');
    const utvs = [];
    const etvs = [];
    const ty = parseType(a, map, tvs, utvs, etvs);
    return DType(x, tvs, utvs, etvs, ty);
  } else {
    const utvs = [];
    const etvs = [];
    const ty = parseType(a, undefined, [], utvs, etvs);
    return DType(x, [], utvs, etvs, ty);
  }
};

const parseExpr = a => {
  if (a.length === 0) throw new SyntaxError('empty');
  if (match(a, '(')) {
    const es = [];
    while (true) {
      if (a.length === 0) throw new SyntaxError('missing )');
      if (match(a, ')')) break;
      es.push(parseExpr(a));
    }
    if (es.length === 0) throw new SyntaxError('empty');
    const head = es[0];
    if (head.tag === 'Var' && /[A-Z]/.test(head.name[0])) {
      if (es.length !== 2)
        throw new SyntaxError(`constructor ${head.name} takes 1 argument, but ${es.length - 1} given.`);
      return Con(head.name, es[1]);
    }
    return es.reduce(App);
  } else if (match(a, '\\')) {
    const args = [];
    while (!match(a, '->')) args.push(parseName(a));
    if (args.length === 0)
      throw new SyntaxError('abs without parameters');
    for (let i = 1, l = args.length; i < l; i++)
      if (/[A-Z]/.test(args[i][0]))
        throw new SyntaxError(`constructor in abs argument: ${args[i]}`);
    const es = [];
    while (true) {
      if (a.length === 0) break;
      if (a[a.length - 1] === ')') break;
      es.push(a.pop());
    }
    es.unshift('('); es.push(')');
    const body = parseExpr(es.reverse());
    if (/[A-Z]/.test(args[0][0])) {
      if (args.length !== 2)
        throw new SyntaxError(`deconstructor ${args[0]} expects 1 argument but got ${args.length - 1}`);
      return Decon(args[0], args[1], body);
    }
    return args.reduceRight((x, y) => Abs(y, x), body);
  } else if (matchfn(a, x => !/[a-z]/i.test(x[0])))
    throw new SyntaxError(`unexpected ${a.pop()}`);
  return Var(a.pop());
};

const parseName = ts => {
  if (ts.length === 0)
    throw new SyntaxError('name expected but got nothing');
  const x = ts.pop();
  if (!/[a-z]/i.test(x))
    throw new SyntaxError(`name expected but got ${x}`);
  return x;
};

const parseDef = ts => {
  const x = parseName(ts);
  if (!match(ts, '='))
    throw new SyntaxError(`= missing after definition name`);
  const body = [];
  let found = true;
  while (!match(ts, '=')) {
    if (ts.length === 0) {
      found = false;
      break;
    }
    body.push(ts.pop());
  }
  if (found) ts.push('=', body.pop());
  if (/[a-z]/.test(x[0])) {
    body.unshift('('); body.push(')');
    body.reverse();
    return DValue(x, parseExpr(body));
  }
  if (body[0] !== '\\') {
    body.unshift('('); body.push(')');
  }
  body.reverse();
  return parseTypeTop(x, body);
};

const parseDefs = s => {
  const ts = tokenize(s).reverse();
  const ds = [];
  while (ts.length > 0) {
    const d = parseDef(ts);
    ds.push(d);
  }
  return ds;
};

const parseExprTop = s => {
  const ts = tokenize(s);
  ts.unshift('('); ts.push(')');
  return parseExpr(ts.reverse());
};

module.exports = {
  parseDefs,
  parseDef,
  parseExprTop,
};

},{"./defs":2,"./exprs":3,"./types":7}],6:[function(require,module,exports){
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

},{"./compiler":1,"./exprs":3,"./inference":4,"./parser":5,"./types":7}],7:[function(require,module,exports){
let _id = 0;
const fresh = () => _id++;
const resetId = () => { _id = 0 };

const TCon = name => ({ tag: 'TCon', name });
const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TApp = (left, right) => ({ tag: 'TApp', left, right });

const tapp = (fn, args) => args.reduce(TApp, fn);

const freshTMeta = () => TMeta(fresh());

const TFunC = TCon('->');
const TFun = (left, right) => TApp(TApp(TFunC, left), right);
const tfuns = a => a.reduceRight((x, y) => TFun(y, x));

const showType = t => {
  switch (t.tag) {
    case 'TCon': return t.name;
    case 'TVar': return `'${t.id}`;
    case 'TMeta': return `?${t.id}`;
    case 'TApp':
      return t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
        !/[a-z]/i.test(t.left.left.name) ?
        `(${showType(t.left.right)} ${t.left.left.name} ${showType(t.right)})` :
        `(${showType(t.left)} ${showType(t.right)})`;
  }
};

const prune = t => {
  if (t.tag === 'TMeta') {
    if (!t.type) return t;
    const ty = prune(t.type);
    t.type = ty;
    return ty;
  }
  if (t.tag === 'TApp') return TApp(prune(t.left), prune(t.right));
  return t;
};

const occurs = (v, t) => {
  if (v === t) return true;
  if (t.tag === 'TApp') return occurs(v, t.left) || occurs(v, t.right);
  return false;
};
const occursAny = (vs, t) => {
  if (vs.indexOf(t) >= 0) return t;
  if (t.tag === 'TApp') return occursAny(vs, t.left) || occursAny(vs, t.right);
  return null;
};

const freeTVars = (t, a = []) => {
  if (t.tag === 'TVar') {
    if (a.indexOf(t) >= 0) return a;
    a.push(t);
    return a;
  }
  if (t.tag === 'TApp') {
    freeTVars(t.left, a);
    freeTVars(t.right, a);
    return a;
  }
  return a;
};

module.exports = {
  resetId,

  TCon,
  TVar,
  TMeta,
  TApp,

  tapp,

  freshTMeta,

  TFunC,
  TFun,
  tfuns,

  showType,

  prune,
  occurs,
  occursAny,

  freeTVars,
};

},{}],8:[function(require,module,exports){
const {
  showType,
  prune,
  occurs,
} = require('./types');

const bind = (v, t) => {
  if (occurs(v, t)) throw new TypeError(`${showType(v)} occurs in ${showType(t)}`);
  v.type = t;
};

const unify = (a_, b_, skol = {}) => {
  if (a_ === b_) return;
  const a = prune(a_);
  const b = prune(b_);
  // console.log(`${a.tag === 'TMeta' && skol[a.id] ? 'skolem ' : ''}${showType(a)} ~ ${b.tag === 'TMeta' && skol[b.id] ? 'skolem ' : ''}${showType(b)}`);
  if (a === b) return;
  if (a.tag === 'TMeta' && !skol[a.id]) return bind(a, b);
  if (b.tag === 'TMeta' && !skol[b.id]) return bind(b, a);
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left, skol);
    unify(a.right, b.right, skol);
    return;
  }
  if (a.tag === 'TVar' && b.tag === 'TVar' && a.id === b.id) return;
  if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name) return;
  if (a.tag === 'TMeta' && b.tag === 'TMeta' && a.id === b.id) return;
  throw new TypeError(`cannot unify ${a.tag === 'TMeta' && skol[a.id] ? 'skolem ' : ''}${showType(a)} ~ ${b.tag === 'TMeta' && skol[b.id] ? 'skolem ' : ''}${showType(b)}`);
};

module.exports = {
  unify,
};

},{"./types":7}],9:[function(require,module,exports){
const { run } = require('./repl');

function getOutput(s, cb) {
  run(s, cb);
}

var hist = [], index = -1;
var input = document.getElementById('input');
var content = document.getElementById('content');
function onresize() {
	content.style.height = window.innerHeight;
}
window.addEventListener('resize', onresize);
onresize();
addResult("REPL");
input.focus();
input.onkeydown = function(keyEvent) {
	var val = input.value;
	var txt = (val || '').trim();
	if(keyEvent.keyCode === 13) {
		keyEvent.preventDefault();
		if(txt) {
			hist.push(val);
			index = hist.length;
			input.value = '';
			var div = document.createElement('div');
			div.innerHTML = val;
			div.className = 'line input';
			content.insertBefore(div, input);
			getOutput(txt, addResult);
		}
	} else if(keyEvent.keyCode === 38 && index > 0) {
		keyEvent.preventDefault();
		input.value = hist[--index];
	} else if(keyEvent.keyCode === 40 && index < hist.length-1) {
		keyEvent.preventDefault();
		input.value = hist[++index];
	} else if(keyEvent.keyCode === 40 && keyEvent.ctrlKey && index >= hist.length-1) {
		index = hist.length;
		input.value = '';
	}
}

function addResult(msg, err) {
	var divout = document.createElement('pre');
	divout.className = 'line output';
	if(err) divout.className += ' error';
	divout.innerHTML = '' + msg;
	content.insertBefore(divout, input);
	input.focus();
	content.scrollTop = content.scrollHeight;
	return divout;
}

},{"./repl":6}]},{},[9]);

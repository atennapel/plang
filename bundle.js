(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
const compile = e => {
  if (e.tag === 'Var') return e.name;
  if (e.tag === 'Abs') return `(${e.name} => ${compile(e.body)})`;
  if (e.tag === 'App') return `${compile(e.left)}(${compile(e.right)})`;
  if (e.tag === 'Let') return `(${e.name} => ${compile(e.body)})(${compile(e.val)})`;
  if (e.tag === 'Con') return compile(e.arg);
  if (e.tag === 'Decon') return `(${e.name} => ${compile(e.body)})`;
};

module.exports = {
  compile,
};

},{}],2:[function(require,module,exports){
const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });
const Let = (name, val, body) => ({ tag: 'Let', name, val, body });
const Con = (con, arg) => ({ tag: 'Con', con, arg });
const Decon = (con, name, body) => ({ tag: 'Decon', con, name, body });

const showExpr = e => {
  if (e.tag === 'Var') return e.name;
  if (e.tag === 'Abs') return `(\\${e.name} -> ${showExpr(e.body)})`;
  if (e.tag === 'App') return `(${showExpr(e.left)} ${showExpr(e.right)})`;
  if (e.tag === 'Let') return `(let ${e.name} = ${showExpr(e.val)} in ${showExpr(e.body)})`;
  if (e.tag === 'Con') return `(${e.con} ${showExpr(e.arg)})`;
  if (e.tag === 'Decon') return `\\(${e.con} ${e.name}) -> ${showExpr(e.body)}`;
};

module.exports = {
  Var,
  Abs,
  App,
  Let,
  Con,
  Decon,

  showExpr,
};

},{}],3:[function(require,module,exports){
const {
  TVar,
  TApp,
  resetTMeta,
  freshTMeta,
  TFun,
  pruneType,
  tmetas,
} = require('./types');
const { kType } = require('./kinds');
const { unify } = require('./unification');
const { checkKindType } = require('./kindInference');

const err = msg => { throw new TypeError(msg) };

const tmetasInEnv = (env, map = {}) => {
  for (let k in env) tmetas(env[k], map);
  return map;
};

const inst = (t, map = {}) => {
  if (t.tag === 'TVar') {
    if (map[t.name]) return map[t.name];
    const tv = freshTMeta(t.kind);
    map[t.name] = tv;
    return tv;
  }
  if (t.tag === 'TApp') {
    const a = inst(t.left, map);
    const b = inst(t.right, map);
    return a !== t.left || b !== t.right ? TApp(a, b) : t;
  }
  return t;
};
const gen = (t, tvs = {}, map = {}) => {
  if (t.tag === 'TMeta') {
    if (tvs[t.id]) return t;
    if (map[t.id]) return map[t.id];
    const tv = TVar(`t${t.id}`, t.kind);
    map[t.id] = tv;
    return tv;
  }
  if (t.tag === 'TApp') {
    const a = gen(t.left, tvs, map);
    const b = gen(t.right, tvs, map);
    return a !== t.left || b !== t.right ? TApp(a, b) : t;
  }
  return t;
};

const synth = (env, e) => {
  if (e.tag === 'Var') {
    if (!env[e.name]) return err(`undefined variable: ${e.name}`);
    return inst(env[e.name]);
  }
  if (e.tag === 'Abs') {
    const old = env[e.name];
    const tv = freshTMeta(kType);
    env[e.name] = tv;
    const tr = synth(env, e.body);
    if (old) env[e.name] = old;
    else delete env[e.name];
    return TFun(pruneType(tv), tr);
  }
  if (e.tag === 'App') {
    const a = synth(env, e.left);
    const b = synth(env, e.right);
    const r = freshTMeta(kType);
    unify(a, TFun(b, r));
    return pruneType(r);
  }
  if (e.tag === 'Let') {
    const ty = synth(env, e.val);
    const old = env[e.name];
    env[e.name] = gen(ty, tmetasInEnv(env));
    const tr = synth(env, e.body);
    if (old) env[e.name] = old;
    else delete env[e.name];
    return tr;
  }
  return err('unimplemented');
};

const infer = (env, e) => {
  resetTMeta();
  const ty = synth(env, e);
  checkKindType(ty);
  return gen(ty);
};

module.exports = {
  infer,
};

},{"./kindInference":4,"./kinds":6,"./types":9,"./unification":10}],4:[function(require,module,exports){
const { KFun, freshKMeta, kType, kRow, pruneKind } = require('./kinds');
const { showType } = require('./types');
const { unifyKinds } = require('./kindUnification');

const kRowExtend = KFun(kType, KFun(kRow, kRow));

const inferKind = t => {
  // console.log(`inferKind ${showType(t)}`);
  if (t.tag === 'TApp') {
    const ka = inferKind(t.left);
    const kb = inferKind(t.right);
    const kr = freshKMeta();
    unifyKinds(ka, KFun(kb, kr));
    return pruneKind(kr);
  }
  if (t.tag === 'TRowExtend') return kRowExtend;
  return t.kind;
};

const checkKindType = t => unifyKinds(inferKind(t), kType);

module.exports = {
  inferKind,
  checkKindType,
};

},{"./kindUnification":5,"./kinds":6,"./types":9}],5:[function(require,module,exports){
const {
  showKind,
  pruneKind,
} = require('./kinds');

const err = msg => { throw new TypeError(msg) };

const occurs = (v, t) => {
  if (v === t) return err(`occurs check failed in kind: ${showKind(v)} in ${showKind(t)}`);
  if (t.tag === 'KFun') {
    occurs(v, t.left);
    occurs(v, t.right);
  }
};

const bind = (v, t) => {
  occurs(v, t);
  v.kind = t;
};

const unifyKinds = (a_, b_) => {
  const a = pruneKind(a_);
  const b = pruneKind(b_);
  if (a === b) return;
  // console.log(`${showKind(a)} ~k ${showKind(b)}`);
  if (a.tag === 'KMeta') return bind(a, b);
  if (b.tag === 'KMeta') return bind(b, a);
  if (a.tag === 'KFun' && b.tag === 'KFun') {
    unifyKinds(a.left, b.left);
    return unifyKinds(a.right, b.right);
  }
  return err(`failed to unify kinds: ${showKind(a)} ~ ${showKind(b)}`);
};

module.exports = {
  unifyKinds,
};

},{"./kinds":6}],6:[function(require,module,exports){
const KCon = name => ({ tag: 'KCon', name });
const KMeta = id => ({ tag: 'KMeta', id, kind: null });
const KFun = (left, right) => ({ tag: 'KFun', left, right });

let _idKMeta = 0;
const resetKMeta = () => { _idKMeta = 0 };
const freshKMeta = () => KMeta(_idKMeta++);

const kType = KCon('Type');
const kRow = KCon('Row');

const showKind = k => {
  if (k.tag === 'KCon') return k.name;
  if (k.tag === 'KMeta') return `?${k.id}`;
  if (k.tag === 'KFun') return `(${showKind(k.left)} -> ${showKind(k.right)})`;
};

const flattenKFun = k => {
  let c = k;
  const r = [];
  while (c.tag === 'KFun') {
    r.push(c.left)
    c = c.right;
  }
  r.push(c);
  return r;
};

const prettyKind = k => {
  if (k.tag === 'KCon') return k.name;
  if (k.tag === 'KMeta') return `?${k.id}`;
  if (k.tag === 'KFun')
    return flattenKFun(k)
      .map(x => x.tag === 'KFun' ? `(${prettyKind(x)})` : prettyKind(x))
      .join(' -> ');
};

const pruneKind = k => {
  if (k.tag === 'KMeta') {
    if (!k.kind) return k;
    const ki = pruneKind(k.kind);
    k.kind = ki;
    return ki;
  }
  if (k.tag === 'KFun') {
    const a = pruneKind(k.left);
    const b = pruneKind(k.right);
    return a !== k.left || b !== k.right ? KFun(a, b) : k;
  }
  return k;
};

module.exports = {
  KCon,
  KMeta,
  KFun,

  resetKMeta,
  freshKMeta,

  flattenKFun,
  prettyKind,

  kType,
  kRow,

  showKind,

  pruneKind,
};

},{}],7:[function(require,module,exports){
const {
  Var,
  Abs,
  App,
  Let,
  Con,
  Decon,
} = require('./exprs');

const err = msg => { throw new SyntaxError(msg) };

// tokens
const SymbolT = val => ({ tag: 'SymbolT', val });
const VarT = val => ({ tag: 'VarT', val });
const ConT = val => ({ tag: 'ConT', val});
const KeywordT = val => ({ tag: 'KeywordT', val });

const showTokens = ts =>
  ts.map(x => `${x.val}`).join(' ');

const SYM1 = ['(', ')', '\\', '='];
const SYM2 = ['->'];
const KEYWORDS = ['let', 'in'];

const START = 0;
const NAME = 1;
const tokenize = sc => {
  let state = START;
  const r = [];
  let t = '';
  for (let i = 0, l = sc.length; i <= l; i++) {
    const c = sc[i] || ' ';
    const next = sc[i + 1] || '';
    // console.log(`${i};${c};${next};${state}`, r);
    if (state === START) {
      if (SYM2.indexOf(c + next) >= 0) r.push(SymbolT(c + next)), i++;
      else if (SYM1.indexOf(c) >= 0) r.push(SymbolT(c));
      else if (/[a-z]/i.test(c)) t += c, state = NAME;
      else if (/\s/.test(c)) continue;
      else return err(`invalid char ${c} in tokenize`);
    } else if (state === NAME) {
      if (!/[a-z0-9]/i.test(c)) {
        r.push(
          KEYWORDS.indexOf(t) >= 0 ? KeywordT(t) :
          /[a-z]/.test(t[0]) ? VarT(t) :
          ConT(t));
        t = '', i--, state = START;
      } else t += c;
    }
  }
  if (state !== START) return err('invalid tokenize end state');
  return r;
};

// patterns
const PVar = val => ({ tag: 'PVar', val });
const PCon = (con, val) => ({ tag: 'PCon', con, val });

// parser
const match = (ts, tag, val = null) => {
  if (ts.length === 0) return false;
  const top = ts[ts.length - 1];
  if (top.tag === tag && (!val || top.val === val)) {
    ts.pop();
    return true;
  }
  return false;
};
const safeMatch = (ts, tag, val = null) => {
  if (ts.length === 0) return false;
  const top = ts[ts.length - 1];
  if (top.tag === tag && (!val || top.val === val))
    return true;
  return false;
};

const parseArg = ts => {
  const x = ts.pop();
  if (x.tag === 'VarT') return PVar(x.val);
  if (x.tag === 'SymbolT' && x.val === '(') {
    const con = ts.pop();
    if (con.tag !== 'ConT') return err(`not a con in argument: ${con.val}`);
    const arg = ts.pop();
    if (arg.tag !== 'VarT') return err(`not a valid arg in (${con.val} ${arg.val})`);
    if (!match(ts, 'SymbolT', ')')) return err(`missing ) in (${con.val} ${arg.val}`);
    return PCon(con.val, arg.val);
  }
  err(`invalid arg: ${x.val}`);
};

const parseAppTo = (ts, fn) => {
  const es = [];
  while (fn(ts)) es.push(parseExpr(ts));
  if (es.length === 0) return err('empty app');
  if (es.length === 1) return es[0];
  return es.reduce(App);
};
const parseApp = ts => parseAppTo(ts, ts => {
  if (match(ts, 'SymbolT', ')')) return false;
  if (ts.length === 0) return err('app end');
  return true;
});
const parseAppAll = ts => parseAppTo(ts, ts => {
  if (ts.length === 0 ||
    safeMatch(ts, 'SymbolT', ')') ||
    safeMatch(ts, 'KeywordT', 'in'))
    return false;
  return true;
});

const parseExpr = ts => {
  // console.log(showTokens(ts.slice(0).reverse()));
  if (ts.length === 0) return err('empty expr');
  if (match(ts, 'SymbolT', '\\')) {
    const args = [];
    while (!match(ts, 'SymbolT', '->')) args.push(parseArg(ts));
    if (args.length === 0) return err('empty args after \\');
    const body = parseAppAll(ts);
    return args.reduceRight((x, y) =>
      y.tag === 'PVar' ? Abs(y.val, x) : Decon(y.con, y.val, x), body);
  } else if (match(ts, 'SymbolT', '(')) {
    return parseApp(ts);
  } else if (match(ts, 'KeywordT', 'let')) {
    if (!safeMatch(ts, 'VarT')) return err('no name after let');
    const x = ts.pop().val;
    if (!match(ts, 'SymbolT', '=')) return err('no = after name after let');
    const val = parseAppTo(ts, ts => {
      if (match(ts, 'KeywordT', 'in')) return false;
      if (ts.length === 0) return err('no in after let');
      return true;
    });
    const body = parseAppAll(ts);
    return Let(x, val, body);
  } else if (safeMatch(ts, 'ConT')) {
    const con = ts.pop().val;
    const arg = parseAppAll(ts);
    return Con(con, arg);
  } else if (safeMatch(ts, 'VarT')) {
    const x = ts.pop();
    return Var(x.val);
  }
  err(`parseExpr stuck on ${ts[ts.length - 1].val}`);
};

const parse = sc => {
  const ts = tokenize(sc);
  const ex = parseAppAll(ts.reverse());
  if (ts.length > 0) return err(`stuck on ${ts[0].val}`);
  return ex;
};

module.exports = {
  tokenize,
  showTokens,
  parseExpr,
  parse,
};

},{"./exprs":2}],8:[function(require,module,exports){
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

},{"./compiler":1,"./exprs":2,"./inference":3,"./parser":7,"./types":9}],9:[function(require,module,exports){
const { KFun, kType, kRow, pruneKind } = require('./kinds');

const TCon = (name, kind) => ({ tag: 'TCon', name, kind });
const TVar = (name, kind) => ({ tag: 'TVar', name, kind });
const TMeta = (id, kind) => ({ tag: 'TMeta', id, kind, type: null });
const TApp = (left, right) => ({ tag: 'TApp', left, right });
const TRowExtend = label => ({ tag: 'TRowExtend', label });

const makeTRowExtend = (label, type, rest) => TApp(TApp(TRowExtend(label), type), rest);
const isTRowExtend = t =>
  t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left.tag === 'TRowExtend';

let _idTMeta = 0;
const resetTMeta = () => { _idTMeta = 0 };
const freshTMeta = kind => TMeta(_idTMeta++, kind);

const tFun = TCon('->', KFun(kType, KFun(kType, kType)));
const TFun = (left, right) => TApp(TApp(tFun, left), right);
const isTFun = t => t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left === tFun;

const tRowEmpty = TCon('RowEmpty', kRow);
const tRec = TCon('Rec', KFun(kRow, kType));
const tVar = TCon('Var', KFun(kRow, kType));

const showType = t => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TVar') return t.name;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TApp') return `(${showType(t.left)} ${showType(t.right)})`;
  if (t.tag === 'TRowExtend') return `#${t.label}`;
};

const flattenTApp = t => {
  let c = t;
  const r = [];
  while (c.tag === 'TApp') {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};

const flattenTFun = t => {
  let c = t;
  const r = [];
  while (isTFun(c)) {
    r.push(c.left.right);
    c = c.right;
  }
  r.push(c);
  return r;
};

const prettyType = t => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TVar') return t.name;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (isTFun(t))
    return flattenTFun(t)
      .map(x => isTFun(x) ? `(${prettyType(x)})` : prettyType(x))
      .join(' -> ');
  if (t.tag === 'TApp')
    return flattenTApp(t)
      .map(x => x.tag === 'TApp' ? `(${prettyType(x)})` : prettyType(x))
      .join(' ');
  if (t.tag === 'TRowExtend') return `#${t.label}`;
};

const pruneType = t => {
  if (t.tag === 'TCon') {
    t.kind = pruneKind(t.kind);
    return t;
  }
  if (t.tag === 'TVar') {
    t.kind = pruneKind(t.kind);
    return t;
  }
  if (t.tag === 'TMeta') {
    t.kind = pruneKind(t.kind);
    if (!t.type) return t;
    const ty = pruneType(t.type);
    t.type = ty;
    return ty;
  }
  if (t.tag === 'TApp') {
    const a = pruneType(t.left);
    const b = pruneType(t.right);
    return a !== t.left || b !== t.right ? TApp(a, b) : t;
  }
  return t;
};

const tmetas = (t, map = {}) => {
  if (t.tag === 'TMeta') {
    map[t.id] = true;
    return map;
  }
  if (t.tag === 'TApp')
    return tmetas(t.right, tmetas(t.left, map));
  return map;
};

module.exports = {
  TCon,
  TVar,
  TMeta,
  TApp,
  TRowExtend,

  makeTRowExtend,
  isTRowExtend,

  resetTMeta,
  freshTMeta,

  tFun,
  TFun,
  isTFun,

  tRowEmpty,
  tRec,
  tVar,

  showType,

  flattenTApp,
  flattenTFun,
  prettyType,

  pruneType,

  tmetas,
};

},{"./kinds":6}],10:[function(require,module,exports){
const {
  showType,
  pruneType,
  tRowEmpty,
  isTRowExtend,
  freshTMeta,
  makeTRowExtend,
} = require('./types');
const { kType, kRow } = require('./kinds');
const { inferKind } = require('./kindInference');
const { unifyKinds } = require('./kindUnification');

const err = msg => { throw new TypeError(msg) };

const occurs = (v, t) => {
  if (v === t) return err(`occurs check failed: ${showType(v)} in ${showType(t)}`);
  if (t.tag === 'TApp') {
    occurs(v, t.left);
    occurs(v, t.right);
  }
};

const bind = (v, t) => {
  occurs(v, t);
  v.type = t;
};

const rewriteRow = (l, r) => {
  if (r === tRowEmpty) return err(`cannot find label ${l}`);
  if (r.tag === 'TMeta') {
    const tv = freshTMeta(kType);
    const tr = freshTMeta(kRow);
    const row = makeTRowExtend(l, tv, tr);
    bind(r, row);
    return row;
  }
  if (!isTRowExtend(r)) return err(`invalid type in rewriteRow(${l}): ${showType(r)}`);
  if (r.left.left.label === l) return r; 
  const rest = rewriteRow(l, r.right);
  return makeTRowExtend(l, rest.left.right,
    makeTRowExtend(r.left.left.label, r.left.right, rest.right));
};

const unify = (a_, b_) => {
  const a = pruneType(a_);
  const b = pruneType(b_);
  if (a === b) return;
  // console.log(`${showType(a)} ~ ${showType(b)}`);
  unifyKinds(inferKind(a), inferKind(b));
  if (a.tag === 'TMeta') return bind(a, b);
  if (b.tag === 'TMeta') return bind(b, a);
  if (isTRowExtend(a) && isTRowExtend(b)) {
    const br = rewriteRow(a.left.left.label, b);
    unify(a.left.right, br.left.right);
    return unify(a.right, br.right);
  }
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    return unify(a.right, b.right);
  }
  if (a.tag === 'TRowExtend' && b.tag === 'TRowExtend' && a.label === b.label)
    return;
  return err(`failed to unify types: ${showType(a)} ~ ${showType(b)}`);
};

module.exports = {
  unify,
};

},{"./kindInference":4,"./kindUnification":5,"./kinds":6,"./types":9}],11:[function(require,module,exports){
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

},{"./repl":8}]},{},[11]);

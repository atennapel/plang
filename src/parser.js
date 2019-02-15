const {
  Var,
  Abs,
  App,
  Con,
  Decon,
  LitStr,
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
  showType,
} = require('./types');
const {
  DType,
  DValue,
  DDeclare,
  DForeign,
} = require('./defs');

const SYMBOLS = '()\\.=';
const SYMBOLS2 = ['->', '::', ':='];
const SYMBOLS3 = ['::='];
const START = 0;
const NAME = 1;
const NUMBER = 2;
const STRING = 3;
const tokenize = s => {
  let state = START;
  const r = [];
  let t = '';
  let escape = false;
  for (let i = 0, l = s.length; i <= l; i++) {
    const c = s[i] || ' ';
    const next = s[i+1] || '';
    const next2 = s[i+2] || '';
    if (state === START) {
      if (SYMBOLS3.indexOf(c + next + next2) >= 0) r.push(c + next + next2), i += 2;
      else if (SYMBOLS2.indexOf(c + next) >= 0) r.push(c + next), i++;
      else if (SYMBOLS.indexOf(c) >= 0) r.push(c);
      else if (c === '"') state = STRING;
      else if (/[a-z]/i.test(c)) t += c, state = NAME;
      else if (/[0-9]/.test(c)) t += c, state = NUMBER;
      else if (/\s/.test(c)) continue;
      else throw new SyntaxError(`unexpected char ${c}`);
    } else if (state === NAME) {
      if (!/[a-z0-9\.]/i.test(c))
        r.push(t), t = '', i--, state = START;
      else t += c;
    } else if (state === NUMBER) {
      if (!/[0-9]/.test(c))
        r.push(t), t = '', i--, state = START;
      else t += c;
    } else if (state === STRING) {
      if (escape) { t += c; escape = false }
      else if (c === '\\') escape = true;
      else if (c === '"') r.push(c + t), t = '', state = START;
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
  if (isCon(x)) return TCon(x);
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

const isCon = x => {
  if (x[0] === '"') throw new SyntaxError(`invalid name: ${JSON.stringify(x.slice(1))}`);
  const s = x.split('.');
  if (s.length === 0 || s.indexOf('') >= 0) throw new SyntaxError(`invalid name: ${x}`);
  return /[A-Z]/.test(s[s.length - 1][0]);
};

const parseExpr = a => {
  // console.log(a.slice().reverse().join(' '));
  if (a.length === 0) throw new SyntaxError('empty');
  if (match(a, '(')) {
    const es = [];
    while (true) {
      if (a.length === 0) throw new SyntaxError('missing )');
      if (match(a, ')')) break;
      es.push(parseExpr(a));
    }
    if (es.length === 0) throw new SyntaxError('empty');
    if (es.length === 1) {
      const e = es[0];
      if (e.tag === 'LitStr') return e;
      if (e.tag === 'Var' && isCon(e.name))
        throw new SyntaxError(`constructor ${e.name} takes 1 argument, but 0 given.`);
      return e;
    }
    const head = es[0];
    if (head.tag === 'Var' && isCon(head.name)) {
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
      if (isCon(args[i]))
        throw new SyntaxError(`constructor in abs argument: ${args[i]}`);
    const es = [];
    let br = 0;
    while (true) {
      if (a.length === 0) break;
      if (a[a.length - 1] === '(') br++;
      if (a[a.length - 1] === ')') {
        if (br === 0) break;
        br--;
      }
      es.push(a.pop());
    }
    es.unshift('('); es.push(')');
    const body = parseExpr(es.reverse());
    if (isCon(args[0])) {
      if (args.length !== 2)
        throw new SyntaxError(`deconstructor ${args[0]} expects 1 argument but got ${args.length - 1}`);
      return Decon(args[0], args[1], body);
    }
    return args.reduceRight((x, y) => Abs(y, x), body);
  } else if (matchfn(a, x => !/[a-z]/i.test(x[0]) && !/[0-9]/.test(x[0]) && x[0] !== '"'))
    throw new SyntaxError(`unexpected ${a.pop()}`);
  const x = a.pop();
  if (x[0] === '"') return LitStr(x.slice(1));
  return Var(x);
};

const parseName = ts => {
  if (ts.length === 0)
    throw new SyntaxError('name expected but got nothing');
  const x = ts.pop();
  if (!/[a-z]/i.test(x) && !/[0-9]/.test(x))
    throw new SyntaxError(`name expected but got ${x}`);
  return x;
};

const matchDefSymbol = ts => {
  if (match(ts, '=')) return '=';
  if (match(ts, '::')) return '::';
  if (match(ts, '::=')) return '::=';
  return null;
};

const parseDef = ts => {
  const x = parseName(ts);
  // console.log('parseDef', x);
  const sym = matchDefSymbol(ts);
  if (!sym) throw SyntaxError('invalid definition, no definition symbol found');
  const body = [];
  let found;
  while (!(found = matchDefSymbol(ts))) {
    if (ts.length === 0) {
      found = null;
      break;
    }
    body.push(ts.pop());
  }
  if (found) ts.push(found, body.pop());
  if (sym === '=') {
    if (!isCon(x)) {
      body.unshift('('); body.push(')');
      body.reverse();
      return DValue(x, parseExpr(body));
    }
    if (body[0] !== '\\') {
      body.unshift('('); body.push(')');
    }
    body.reverse();
    return parseTypeTop(x, body);
  } else if (sym === '::') {
    if (isCon(x)) throw new SyntaxError(`cannot declare type: ${x} :: ...`);
    body.unshift('('); body.push(')');
    const ty = parseType(body.reverse());
    return DDeclare(x, ty);
  } else if (sym === '::=') {
    if (isCon(x)) throw new SyntaxError(`cannot define foreign constructor ${x}`);
    body.unshift('('); body.push(')');
    const ex = parseExpr(body.reverse());
    if (ex.tag !== 'LitStr') throw new SyntaxError(`Foreign definition ${x} should be a string`);
    return DForeign(x, ex.val);
  } else throw new SyntaxError(`invalid definition symbol: ${sym}`);
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

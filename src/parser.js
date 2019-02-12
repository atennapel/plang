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
  // console.log('parseDef', x);
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

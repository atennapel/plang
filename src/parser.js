const {
  Var,
  Abs,
  App,
  Con,
  Decon,
} = require('./exprs');

const SYMBOLS = '()\\.=';
const START = 0;
const NAME = 1;
const tokenize = s => {
  let state = START;
  const r = [];
  let t = '';
  for (let i = 0, l = s.length; i <= l; i++) {
    const c = s[i] || ' ';
    if (state === START) {
      if (SYMBOLS.indexOf(c) >= 0) r.push(c);
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

const match = (a, x) => {
  if (a.length && a[a.length - 1] === x) {
    a.pop();
    return true;
  }
  return false;
};

const parseExpr = a => {
  if (a.length === 0) throw new SyntaxError('empty');
  if (match(a, '(')) {
    const es = [parseExpr(a)];
    while (a.length && !match(a, ')'))
      es.push(parseExpr(a));
    if (es.length === 0) throw new SyntaxError('empty');
    const head = es[0];
    if (head.tag === 'Var' && /[A-Z]/.test(head.name)) {
      if (es.length !== 2)
        throw new SyntaxError(`constructor ${head.name} takes 1 argument, but ${es.length - 1} given.`);
      return Con(head.name, es[1]);
    }
    return es.reduce(App);
  } else if (match(a, '\\')) {
    const args = [];
    while (!match(a, '.')) args.push(parseName(a));
    if (args.length === 0)
      throw new SyntaxError('abs without parameters');
    for (let i = 1, l = args.length; i < l; i++)
      if (/[A-Z]/.test(args[i][0]))
        throw new SyntaxError(`constructor in abs argument: ${args[i]}`);
    const body = parseApp(a);
    if (/[A-Z]/.test(args[0][0])) {
      if (args.length !== 2)
        throw new SyntaxError(`deconstructor ${args[0]} expects 1 argument but got ${args.length - 1}`);
      return Decon(args[0], args[1], body);
    }
    return args.reduceRight((x, y) => Abs(y, x), body);
  } else if (match(a, '.'))
    throw new SyntaxError('unexpected .');
  else if (match(a, '='))
    throw new SyntaxError('unexpected =');
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

const parseApp = ts => {
  const es = [];
  while (ts.length) es.push(parseExpr(ts));
  if (es.length === 0) throw new SyntaxError('empty');
  const head = es[0];
  if (head.tag === 'Var' && /[A-Z]/.test(head.name)) {
    if (es.length !== 2)
      throw new SyntaxError(`constructor ${head.name} takes 1 argument, but ${es.length - 1} given.`);
    return Con(head.name, es[1]);
  }
  return es.reduce(App);
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
  return [x, parseApp(body.reverse())];
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

module.exports = {
  parseDefs,
};

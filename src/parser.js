const {
  Var,
  Abs,
  App,
  Con,
  Decon,
} = require('./exprs');

const err = msg => { throw new SyntaxError(msg) };

// tokens
const SymbolT = val => ({ tag: 'SymbolT', val });
const VarT = val => ({ tag: 'VarT', val });
const ConT = val => ({ tag: 'ConT', val});

const showTokens = ts =>
  ts.map(x => `${x.tag}#${x.val}`).join(' ');

const SYM1 = ['(', ')', '\\'];
const SYM2 = ['->'];

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
      if (!/[a-z0-9]/i.test(c)) 
        r.push(/[a-z]/.test(t[0]) ? VarT(t) : ConT(t)), t = '', i--, state = START;
      else t += c;
    }
  }
  if (state !== START) return err('invalid tokenize end state');
  return r;
};

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
  if (x.tag === 'VarT') return x.val;
  err(`invalid arg tag: ${x.tag}`);
};

const parseApp = ts => {
  const es = [];
  while (!match(ts, 'SymbolT', ')')) {
    if (ts.length === 0) return err('unclosed (');
    es.push(parseExpr(ts));
  }
  if (es.length === 0) return err('empty app');
  if (es.length === 1) return es[0];
  return es.reduce(App);
};

const parseExpr = ts => {
  if (ts.length === 0) return err('empty expr');
  if (match(ts, 'SymbolT', '\\')) {
    const args = [];
    while (!match(ts, 'SymbolT', '->')) args.push(parseArg(ts));
    if (args.length === 0) return err('empty args after \\');
    ts.push(SymbolT('(')); ts.unshift(SymbolT(')'));
    const body = parseExpr(ts);
    return args.reduceRight((x, y) => Abs(y, x), body);
  } else if (match(ts, 'SymbolT', '(')) {
    return parseApp(ts);
  } else if (safeMatch(ts, 'VarT')) {
    const x = ts.pop();
    return Var(x.val);
  }
  err(`parseExpr stuck on ${ts[ts.length - 1].val}`);
};

const parse = sc => {
  const ts = tokenize(sc);
  ts.unshift(SymbolT('(')); ts.push(SymbolT(')'));
  const ex = parseExpr(ts.reverse());
  if (ts.length > 0) return err(`stuck on ${ts[0].val}`);
  return ex;
};

module.exports = {
  tokenize,
  showTokens,
  parseExpr,
  parse,
};

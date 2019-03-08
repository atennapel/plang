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
    return args.reduceRight((x, y) =>
      y.tag === 'PVar' ? Abs(y.val, x) : Decon(y.con, y.val, x), body);
  } else if (match(ts, 'SymbolT', '(')) {
    return parseApp(ts);
  } else if (safeMatch(ts, 'ConT')) {
    const con = ts.pop().val;
    ts.push(SymbolT('(')); ts.unshift(SymbolT(')'));
    const arg = parseExpr(ts);
    return Con(con, arg);
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

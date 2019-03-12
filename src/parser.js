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

const parseType = ts => {
  return err('unimplemented');
};

const parseTypeTop = sc => {
  const ts = tokenize(sc);
  const ex = parseType(ts.reverse());
  if (ts.length > 0) return err(`type stuck on ${ts[0].val}`);
  return ex;
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

const parseExprTop = sc => {
  const ts = tokenize(sc);
  const ex = parseAppAll(ts.reverse());
  if (ts.length > 0) return err(`stuck on ${ts[0].val}`);
  return ex;
};

module.exports = {
  tokenize,
  showTokens,
  parseExpr: parseExprTop,
  parseType: parseTypeTop,
};

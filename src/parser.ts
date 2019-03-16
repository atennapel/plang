import { Kind, KVar, kfunFrom } from './kinds';
import { Type, TVar, tappFrom, tforall } from './types';
import {
  Term,
  Var,
  abs,
  appFrom,
  Let,
  Ann,
} from './terms';
import { Name } from './names';

const err = (msg: string) => { throw new SyntaxError(msg) };

// tokens
interface Token {
  readonly tag: 'SymbolT' | 'VarT' | 'KeywordT';
  readonly val: string;
}
const SymbolT = (val: string): Token => ({ tag: 'SymbolT', val });
const VarT = (val: string): Token => ({ tag: 'VarT', val });
const KeywordT = (val: string): Token => ({ tag: 'KeywordT', val });

const showTokens = (ts: Token[]) => ts.map(x => `${x.val}`).join(' ');

const SYM1 = ['(', ')', '\\', '=', ':', '.'];
const SYM2 = ['->'];
const KEYWORDS = ['let', 'in', 'forall'];

const START = 0;
const NAME = 1;
const tokenize = (sc: string) => {
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
          VarT(t)
        );
        t = '', i--, state = START;
      } else t += c;
    }
  }
  if (state !== START) return err('invalid tokenize end state');
  return r;
};

// parser
const match = (ts: Token[], tag: string, val: string | null = null) => {
  if (ts.length === 0) return false;
  const top = ts[ts.length - 1];
  if (top.tag === tag && (!val || top.val === val)) {
    ts.pop();
    return true;
  }
  return false;
};
const safeMatch = (ts: Token[], tag: string, val: string | null = null) => {
  if (ts.length === 0) return false;
  const top = ts[ts.length - 1];
  if (top.tag === tag && (!val || top.val === val))
    return true;
  return false;
};

// kinds
const parseKindR = (ts: Token[]): Kind => {
  if (ts.length === 0) return err('empty kind');
  if (safeMatch(ts, 'VarT')) {
    const x = ts.pop() as Token;
    return KVar(Name(x.val));
  }
  return err(`parseKindR stuck on ${ts[ts.length - 1].val}`);
};

export const parseKind = (sc: string): Kind => {
  const ts = tokenize(sc);
  const ex = parseKindR(ts.reverse());
  if (ts.length > 0) return err(`kind stuck on ${ts[0].val}`);
  return ex;
};

// types
const parseTAppTo = (ts: Token[], fn: (ts: Token[]) => boolean): Type => {
  const es = [];
  while (fn(ts)) es.push(parseTypeR(ts));
  if (es.length === 0) return err('empty app');
  if (es.length === 1) return es[0];
  return tappFrom(es);
};
const parseTApp = (ts: Token[]) => parseTAppTo(ts, ts => {
  if (match(ts, 'SymbolT', ')')) return false;
  if (ts.length === 0) return err('tapp end');
  return true;
});
const parseTAppAll = (ts: Token[]): Type => {
  const es = [];
  while (!(ts.length === 0 ||
    safeMatch(ts, 'SymbolT', ')'))) es.push(parseTypeR(ts));
  if (es.length === 0) return err('empty app');
  return tappFrom(es);
};

const parseTypeR = (ts: Token[]): Type => {
  if (ts.length === 0) return err('empty type');
  if (match(ts, 'KeywordT', 'forall')) {
    const args = [];
    while (!match(ts, 'SymbolT', '.')) args.push(parseArg(ts));
    if (args.length === 0) return err('empty args after forall');
    const body = parseTAppAll(ts);
    return tforall(args.map(Name), body);
  } else if (match(ts, 'SymbolT', '(')) {
    return parseTApp(ts);
  } else if (safeMatch(ts, 'VarT')) {
    const x = ts.pop() as Token;
    return TVar(Name(x.val));
  }
  return err(`parseTypeR stuck on ${ts[ts.length - 1].val}`);
};

export const parseType = (sc: string): Type => {
  const ts = tokenize(sc);
  const ex = parseTypeR(ts.reverse());
  if (ts.length > 0) return err(`type stuck on ${ts[0].val}`);
  return ex;
};

// terms
const parseArg = (ts: Token[]): string => {
  if (ts.length === 0) return err(`empty in argument`);
  const x = ts.pop() as Token;
  if (x.tag === 'VarT') return x.val;
  return err(`invalid arg: ${x.val}`);
};

const parseAppTo = (ts: Token[], fn: (ts: Token[]) => boolean): Term => {
  const es = [];
  while (fn(ts)) es.push(parseExpr(ts));
  if (es.length === 0) return err('empty app');
  if (es.length === 1) return es[0];
  return appFrom(es);
};
const parseApp = (ts: Token[]) => parseAppTo(ts, ts => {
  if (match(ts, 'SymbolT', ')')) return false;
  if (ts.length === 0) return err('app end');
  return true;
});
const parseAppAll = (ts: Token[]): Term => {
  const es = [];
  while (!(ts.length === 0 ||
    safeMatch(ts, 'SymbolT', ')') ||
    safeMatch(ts, 'SymbolT', ':') ||
    safeMatch(ts, 'KeywordT', 'in'))) es.push(parseExpr(ts));
  if (es.length === 0) return err('empty app');
  const ex = appFrom(es);
  if (safeMatch(ts, 'SymbolT', ':')) {
    ts.pop();
    const ty = parseTAppAll(ts);
    return Ann(ex, ty);
  }
  return ex;
};

const parseExpr = (ts: Token[]): Term => {
  // console.log(showTokens(ts.slice(0).reverse()));
  if (ts.length === 0) return err('empty expr');
  if (match(ts, 'SymbolT', '\\')) {
    const args = [];
    while (!match(ts, 'SymbolT', '->')) args.push(parseArg(ts));
    if (args.length === 0) return err('empty args after \\');
    const body = parseAppAll(ts);
    return abs(args.map(Name), body);
  } else if (match(ts, 'SymbolT', '(')) {
    return parseApp(ts);
  } else if (match(ts, 'KeywordT', 'let')) {
    if (!safeMatch(ts, 'VarT')) return err('no name after let');
    const x = (ts.pop() as Token).val;
    if (!match(ts, 'SymbolT', '=')) return err('no = after name after let');
    const val = parseAppTo(ts, ts => {
      if (match(ts, 'KeywordT', 'in')) return false;
      if (ts.length === 0) return err('no in after let');
      return true;
    });
    const body = parseAppAll(ts);
    return Let(Name(x), val, body)
  } else if (safeMatch(ts, 'VarT')) {
    const x = ts.pop() as Token;
    return Var(Name(x.val));
  } else if (match(ts, 'KeywordT', 'forall')) {
    return Var(Name('forall'));
  }
  return err(`parseExpr stuck on ${ts[ts.length - 1].val}`);
};

export const parseTerm = (sc: string): Term => {
  const ts = tokenize(sc);
  const ex = parseAppAll(ts.reverse());
  if (ts.length > 0) return err(`stuck on ${ts[0].val}`);
  return ex;
};


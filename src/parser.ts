import { Term, Var, appFrom, abs, Ann } from './terms';
import { Name } from './names';
import { TVar, Type, tappFrom, tforall, tfunFrom } from './types';

const err = (msg: string) => { throw new SyntaxError(msg) };

// tokens
type Token
  = VarT
  | SymbolT
  | ParenT;

interface VarT { readonly tag: 'VarT'; readonly val: string }
const VarT = (val: string): VarT => ({ tag: 'VarT', val });
const matchVarT = (val: string, t: Token): boolean => t.tag === 'VarT' && t.val === val;
interface SymbolT { readonly tag: 'SymbolT'; readonly val: string }
const SymbolT = (val: string): SymbolT => ({ tag: 'SymbolT', val });
const matchSymbolT = (val: string, t: Token): boolean => t.tag === 'SymbolT' && t.val === val;
interface ParenT { readonly tag: 'ParenT'; readonly val: Token[] }
const ParenT = (val: Token[]): ParenT => ({ tag: 'ParenT', val });

const showToken = (t: Token): string => {
  switch (t.tag) {
    case 'SymbolT':
    case 'VarT': return t.val;
    case 'ParenT': return `(${t.val.map(showToken).join(' ')})`;
  }
};
const showTokens = (ts: Token[]): string => ts.map(showToken).join(' ');

type Bracket = '(' | ')';
const matchingBracket = (c: Bracket): Bracket => {
  if(c === '(') return ')';
  if(c === ')') return '(';
  return err(`invalid bracket: ${c}`);
}

const SYM1 = ['\\', ':', '.'];
const SYM2 = ['->'];

const START = 0;
const NAME = 1;
const tokenize = (sc: string): Token[] => {
  let state = START;
  let r: Token[] = [];
  let t = '';
  let p: Token[][] = [], b: Bracket[] = [];
  for (let i = 0, l = sc.length; i <= l; i++) {
    const c = sc[i] || ' ';
    const next = sc[i + 1] || '';
    // console.log(`${i};${c};${next};${state}`, r);
    if (state === START) {
      if (SYM2.indexOf(c + next) >= 0) r.push(SymbolT(c + next)), i++;
      else if (SYM1.indexOf(c) >= 0) r.push(SymbolT(c));
      else if (/[a-z]/i.test(c)) t += c, state = NAME;
      else if(c === '(') b.push(c), p.push(r), r = [];
      else if(c === ')') {
        if(b.length === 0) return err(`unmatched bracket: ${c}`);
        const br = b.pop() as Bracket;
        if(matchingBracket(br) !== c) return err(`unmatched bracket: ${br} and ${c}`);
        const a: Token[] = p.pop() as Token[];
        a.push(ParenT(r));
        r = a;
      }
      else if (/\s/.test(c)) continue;
      else return err(`invalid char ${c} in tokenize`);
    } else if (state === NAME) {
      if (!/[a-z0-9]/i.test(c)) {
        r.push(VarT(t));
        t = '', i--, state = START;
      } else t += c;
    }
  }
  if (b.length > 0) return err(`unclosed brackets: ${b.join(' ')}`);
  if (state !== START) return err('invalid tokenize end state');
  return r;
};

const indexOf = (a: Token[], fn: (t: Token) => boolean): number => {
  for (let i = 0, l = a.length; i < l; i++)
    if (fn(a[i])) return i;
  return -1;
};
const contains = (a: Token[], fn: (t: Token) => boolean): boolean =>
  indexOf(a, fn) >= 0;
const splitTokens = (a: Token[], fn: (t: Token) => boolean): Token[][] => {
  const r: Token[][] = [];
  let t: Token[] = [];
  for (let i = 0, l = a.length; i < l; i++) {
    const c = a[i];
    if (fn(c)) {
      r.push(t);
      t = [];
    } else t.push(c);
  }
  r.push(t);
  return r;
};

// types
const parseTokenType = (ts: Token): Type => {
  // console.log(`parseTokenType ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': {
      if (matchVarT('forall', ts)) return err(`stuck on forall`);
      return TVar(Name(ts.val));
    }
    case 'SymbolT': return err(`stuck on ${ts.val}`);
    case 'ParenT': return parseParensType(ts.val);
  }
};

const parseParensType = (ts: Token[]): Type => {
  // console.log(`parseParensType ${showTokens(ts)}`);
  if (ts.length === 0) return err('empty type');
  if (ts.length === 1) return parseTokenType(ts[0]);
  if (matchVarT('forall', ts[0])) {
    const args: string[] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no . after forall`);
      if (matchSymbolT('.', c)) break;
      if (c.tag !== 'VarT') return err(`invalid arg to forall: ${c.val}`);
      args.push(c.val);
    }
    if (args.length === 0) return err(`forall without args`);
    const body = parseParensType(ts.slice(i));
    return tforall(args.map(Name), body);
  }
  let args: Token[] = [];
  const fs: Token[][] = [];
  for (let i = 0; i < ts.length; i++) {
    const c = ts[i];
    if (matchVarT('forall', c)) {
      const rest = parseParensType(ts.slice(i));
      const app = tappFrom(args.map(parseTokenType).concat([rest]));
      return tfunFrom(fs.map(ts => tappFrom(ts.map(parseTokenType))).concat([app]));
    }
    if (matchSymbolT('->', c)) {
      fs.push(args);
      args = [];
      continue;
    }
    args.push(c);
  }
  fs.push(args);
  return tfunFrom(fs.map(ts => tappFrom(ts.map(parseTokenType))));
};

// terms
const parseToken = (ts: Token): Term => {
  // console.log(`parseToken ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': return Var(Name(ts.val));
    case 'SymbolT': return err(`stuck on ${ts.val}`);
    case 'ParenT': return parseParens(ts.val);
  }
};

const parseParens = (ts: Token[]): Term => {
  // console.log(`parseParens ${showTokens(ts)}`);
  if (ts.length === 0) return err('empty');
  if (ts.length === 1) return parseToken(ts[0]);
  if (contains(ts, t => matchSymbolT(':', t))) {
    const parts = splitTokens(ts, t => matchSymbolT(':', t));
    if (parts.length !== 2) return err(`invalid use (${parts.length}) of :`);
    const left = parseParens(parts[0]);
    const right = parseParensType(parts[1]);
    return Ann(left, right);
  }
  if (matchSymbolT('\\', ts[0])) {
    const args: string[] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no -> after \\`);
      if (matchSymbolT('->', c)) break;
      if (c.tag !== 'VarT') return err(`invalid arg: ${c.val}`);
      args.push(c.val);
    }
    if (args.length === 0) return err(`\\ without args`);
    const body = parseParens(ts.slice(i));
    return abs(args.map(Name), body);
  }
  const args: Term[] = [];
  for (let i = 0; i < ts.length; i++) {
    const c = ts[i];
    if (matchSymbolT('\\', c)) {
      args.push(parseParens(ts.slice(i)));
      return appFrom(args);
    }
    args.push(parseToken(c));
  }
  return appFrom(args);
};

export const parse = (sc: string) => {
  const ts = tokenize(sc);
  // console.log(showTokens(ts));
  const ex = parseParens(ts);
  return ex;
};

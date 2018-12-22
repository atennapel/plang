import { Expr, Select, Var, Case, Inject, Extend, appFrom } from "./exprs";

const err = (msg: string) => { throw new SyntaxError(msg) };

type Token = TkNum | TkName | TkParens;

interface TkNum { tag: 'TkNum'; val: string }
interface TkName { tag: 'TkName'; val: string }
interface TkParens { tag: 'TkParens'; val: Token[] }

type Bracket = '(' | ')';
const matchingBracket = (c: Bracket): Bracket => {
  if(c === '(') return ')';
  if(c === ')') return '(';
  return err(`invalid bracket: ${c}`);
}

const START = 0;
const NUM = 1;
const NAME = 2;

const tokenize = (s: string): Token[] => {
  let state = START;
  let t = '';
  let r: Token[] = [], p: Token[][] = [], b: Bracket[] = [];
  for (let i = 0; i <= s.length; i++) {
    const c = s[i] || ' ';
    if (state === START) {
      if (/[a-z\.\+\?\@]/i.test(c)) t += c, state = NAME;
      else if (/[0-9]/.test(c)) t += c, state = NUM;
      else if(c === '(') b.push(c), p.push(r), r = [];
      else if(c === ')') {
        if(b.length === 0) return err(`unmatched bracket: ${c}`);
        const br = b.pop() as Bracket;
        if(matchingBracket(br) !== c) return err(`unmatched bracket: ${br} and ${c}`);
        const a: Token[] = p.pop() as Token[];
        a.push({ tag: 'TkParens', val: r });
        r = a;
      } else if(/\s+/.test(c)) continue;
      else return err(`invalid char: ${c}`);
    } else if (state === NUM) {
      if(!/[0-9\.]/.test(c)) r.push({ tag: 'TkNum', val: t }), t = '', i--, state = START;
      else t += c;
    } else if (state === NAME) {
      if(!/[a-z0-9]/i.test(c)) r.push({ tag: 'TkName', val: t }), t = '', i--, state = START;
      else t += c;
    }
  }
  if(state !== START) return err(`invalid parsing end state: ${state}`);
  return r;
};

const parseToken = (a: Token): Expr => {
  switch (a.tag) {
    case 'TkNum': {
      const n = +a.val;
      if (isNaN(n)) return err(`invalid number: ${a.val}`);
      return err(`numbers not implemented`);
    }
    case 'TkName': {
      if (a.val[0] === '.') return a.val.length === 1 ? err('nothing after .') : Select(a.val.slice(1));
      if (a.val[0] === '+') return a.val.length === 1 ? err('nothing after +') : Extend(a.val.slice(1));
      if (a.val[0] === '@') return a.val.length === 1 ? err('nothing after @') : Inject(a.val.slice(1));
      if (a.val[0] === '?') return a.val.length === 1 ? err('nothing after ?') : Case(a.val.slice(1));
      return Var(a.val);
    }
    case 'TkParens':
      return parseParens(a.val);
  }
};

const parseParens = (a: Token[]): Expr => {
  if (a.length === 0) return Var('Unit');
  if (a.length === 1) return parseToken(a[0]);
  return appFrom(a.map(parseToken));
};

export const parse = (s: string): Expr => parseParens(tokenize(s));

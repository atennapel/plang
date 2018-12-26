import { Expr, Select, Var, Case, Inject, Extend, appFrom, Lit, Abs, abs, Anno, App } from "./exprs";
import { Type, TVar, initialTypes, TRowEmpty, tfuns, tfun, tfunFrom, tappFrom } from "./types";
import { freshKMeta } from "./kinds";

const err = (msg: string) => { throw new SyntaxError(msg) };

type Token = TkNum | TkName | TkStr | TkParens;

interface TkNum { tag: 'TkNum'; val: string }
interface TkName { tag: 'TkName'; val: string }
interface TkStr { tag: 'TkStr'; val: string }
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
const STR = 3;

const tokenize = (s: string): Token[] => {
  let state = START;
  let t = '';
  let r: Token[] = [], p: Token[][] = [], b: Bracket[] = [], esc = false;
  for (let i = 0; i <= s.length; i++) {
    const c = s[i] || ' ';
    const next = s[i+1] || ' ';
    // console.log(i, c, state, t, esc);
    if (state === START) {
      if (c === '-' && next === '>') r.push({ tag: 'TkName', val: '->' }), i++;
      else if (c === ':') r.push({ tag: 'TkName', val: c });
      else if (c === '$') r.push({ tag: 'TkName', val: c });
      else if (c === '|') r.push({ tag: 'TkName', val: c });
      else if (/[a-z\.\+\?\@]/i.test(c)) t += c, state = NAME;
      else if (/[0-9]/.test(c)) t += c, state = NUM;
      else if(c === '"') state = STR;
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
    } else if (state === STR) {
      if (esc) { esc = false; t += c }
      else if (c === '\\') esc = true;
      else if (c === '"') r.push({ tag: 'TkStr', val: t }), t = '', state = START;
      else t += c;
    }
  }
  if (b.length > 0) return err(`unclosed brackets: ${b.join(' ')}`);
  if (state === STR) return err('unclosed string');
  if (state !== START) return err(`invalid parsing end state: ${state}`);
  return r;
};

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
  if (t.length > 0) r.push(t);
  return r;
};

const parseToken = (a: Token, tvMap: { [key: string]: number }, index: { val: number }): Expr => {
  switch (a.tag) {
    case 'TkNum': {
      const n = +a.val;
      if (isNaN(n)) return err(`invalid number: ${a.val}`);
      return Lit(n);
    }
    case 'TkName': {
      if (a.val[0] === '.') return a.val.length === 1 ? err('nothing after .') : Select(a.val.slice(1));
      if (a.val[0] === '+') return a.val.length === 1 ? err('nothing after +') : Extend(a.val.slice(1));
      if (a.val[0] === '@') return a.val.length === 1 ? err('nothing after @') : Inject(a.val.slice(1));
      if (a.val[0] === '?') return a.val.length === 1 ? err('nothing after ?') : Case(a.val.slice(1));
      if (/[a-z][a-z0-9]*/i.test(a.val)) return Var(a.val);
      return err(`invalid name: ${a.val}`);
    }
    case 'TkStr': return Lit(a.val);
    case 'TkParens':
      return parseParens(a.val, tvMap, index);
  }
};
const parseParens = (a: Token[], tvMap: { [key: string]: number }, index: { val: number }): Expr => {
  if (a.length === 0) return Var('empty');
  if (a.length === 1) return parseToken(a[0], tvMap, index);

  const spl = splitTokens(a, t => t.tag === 'TkName' && t.val === ':');
  if (spl.length === 0 || spl.length > 2) return err('invalid use of :');
  if (spl.length === 2) {
    const left = parseParens(spl[0], tvMap, index);
    const right = parseParensType(spl[1], tvMap, index);
    return Anno(left, right);
  }
  if (a.length >= 2 && a[0].tag === 'TkName' && a[0].val === 'fn') {
    const sa = a[1].val;
    const args = Array.isArray(sa) ? sa.map(t => t.tag === 'TkName' ? t.val : err('invalid arg in fn')) : [sa];
    return abs(args, parseParens(a.slice(2), tvMap, index));
  }
  const rapps = splitTokens(a, t => t.tag === 'TkName' && t.val === '|');
  if (rapps.length > 1) return rapps.map(x => parseParens(x, tvMap, index)).reduce((x, y) => App(y, x));
  const apps = splitTokens(a, t => t.tag === 'TkName' && t.val === '$');
  if (apps.length > 1) return appFrom(apps.map(x => parseParens(x, tvMap, index)));
  return appFrom(apps[0].map(x => parseToken(x, tvMap, index)));
};

const parseTokenType = (a: Token, tvMap: { [key: string]: number }, index: { val: number }): Type => {
  switch (a.tag) {
    case 'TkNum': return err('number cannot be used as a type');
    case 'TkName': {
      const n = a.val;
      if (/[a-z]/.test(n[0])) return TVar(typeof tvMap[n] !== 'undefined' ? tvMap[n] : tvMap[n] = index.val++, freshKMeta());
      if (/[A-Z]/.test(n[0]) && initialTypes[n]) return initialTypes[n];
      return err(`invalid type: ${n}`);
    }
    case 'TkStr': return err('string cannot be used as a type');
    case 'TkParens':
      return parseParensType(a.val, tvMap, index);
  }
};
const parseParensType = (a: Token[], tvMap: { [key: string]: number }, index: { val: number }): Type => {
  if (a.length === 0) return TRowEmpty;
  if (a.length === 1) return parseTokenType(a[0], tvMap, index);
  const spl = splitTokens(a, t => t.tag === 'TkName' && t.val === '->');
  if (spl.length === 1) return tappFrom(spl[0].map(x => parseTokenType(x, tvMap, index)));
  if (spl.length < 2) return err('invalid use of ->');
  return tfunFrom(spl.map(x => parseParensType(x, tvMap, index)));
};

export const parse = (s: string): Expr => parseParens(tokenize(s), {}, { val: 0 });

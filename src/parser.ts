import { Term, Var, appFrom, abs, Ann, Let } from './terms';
import { Name, NameT } from './names';
import { TVar, Type, tappFrom, tfunFrom, tforallK } from './types';
import { Kind, KVar, kfunFrom, kType } from './kinds';
import { Def, DLet, DType, DDeclType, DDeclare, DForeign } from './definitions';

const err = (msg: string) => { throw new SyntaxError(msg) };

// tokens
type Token
  = VarT
  | SymbolT
  | StringT
  | ParenT;

interface VarT { readonly tag: 'VarT'; readonly val: string }
const VarT = (val: string): VarT => ({ tag: 'VarT', val });
const matchVarT = (val: string, t: Token): boolean => t.tag === 'VarT' && t.val === val;
interface SymbolT { readonly tag: 'SymbolT'; readonly val: string }
const SymbolT = (val: string): SymbolT => ({ tag: 'SymbolT', val });
const matchSymbolT = (val: string, t: Token): boolean => t.tag === 'SymbolT' && t.val === val;
interface StringT { readonly tag: 'StringT'; readonly val: string }
const StringT = (val: string): StringT => ({ tag: 'StringT', val });
interface ParenT { readonly tag: 'ParenT'; readonly val: Token[] }
const ParenT = (val: Token[]): ParenT => ({ tag: 'ParenT', val });

const showToken = (t: Token): string => {
  switch (t.tag) {
    case 'SymbolT':
    case 'VarT': return t.val;
    case 'StringT': return JSON.stringify(t.val);
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

const SYM1 = ['\\', ':', '.', '='];
const SYM2 = ['->'];

const KEYWORDS = ['let', 'in', 'type'];
const KEYWORDS_TYPE = ['forall', 'type', 'let'];
const KEYWORDS_DEF = ['let', 'type', 'decltype', 'declare', 'foreign'];

const START = 0;
const NAME = 1;
const STRING = 2;
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
      else if (c === '"') state = STRING;
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
    } else if (state === STRING) {
      if (c === '"') {
        r.push(StringT(t));
        t = '', state = START;
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

// kinds
const parseTokenKind = (ts: Token): Kind => {
  // console.log(`parseTokenKind ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': return KVar(Name(ts.val));
    case 'SymbolT': return err(`stuck on ${ts.val}`);
    case 'ParenT': return parseParensKind(ts.val);
    case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
  }
};

const parseParensKind = (ts: Token[]): Kind => {
  // console.log(`parseParensKind ${showTokens(ts)}`);
  if (ts.length === 0) return err('empty kind');
  if (ts.length === 1) return parseTokenKind(ts[0]);
  let args: Token[] = [];
  const fs: Token[][] = [];
  for (let i = 0; i < ts.length; i++) {
    const c = ts[i];
    if (matchSymbolT('->', c)) {
      fs.push(args);
      args = [];
      continue;
    }
    args.push(c);
  }
  fs.push(args);
  return kfunFrom(fs.map(ts => {
    if (ts.length === 0) return err(`empty kind`);
    if (ts.length > 1) return err(`kind applications unimplemented`);
    return parseTokenKind(ts[0]);
  }));
};

// types
const parseTokenType = (ts: Token): Type => {
  // console.log(`parseTokenType ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': {
      if (KEYWORDS_TYPE.indexOf(ts.val) >= 0) return err(`stuck on ${ts.val}`);
      return TVar(Name(ts.val));
    }
    case 'SymbolT': return err(`stuck on ${ts.val}`);
    case 'ParenT': return parseParensType(ts.val);
    case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
  }
};

const parseParensType = (ts: Token[]): Type => {
  // console.log(`parseParensType ${showTokens(ts)}`);
  if (ts.length === 0) return err('empty type');
  if (ts.length === 1) return parseTokenType(ts[0]);
  if (matchVarT('forall', ts[0])) {
    const args: [NameT, Kind | null][] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no . after forall`);
      if (matchSymbolT('.', c)) break;
      if (c.tag === 'ParenT') {
        const parts = splitTokens(c.val, t => matchSymbolT(':', t));
        if (parts.length !== 2) return err(`invalid use of : in forall argument`);
        const as = parts[0].map(t => {
          if (t.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(t.val) >= 0)
            return err(`not a valid arg in forall: ${t.val}`);
          return Name(t.val);
        });
        const ki = parseParensKind(parts[1]);
        for (let j = 0; j < as.length; j++) args.push([as[j], ki]);
        continue;
      }
      if (c.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(c.val) >= 0)
        return err(`invalid arg to forall: ${c.val}`);
      args.push([Name(c.val), null]);
    }
    if (args.length === 0) return err(`forall without args`);
    const body = parseParensType(ts.slice(i));
    return tforallK(args, body);
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
    case 'VarT': {
      if (KEYWORDS.indexOf(ts.val) >= 0) return err(`stuck on ${ts.val}`);
      return Var(Name(ts.val));
    }
    case 'SymbolT': return err(`stuck on ${ts.val}`);
    case 'ParenT': return parseParens(ts.val);
    case 'StringT': return err(`stuck on ${JSON.stringify(ts.val)}`);
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
      if (c.tag !== 'VarT' || KEYWORDS.indexOf(c.tag) >= 0)
        return err(`invalid arg: ${c.val}`);
      args.push(c.val);
    }
    if (args.length === 0) return err(`\\ without args`);
    const body = parseParens(ts.slice(i));
    return abs(args.map(Name), body);
  }
  if (matchVarT('let', ts[0])) {
    const args: NameT[] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no = after let`);
      if (matchSymbolT('=', c)) break;
      if (c.tag !== 'VarT' || KEYWORDS.indexOf(c.tag) >= 0)
        return err(`invalid arg: ${c.val}`);
      args.push(Name(c.val));
    }
    if (args.length === 0) return err(`let without name`);
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no in after = in let`);
      if (matchVarT('in', c)) break;
      bodyts.push(c);
    }
    const body = parseParens(bodyts);
    const rest = parseParens(ts.slice(i));
    return Let(args[0], args.length > 1 ? abs(args.slice(1), body) : body , rest);
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

// definitions
const parseParensDefs = (ts: Token[]): Def[] => {
  if (ts.length === 0) return [];
  if (matchVarT('type', ts[0])) {
    if (ts[1].tag !== 'VarT') return err(`invalid type name: ${ts[1].val}`);
    const tname = ts[1].val as string;
    const args: [NameT, Kind | null][] = [];
    let i = 2;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no = after type def`);
      if (matchSymbolT('=', c)) break;
      if (c.tag === 'ParenT') {
        const parts = splitTokens(c.val, t => matchSymbolT(':', t));
        if (parts.length !== 2) return err(`invalid use of : in type argument`);
        const as = parts[0].map(t => {
          if (t.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(t.val) >= 0)
            return err(`not a valid arg in type: ${t.val}`);
          return Name(t.val);
        });
        const ki = parseParensKind(parts[1]);
        for (let j = 0; j < as.length; j++) args.push([as[j], ki]);
        continue;
      }
      if (c.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(c.tag) >= 0)
        return err(`invalid arg: ${c.val}`);
      args.push([Name(c.val), null]);
    }
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0)) break;
      bodyts.push(c);
    }
    const body = parseParensType(bodyts);
    const rest = parseParensDefs(ts.slice(i - 1));
    return [DType(Name(tname), args, body) as Def].concat(rest);
  }
  if (matchVarT('let', ts[0])) {
    const args: NameT[] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no = after let def`);
      if (matchSymbolT('=', c)) break;
      if (c.tag !== 'VarT' || KEYWORDS.indexOf(c.tag) >= 0)
        return err(`invalid arg: ${c.val}`);
      args.push(Name(c.val));
    }
    if (args.length === 0) return err(`let def without name`);
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0)) break;
      bodyts.push(c);
    }
    const body = parseParens(bodyts);
    const rest = parseParensDefs(ts.slice(i - 1));
    return [DLet(args[0], args.slice(1), body) as Def].concat(rest);
  }
  if (matchVarT('decltype', ts[0])) {
    if (ts[1].tag !== 'VarT') return err(`invalid type name: ${ts[1].val}`);
    const name = ts[1].val as string;
    if (ts[2].tag !== 'SymbolT' || ts[2].val !== ':')
      return err(`: expected after declare name but got ${ts[2].val}`);
    let i = 3;
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0)) break;
      bodyts.push(c);
    }
    const body = parseParensKind(bodyts);
    const rest = parseParensDefs(ts.slice(i - 1));
    return [DDeclType(Name(name), body) as Def].concat(rest);
  }
  if (matchVarT('declare', ts[0])) {
    if (ts[1].tag !== 'VarT') return err(`invalid def name: ${ts[1].val}`);
    const name = ts[1].val as string;
    if (ts[2].tag !== 'SymbolT' || ts[2].val !== ':')
      return err(`: expected after declare name but got ${ts[2].val}`);
    let i = 3;
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0)) break;
      bodyts.push(c);
    }
    const body = parseParensType(bodyts);
    const rest = parseParensDefs(ts.slice(i - 1));
    return [DDeclare(Name(name), body) as Def].concat(rest);
  }
  if (matchVarT('foreign', ts[0])) {
    if (ts[1].tag !== 'VarT') return err(`invalid foreign name: ${ts[1].val}`);
    const name = ts[1].val as string;
    if (ts[2].tag !== 'StringT')
      return err(`string literal expected for foreign ${name} but got ${ts[2].val}`);
    const body = ts[2].val as string;
    const rest = parseParensDefs(ts.slice(3));
    return [DForeign(Name(name), body) as Def].concat(rest);
  }
  return err(`def stuck on ${ts[0].val}`);
};

// parsing
export const parseKind = (sc: string): Kind => {
  const ts = tokenize(sc);
  const ex = parseParensKind(ts);
  return ex;
};
export const parseType = (sc: string): Type => {
  const ts = tokenize(sc);
  const ex = parseParensType(ts);
  return ex;
};
export const parse = (sc: string): Term => {
  const ts = tokenize(sc);
  const ex = parseParens(ts);
  return ex;
};
export const parseDefs = (sc: string): Def[] => {
  const ts = tokenize(sc);
  const ex = parseParensDefs(ts);
  return ex;
};

import { log } from './config';
import { Kind, KCon, kfunFrom } from './kinds';
import { Type, TVar, TCon, tFun, tforall, TApp, tfunFrom, tappFrom } from './types';
import { Name } from './util';
import { Term, Var, abs, PVar, appFrom, App, Ann, Pat, PWildcard, Let, PAnn, PCon, If, LitNat, LitChar, LitStr, Hole, LitInt, LitRat } from './terms';
import { Def, DLet, DType } from './definitions';
import { load, loadPromise } from './import';

const err = (msg: string) => { throw new SyntaxError(msg) };

// tokens
type Token
  = VarT
  | SymbolT
  | StringT
  | CharT
  | NumberT
  | ParenT
  | HoleT;

interface VarT { readonly tag: 'VarT'; readonly val: string }
const VarT = (val: string): VarT => ({ tag: 'VarT', val });
const matchVarT = (val: string, t: Token): boolean => t.tag === 'VarT' && t.val === val;
interface SymbolT { readonly tag: 'SymbolT'; readonly val: string }
const SymbolT = (val: string): SymbolT => ({ tag: 'SymbolT', val });
const matchSymbolT = (val: string, t: Token): boolean => t.tag === 'SymbolT' && t.val === val;
interface StringT { readonly tag: 'StringT'; readonly val: string }
const StringT = (val: string): StringT => ({ tag: 'StringT', val });
interface CharT { readonly tag: 'CharT'; readonly val: string }
const CharT = (val: string): CharT => ({ tag: 'CharT', val });
interface NumberT { readonly tag: 'NumberT'; readonly val: string }
const NumberT = (val: string): NumberT => ({ tag: 'NumberT', val });
interface ParenT { readonly tag: 'ParenT'; readonly val: Token[] }
const ParenT = (val: Token[]): ParenT => ({ tag: 'ParenT', val });
interface HoleT { readonly tag: 'HoleT'; readonly val: string }
const HoleT = (val: string): HoleT => ({ tag: 'HoleT', val });

const showToken = (t: Token): string => {
  switch (t.tag) {
    case 'SymbolT':
    case 'VarT': return t.val;
    case 'HoleT': return `_${t.val}`;
    case 'StringT': return JSON.stringify(t.val);
    case 'CharT': return `'JSON.stringify(t.val).slice(1, -1)'`;
    case 'ParenT': return `(${t.val.map(showToken).join(' ')})`;
    case 'NumberT': return t.val;
  }
};
const showTokens = (ts: Token[]): string => ts.map(showToken).join(' ');

type Bracket = '(' | ')';
const matchingBracket = (c: Bracket): Bracket => {
  if(c === '(') return ')';
  if(c === ')') return '(';
  return err(`invalid bracket: ${c}`);
}

const SYM1 = ['\\', ':', '.', '=', '?'];
const SYM2 = ['->', '<|', '|>', '<<', '>>'];


const KEYWORDS_DEF = ['let', 'type', 'import'];
const KEYWORDS = ['let', 'in', 'if', 'then', 'else'].concat(KEYWORDS_DEF);
const KEYWORDS_TYPE = ['forall'].concat(KEYWORDS_DEF);

const ESCAPES: { [key: string]: string } = {
  'n': '\n',
  't': '\t',
  'r': '\r',
};

const START = 0;
const NAME = 1;
const STRING = 2;
const COMMENT = 3;
const NUMBER = 4;
const CHAR = 5;
const tokenize = (sc: string): Token[] => {
  let state = START;
  let r: Token[] = [];
  let t = '';
  let esc = false;
  let p: Token[][] = [], b: Bracket[] = [];
  for (let i = 0, l = sc.length; i <= l; i++) {
    const c = sc[i] || ' ';
    const next = sc[i + 1] || '';
    log(() => `${i};${c};${next};${state};${showTokens(r)}`);
    if (state === START) {
      if (SYM2.indexOf(c + next) >= 0) r.push(SymbolT(c + next)), i++;
      else if (SYM1.indexOf(c) >= 0) r.push(SymbolT(c));
      else if (c === ';') state = COMMENT;
      else if (c === '"') state = STRING;
      else if (c === "'") state = CHAR;
      else if (/[\_a-z]/i.test(c)) t += c, state = NAME;
      else if (/[0-9]/i.test(c)) t += c, state = NUMBER;
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
      if (!/[a-z0-9\_]/i.test(c)) {
        if (t === '_') r.push(SymbolT(t));
        else if (t[0] === '_') {
          if (/[0-9]/.test(t[1]))
            r.push(NumberT(t));
          else r.push(HoleT(t.slice(1)));
        } else r.push(VarT(t));
        t = '', i--, state = START;
      } else t += c;
    } else if (state === NUMBER) {
      if (!/[0-9a-z\_]/i.test(c)) {
        r.push(NumberT(t.toLowerCase()));
        t = '', i--, state = START;
      } else t += c;
    } else if (state === STRING) {
      if (esc) { t += ESCAPES[c] || c; esc = false }
      else if (c === '\\') esc = true;
      else if (c === '"') {
        r.push(StringT(t));
        t = '', state = START;
      } else t += c;
    } else if (state === CHAR) {
      if (esc) { t += ESCAPES[c] || c; esc = false }
      else if (c === '\\') esc = true;
      else if (c === "'") {
        if (t.length === 0 || t.length > 1)
          return err(`invalid char literal: '${JSON.stringify(t).slice(1, -1)}'`);
        r.push(CharT(t));
        t = '', state = START;
      } else t += c;
    } else if (state === COMMENT) {
      if (c === '\n') state = START;
    }
  }
  if (b.length > 0) return err(`unclosed brackets: ${b.join(' ')}`);
  if (state === STRING) return err(`unclosed string: "${t}`);
  if (state === CHAR) return err(`unclosed char: '${t}`)
  if (state !== START && state !== COMMENT)
    return err('invalid tokenize end state');
  if (esc) return err(`escape is true after tokenize`);
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
  log(() => `parseTokenKind ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': return KCon(ts.val);
    case 'ParenT': return parseParensKind(ts.val);
    default: return err(`stuck on ${showToken(ts)} in kind`);
  }
};

const parseParensKind = (ts: Token[]): Kind => {
  log(() => `parseParensKind ${showTokens(ts)}`);
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
    if (ts.length === 0) return err(`empty kind ->`);
    if (ts.length > 1) return err(`kind applications unimplemented`);
    return parseTokenKind(ts[0]);
  }));
};

// types
const isCon = (x: Name): boolean =>
  /[A-Z]/.test(x[0]);

const parseTokenType = (ts: Token): Type => {
  log(() => `parseTokenType ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': {
      if (KEYWORDS_TYPE.indexOf(ts.val) >= 0) return err(`stuck on ${ts.val}`);
      return isCon(ts.val) ? TCon(ts.val) : TVar(ts.val);
    }
    case 'SymbolT': {
      if (ts.val === '->') return tFun;
      return err(`stuck on ${ts.val}`);
    }
    case 'ParenT': return parseParensType(ts.val);
    default: return err(`stuck on ${showToken(ts)} in type`);
  }
};

const parseTypePat = (ts: Token): [Name, Kind | null][] => {
  log(() => `parseTypePat ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': {
      if (KEYWORDS_TYPE.indexOf(ts.val) >= 0 || isCon(ts.val)) return err(`stuck on ${ts.val}`);
      return [[ts.val, null]];
    }
    case 'ParenT': {
      const parts = splitTokens(ts.val, t => matchSymbolT(':', t));
      if (parts.length !== 2) return err(`invalid use of : in forall argument`);
      const as = parts[0].map(t => {
        if (t.tag !== 'VarT' || KEYWORDS_TYPE.indexOf(t.val) >= 0)
          return err(`not a valid arg in forall: ${t.val}`);
        return t.val;
      });
      const ki = parseParensKind(parts[1]);
      return as.map(x => [x, ki]);
    }
    default: return err(`stuck on ${showToken(ts)} in forall pattern`);
  }
};

const parseParensType = (ts: Token[]): Type => {
  log(() => `parseParensType ${showTokens(ts)}`);
  if (ts.length === 0) return TCon('Unit');
  if (ts.length === 1) return parseTokenType(ts[0]);
  if (matchVarT('forall', ts[0])) {
    const args: [Name, Kind | null][] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no . after forall`);
      if (matchSymbolT('.', c)) break;
      const ps = parseTypePat(c);
      for (let j = 0; j < ps.length; j++) args.push(ps[j]);
    }
    if (args.length === 0) return err(`forall without args`);
    const body = parseParensType(ts.slice(i));
    return tforall(args, body);
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
  if (fs.length === 2) {     
    // special case (t ->)
    if (fs[1].length === 0) {
      return TApp(tFun, tappFrom(fs[0].map(parseTokenType)));
    // special case (-> t)
    } else if (fs[0].length === 0) {
      return TApp(tFun, tappFrom(fs[1].map(parseTokenType)));
    }
  }
  return tfunFrom(fs.map(ts => {
    if (ts.length === 0) return err(`empty type ->`);
    return tappFrom(ts.map(parseTokenType))
  }));
};

// terms
const parseToken = (ts: Token): Term => {
  log(() => `parseToken ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': {
      if (KEYWORDS.indexOf(ts.val) >= 0) return err(`stuck on ${ts.val}`);
      return Var(ts.val);
    }
    case 'SymbolT': {
      if (ts.val === '<|') {
        const f = 'f';
        const x = 'x';
        return abs([PVar(f), PVar(x)], appFrom([Var(f), Var(x)]));
      }
      if (ts.val === '|>') {
        const f = 'f';
        const x = 'x';
        return abs([PVar(x), PVar(f)], appFrom([Var(f), Var(x)]));
      }
      if (ts.val === '<<') {
        const f = 'f';
        const g = 'g';
        const x = 'x';
        return abs([PVar(f), PVar(g), PVar(x)], App(Var(f), App(Var(g), Var(x))));
      }
      if (ts.val === '>>') {
        const f = 'f';
        const g = 'g';
        const x = 'x';
        return abs([PVar(g), PVar(f), PVar(x)], App(Var(f), App(Var(g), Var(x))));
      }
      return err(`stuck on ${ts.val}`);
    }
    case 'ParenT': return parseParens(ts.val);
    case 'StringT': {
      return LitStr(ts.val);
    }
    case 'NumberT': {
      let val = ts.val;
      let neg = val[0] === '_';
      val = (neg ? val.slice(1) : val).replace(/\_/g, '');
      if (/^[0-9]+$/.test(val)) {
        if (neg) return err(`natural number cannot be negative: ${ts.val}`);
        return LitNat(val);
      }
      if (/^[0-9]+i$/.test(val)) {
        const num = val.slice(0, -1);
        return LitInt(num, neg);
      }
      if (/^[0-9]+r[0-9]*$/.test(val)) {
        const [a, b] = val.split('r');
        return LitRat(a, b || '1', neg);
      }
      return err(`invalid number: ${ts.val}`);
    }
    case 'CharT': {
      return LitChar(ts.val);
    }
    case 'HoleT': {
      return Hole(ts.val);
    }
  }
};

const parsePat = (ts: Token): Pat[] => {
  log(() => `parsePat ${showToken(ts)}`);
  switch (ts.tag) {
    case 'VarT': {
      if (KEYWORDS.indexOf(ts.val) >= 0 || isCon(ts.val))
        return err(`stuck on ${ts.val}`);
      return [PVar(ts.val)];
    }
    case 'SymbolT': {
      if (ts.val === '_') return [PWildcard];
      return err(`stuck on ${ts.val}`);
    }
    case 'ParenT': {
      const a = ts.val;
      if (a.length === 0) return [PAnn(PWildcard, TCon('Unit'))];
      if (a.length === 1) return parsePat(a[0]);
      if (a.length === 2 && a[0].tag === 'VarT' && isCon(a[0].val as string)) {
        const con = a[0].val as string;
        const pat = parsePat(a[1]);
        if (pat.length !== 1)
          return err(`con with too many arguments: ${con}`);
        return [PCon(con, pat[0])];
      }
      const args: Pat[] = [];
      let i = 0;
      while (true) {
        const c = a[i++];
        if (!c) return err(`no : in abs annotation`);
        if (matchSymbolT(':', c)) break;
        const ps = parsePat(c);
        for (let j = 0; j < ps.length; j++) args.push(ps[j]);
      }
      if (args.length === 0) return err(`empty args in abs annotation`);
      const ty = parseParensType(a.slice(i));
      return args.map(p => PAnn(p, ty));
    }
    default: return err(`stuck on ${showToken(ts)} in pattern`);
  }
};

const parseParens = (ts: Token[]): Term => {
  log(() => `parseParens ${showTokens(ts)}`);
  if (ts.length === 0) return Var('unit');
  if (ts.length === 1) return parseToken(ts[0]);
  if (contains(ts, t => matchSymbolT(':', t))) {
    const parts = splitTokens(ts, t => matchSymbolT(':', t));
    if (parts.length !== 2) return err(`invalid use (${parts.length}) of :`);
    const left = parseParens(parts[0]);
    const right = parseParensType(parts[1]);
    return Ann(left, right);
  }
  if (matchSymbolT('\\', ts[0])) {
    const args: Pat[] = [];
    let i = 1;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no -> after \\`);
      if (matchSymbolT('->', c)) break;
      const ps = parsePat(c);
      for (let j = 0; j < ps.length; j++) args.push(ps[j]);
    }
    if (args.length === 0) args.push(PWildcard);
    const body = parseParens(ts.slice(i));
    return abs(args, body);
  }
  if (matchVarT('let', ts[0])) {
    if (ts.length < 2) return err(`let without name`);
    const names = parsePat(ts[1]);
    if (names.length !== 1) return err(`too many/few patterns in let`);
    const name = names[0];
    const args: Pat[] = [];
    let i = 2;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no = after let`);
      if (matchSymbolT('=', c)) break;
      const ps = parsePat(c);
      for (let j = 0; j < ps.length; j++) args.push(ps[j]);
    }
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no in after = in let`);
      if (matchVarT('in', c)) break;
      bodyts.push(c);
    }
    const body = parseParens(bodyts);
    const rest = parseParens(ts.slice(i));
    return Let(name, args.length > 0 ? abs(args, body) : body , rest);
  }
  if (matchVarT('if', ts[0])) {
    let i = 1;
    const condts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no then after if`);
      if (matchVarT('then', c)) break;
      condts.push(c);
    }
    const truets: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no else after then after if`);
      if (matchVarT('else', c)) break;
      truets.push(c);
    }
    const cond = parseParens(condts);
    const true_ = parseParens(truets);
    const false_ = parseParens(ts.slice(i));
    return If(cond, true_, false_);
  }
  if (contains(ts, t => matchSymbolT('<|', t))) {
    const split = splitTokens(ts, t => matchSymbolT('<|', t));
    // special case
    if (split.length === 2) {
      // (f <|) = \x -> f x
      if (split[1].length === 0) {
        const f = parseParens(split[0]);
        const x = '_x';
        return abs([PVar(x)], App(f, Var(x)));
      // (<| x) = \f -> f x
      } else if (split[0].length === 0) {
        const f = '_f';
        const x = parseParens(split[1]);
        return abs([PVar(f)], App(Var(f), x));
      }
    }
    const terms = split.map(parseParens);
    return terms.reduceRight((x, y) => App(y, x));
  }
  if (contains(ts, t => matchSymbolT('|>', t))) {
    const split = splitTokens(ts, t => matchSymbolT('|>', t));
    // special case
    if (split.length === 2) {
      // (x |>) = \f -> f x
      if (split[1].length === 0) {
        const f = '_f';
        const x = parseParens(split[0]);
        return abs([PVar(f)], App(Var(f), x));
      // (|> f) = \x -> f x
      } else if (split[0].length === 0) {
        const f = parseParens(split[1]);
        const x = '_x';
        return abs([PVar(x)], App(f, Var(x)));
      }
    }
    const terms = split.map(parseParens);
    return terms.reverse().reduceRight((x, y) => App(y, x));
  }
  if (contains(ts, t => matchSymbolT('<<', t))) {
    const split = splitTokens(ts, t => matchSymbolT('<<', t));
    // special case
    if (split.length === 2) {
      // (f <<) = \g x -> f (g x)
      if (split[1].length === 0) {
        const f = parseParens(split[0]);
        const g = '_g';
        const x = '_x';
        return abs([PVar(g), PVar(x)], App(f, App(Var(g), Var(x))));
      // (<< g) = \f x -> f (g x)
      } else if (split[0].length === 0) {
        const f = '_f';
        const g = parseParens(split[1]);
        const x = '_x';
        return abs([PVar(f), PVar(x)], App(Var(f), App(g, Var(x))));
      }
    }
    const terms = split.map(parseParens);
    const x = '_x';
    return abs([PVar(x)], terms.reduceRight((x, y) => App(y, x), Var(x)));
  }
  if (contains(ts, t => matchSymbolT('>>', t))) {
    const split = splitTokens(ts, t => matchSymbolT('>>', t));
    // special case
    if (split.length === 2) {
      // (f >>) = \g x -> g (f x)
      if (split[1].length === 0) {
        const f = parseParens(split[0]);
        const g = '_g';
        const x = '_x';
        return abs([PVar(g), PVar(x)], App(Var(g), App(f, Var(x))));
      // (>> g) = \f x -> g (f x)
      } else if (split[0].length === 0) {
        const f = '_f';
        const g = parseParens(split[1]);
        const x = '_x';
        return abs([PVar(f), PVar(x)], App(g, App(Var(f), Var(x))));
      }
    }
    const terms = split.map(parseParens);
    const x = '_x';
    return abs([PVar(x)], terms.reverse().reduceRight((x, y) => App(y, x), Var(x)));
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
export type ImportMap = { [key: string]: boolean }
const parseParensDefs = async (ts: Token[], map: ImportMap): Promise<Def[]> => {
  if (ts.length === 0) return [];
  if (matchVarT('import', ts[0])) {
    if (ts[1].tag !== 'VarT')
      return err(`invalid import name: ${ts[1].val}`);
    const name = ts[1].val as string;
    let ds: Def[];
    if (!map[name]) {
      map[name] = true;
      const file = await loadPromise(name);
      ds = await parseParensDefs(tokenize(file), map);
    } else ds = [];
    const rest = await parseParensDefs(ts.slice(2), map);
    return ds.concat(rest);
  }
  if (matchVarT('type', ts[0])) {
    if (ts[1].tag !== 'VarT' || !isCon(ts[1].val as string))
      return err(`invalid type name: ${ts[1].val}`);
    const tname = ts[1].val as string;
    const args: [Name, Kind | null][] = [];
    let i = 2;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no = after type def`);
      if (matchSymbolT('=', c)) break;
      const ps = parseTypePat(c);
      for (let j = 0; j < ps.length; j++) args.push(ps[j]);
    }
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0)) break;
      bodyts.push(c);
    }
    const body = parseParensType(bodyts);
    const rest = await parseParensDefs(ts.slice(i - 1), map);
    return [DType(tname, args, body) as Def].concat(rest);
  }
  if (matchVarT('let', ts[0])) {
    if (ts.length < 2) return err(`let without name`);
    if (ts[1].tag !== 'VarT' || isCon(ts[0].val as string))
      return err(`invalid name for let`);
    const name = ts[1].val as string;
    const args: Pat[] = [];
    let i = 2;
    while (true) {
      const c = ts[i++];
      if (!c) return err(`no = after let`);
      if (matchSymbolT('=', c)) break;
      const ps = parsePat(c);
      for (let j = 0; j < ps.length; j++) args.push(ps[j]);
    }
    const bodyts: Token[] = [];
    while (true) {
      const c = ts[i++];
      if (!c || (c.tag === 'VarT' && KEYWORDS_DEF.indexOf(c.val) >= 0)) break;
      bodyts.push(c);
    }
    const body = parseParens(bodyts);
    const rest = await parseParensDefs(ts.slice(i - 1), map);
    return [DLet(name, args, body) as Def].concat(rest);
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
export const parseDefs = (sc: string, map: ImportMap): Promise<Def[]> => {
  const ts = tokenize(sc);
  return parseParensDefs(ts, map);
};

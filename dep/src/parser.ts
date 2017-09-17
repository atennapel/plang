import {
  Term,
  Abs,
  abs,
  Pi,
  pi,
  vr,
  app,
  uni,
  nat,
  z,
  s,
  natElim,
} from './index';

abstract class Token {
  abstract toString(): string;
}

class End extends Token {
  toString() {
    return '.';
  }
}

class LamBegin extends Token {
  toString() {
    return '\\';
  }
}

class PiBegin extends Token {
  toString() {
    return '/';
  }
}

class Name extends Token {
  readonly name: string;

  constructor(name: string) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }
}

function tokenToTerm(t: Token): Term {
  if(t instanceof Name) {
    const n = + t.name;
    if(!isNaN(n)) {
      let c = z;
      for(let i = 0; i < n; i++)
        c = s(c);
      return c;
    }
    if(t.name[0] === '*') {
      if(t.name.length === 1) return uni(0);
      const n = +t.name.slice(1);
      if(isNaN(n)) return vr(t.name);
      return uni(Math.abs(Math.floor(n)));
    } else if(t.name === 'Nat') return nat;
    else if(t.name === 'Z') return z;
    return vr(t.name);
  }
  throw new SyntaxError(`cannot convert to term: ${t}`);
}

function tokensToTerm(t: Token[]): Term {
  return app.apply(null, t.map(tokenToTerm));
}

function makeAbs(v: Token[], rest: Token[]): Term {
  if(v.length % 2 !== 0) throw new SyntaxError('odd number of args');
  const r: [string, any][] = [];
  for(let i = 0; i < v.length; i += 2) {
    const k = v[i], t = v[i+1];
    if(!(k instanceof Name)) throw new SyntaxError(`invalid name in args: ${k}`);
    r.push([k.name, tokenToTerm(t)]);
  }
  return abs(r, tokensToTerm(rest));
}

function makePi(v: Token[], rest: Token[]): Term {
  if(v.length % 2 !== 0) throw new SyntaxError('odd number of args');
  const r: [string, any][] = [];
  for(let i = 0; i < v.length; i += 2) {
    const k = v[i], t = v[i+1];
    if(!(k instanceof Name)) throw new SyntaxError(`invalid name in args: ${k}`);
    r.push([k.name, tokenToTerm(t)]);
  }
  return pi(r, tokensToTerm(rest));
}

function handleList(a: Token[]): Term {
  const START = 0, LAM = 1, PI = 2;
  let state = START, t = [], r = [];
  for(let i = 0, l = a.length; i <= l; i++) {
    const c = a[i] || null;
    if(state === START) {
      if(c instanceof LamBegin) state = LAM;
      else if(c instanceof PiBegin) state = PI;
      else if(c) r.push(tokenToTerm(c));
    } else if(state === LAM) {
      if(c instanceof End)
        r.push(makeAbs(t, a.slice(i + 1))), i = l;
      else t.push(c);
    } else if(state === PI) {
      if(c instanceof End)
        r.push(makePi(t, a.slice(i + 1))), i = l;
      else t.push(c);
    }
  }
  if(r.length === 0) return vr('Unit');
  if(r.length === 1) return r[0];
  return app.apply(null, r);
}

export default function parse(s: string): Term {
  const START = 0, NAME = 1;
  let state = START, t = '', p: any[][] = [], r: any[] = [], l = 0;
  for(let i = 0, l = s.length; i <= l; i++) {
    const c = s[i] || ' ';
    if(state === START) {
      if(c === '(') l++, p.push(r), r = [];
      else if(c === ')') {
        if(l === 0) throw new SyntaxError('unmatched brackets');
        l--;
        const a = r;
        r = p.pop() || [];
        r.push(handleList(a));
      }
      else if(c === '.') r.push(new End());
      else if(c === '\\') r.push(new LamBegin());
      else if(c === '/') r.push(new PiBegin());
      else if(/[a-z0-9\*\_\']/i.test(c)) t += c, state = NAME;
    } else if(state === NAME) {
      if(/[a-z0-9\*\_\']/i.test(c)) t += c;
      else r.push(new Name(t)), t = '', i--, state = START;
    }
  }
  if(l !== 0) throw new SyntaxError('unmatched brackets');
  if(state !== START) throw new SyntaxError('parse error');
  return handleList(r);
}

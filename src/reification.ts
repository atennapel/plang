import { MAbs, MBVar, MFVar, MClos, MApp, mapp, MExec, MTop, MState, steps, showMClos, makeClos, GEnv } from './machine';
import { Type, TCon, TApp } from './types';
import { Name, impossible } from './util';
import { tNat } from './inference';

const _id = MAbs(MBVar(0));
const _iterBNat = MFVar('iterBNat');
const _if = MFVar('if');
const _casePair = MFVar('casePair');
const _caseSum = MFVar('caseSum');
const _foldr = MFVar('foldr');

const matchTCon = (t: Type, name: Name): t is TCon =>
  t.tag === 'TCon' && t.name === name;
const matchTApp = (t: Type, name: Name): t is TApp =>
  t.tag === 'TApp' && t.left.tag === 'TCon' && t.left.name === name;
const matchTApp2 = (t: Type, name: Name): t is TApp =>
  t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
    t.left.left.name === name;
export const reify = (v: MClos, t: Type, env: GEnv): any => {
  if (matchTCon(t, 'Nat')) {
    let n = 0n;
    const mt = mapp(
      _iterBNat,
      v.abs,
      MAbs(_id),
      MAbs(MApp(MApp(MBVar(0), _id), MExec('twice', () => { n *= 2n; return true }, _id))),
      MAbs(MApp(MApp(MBVar(0), _id), MExec('twicePlusOne', () => { n = (n * 2n) + 1n; return true }, _id))),
    );
    const st = MState(mt, v.env, MTop);
    steps(env, st);
    return n;
  }
  if (matchTCon(t, 'Int')) {
    const [a, b] = reify(v, TApp(TApp(TCon('Pair'), TCon('Nat')), TCon('Nat')), env);
    const na = reify(a, TCon('Nat'), env);
    const nb = reify(b, TCon('Nat'), env);
    return na - nb;
  }
  if (matchTCon(t, 'Rat')) {
    const [a, b] = reify(v, TApp(TApp(TCon('Pair'), TCon('Int')), TCon('Nat')), env);
    const na = reify(a, TCon('Int'), env);
    const nb = reify(b, TCon('Nat'), env);
    return [na, nb];
  }
  if (matchTCon(t, 'Bool')) {
    let b = false;
    const mt = mapp(
      _if,
      v.abs,
      MAbs(MExec('true', () => { b = true; return true }, _id)),
      _id,
    );
    const st = MState(mt, v.env, MTop);
    steps(env, st);
    return b;
  }
  if (matchTApp2(t, 'Pair')) {
    let p: [any, any] = [null, null];
    const mt = mapp(
      _casePair,
      v.abs,
      MAbs(MAbs(mapp(
        _id,
        MExec('fst', st => { p[0] = makeClos(st.term as MAbs, st.env); return true }, MBVar(1)),
        MExec('snd', st => { p[1] = makeClos(st.term as MAbs, st.env); return true }, MBVar(0))))),
    );
    const st = MState(mt, v.env, MTop);
    steps(env, st);
    return p;
  }
  if (matchTApp2(t, 'Sum')) {
    let s: [boolean, any] = [false, null];
    const mt = mapp(
      _caseSum,
      v.abs,
      MAbs(MExec('inl', st => { s[0] = true; s[1] = makeClos(st.term as MAbs, st.env); return true }, MBVar(0))),
      MAbs(MExec('inr', st => { s[1] = makeClos(st.term as MAbs, st.env); return true }, MBVar(0))),
    );
    const st = MState(mt, v.env, MTop);
    steps(env, st);
    return s;
  }
  if (matchTApp(t, 'List')) {
    const a: MClos[] = [];
    const mt = mapp(
      _foldr,
      MAbs(MAbs(MExec('push', st => { a.push(makeClos(st.term as MAbs, st.env)); return true }, MBVar(1)))),
      _id,
      v.abs,
    );
    const st = MState(mt, v.env, MTop);
    steps(env, st);
    return a.reverse();
  }
  if (matchTCon(t, 'Str')) {
    const l = reify(v, TApp(TCon('List'), TCon('Nat')), env);
    return l.map((v: MClos) => String.fromCharCode(Number(reify(v, TCon('Nat'), env)))).join('');
  }
  return impossible('reify');
};

export const showReifyClos = (v: MClos, t: Type, env: GEnv): string => {
  if (matchTCon(t, 'Unit')) return '()';
  if (matchTCon(t, 'Bool')) return `${reify(v, t, env)}`;
  if (matchTCon(t, 'Nat')) return `${reify(v, t, env)}`;
  if (matchTCon(t, 'Int')) return `${reify(v, t, env)}`;
  if (matchTCon(t, 'Rat')) {
    const [a, b] = reify(v, t, env);
    return `${a}/${b}`;
  }
  if (matchTCon(t, 'Char')) return `'${JSON.stringify(String.fromCharCode(Number(reify(v, tNat, env)))).slice(1, -1)}'`;
  if (matchTApp2(t, 'Pair')) {
    const [a, b] = reify(v, t, env);
    const sa = showReifyClos(a, (t.left as any).right, env);
    const sb = showReifyClos(b, t.right, env);
    return `(${sa}, ${sb})`;
  }
  if (matchTApp2(t, 'Sum')) {
    const [a, b] = reify(v, t, env);
    const s = a ? showReifyClos(b, (t.left as any).right, env) : showReifyClos(b, t.right, env);
    return `(${a ? 'L' : 'R'} ${s})`;
  }
  if (matchTApp(t, 'List')) {
    return `[${reify(v, t, env).map((x: MClos) => showReifyClos(x, t.right, env)).join(', ')}]`;
  }
  if (matchTCon(t, 'Str')) return JSON.stringify(reify(v, t, env));
  if (matchTApp(t, 'IO')) return 'IO';
  return showMClos(v);
};

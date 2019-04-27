const VAR = 0;
const ABS = 1;
const APP = 2;
const EXEC = 3;
const Var = ix => ({ tag: VAR, ix });
const Abs = body => ({ tag: ABS, body });
const App = (left, right) => ({ tag: APP, left, right });
function app() { return Array.from(arguments).reduce(App) }
const Exec = (name, fn, body) => ({ tag: EXEC, name, fn, body });

const showTerm = term => {
  if (term.tag === VAR) return `${term.ix}`;
  if (term.tag === ABS) return `(\\${showTerm(term.body)})`;
  if (term.tag === APP) return `(${showTerm(term.left)} ${showTerm(term.right)})`;
  if (term.tag === EXEC) return `(exec ${term.name} ${showTerm(term.body)})`;
};

const Nil = [];
const Cons = (head, tail) => {
  const l = tail.length;
  const t = Array(l + 1);
  t[0] = head;
  for (let i = 0; i < l; i++) t[i + 1] = tail[i];
  return t;
}
const lookup = (env, ix) => env[ix] || null;

const showEnv = list => {
  return `[${list.join(', ')}]`;
};

const free = (term, fr = {}, under = 0) => {
  if (term.tag === VAR) {
    const ix = term.ix - under;
    if (ix >= 0) fr[ix] = true;
    return ix;
  }
  if (term.tag === ABS) {
    const max = free(term.body, fr, under + 1);
    return max;
  }
  if (term.tag === APP) {
    const a = free(term.left, fr, under);
    const b = free(term.right, fr, under);
    return Math.max(a, b);
  }
  if (term.tag === EXEC) return free(term.body, fr, under);
};
const makeClosEnv = (fr, max, env, i = 0) => {
  const nenv = env.slice(0, max + 1);
  for (let i = 0, l = nenv.length; i < l; i++) {
    if (!fr[i]) nenv[i] = null;
  }
  return nenv;
};
const Clos = (abs, env) => ({ abs, env });
const makeClos = (abs, env) => {
  const fr = {};
  const max = free(abs, fr);
  const nenv = makeClosEnv(fr, max, env);
  return Clos(abs, nenv);
};

const showClos = clos => `{${showTerm(clos.abs)}@${showEnv(clos.env)}}`;

const TOP = 0;
const ARG = 1;
const FUN = 2;
const Top = { tag: TOP };
const Arg = (term, env, rest) => ({ tag: ARG, term, env, rest });
const Fun = (body, env, rest) => ({ tag: FUN, body, env, rest });

const showCont = cont => {
  if (cont.tag === TOP) return 'Top';
  if (cont.tag === ARG) return `Arg(${showTerm(cont.term)}, ${showEnv(cont.env)}):${showCont(cont.rest)}`;
  if (cont.tag === FUN) return `Fun(${showTerm(cont.body)}, ${showEnv(cont.env)}):${showCont(cont.rest)}`;
};

const State = (term, env, cont) => ({ term, env, cont });

const showState = st => `(${showTerm(st.term)}, ${showEnv(st.env)}, ${showCont(st.cont)})`;

const step = state => {
  const { term, env, cont } = state;
  if (term.tag === VAR) {
    const v = lookup(env, term.ix);
    if (!v) return false;
    state.term = v.abs;
    state.env = v.env;
    return true;
  }
  if (term.tag === APP) {
    state.term = term.left;
    state.cont = Arg(term.right, env, cont);
    return true;
  }
  if (term.tag === EXEC) {
    term.fn();
    state.term = term.body;
    return true;
  }
  if (cont.tag === ARG) {
    state.term = cont.term;
    state.env = cont.env;
    state.cont = Fun(term.body, env, cont.rest);
    return true;
  }
  if (cont.tag === FUN) {
    state.term = cont.body;
    state.env = Cons(makeClos(term, env), cont.env);
    state.cont = cont.rest;
    return true;
  }
  return false;
};
const steps = state => {
  let i = 0;
  //console.log(i, showTerm(state.term), state.cont.tag);
  while (step(state)) {
    i++;
    //console.log(i, showTerm(state.term), state.cont.tag);
  }
  return i;
};
const initial = term => State(term, Nil, Top);

const tI = Abs(Var(0));
const tz = Abs(Abs(Var(0)));
const ts = Abs(Abs(Abs(App(Var(1), app(Var(2), Var(1), Var(0))))));

const tnil = Abs(Abs(Var(0)));
const tcons = Abs(Abs(Abs(Abs(app(Var(1), Var(3), app(Var(2), Var(1), Var(0)))))));

const reifyNat = t => {
  let i = 0;
  const term = app(t, Abs(Exec('inc', () => i++, Var(0))), tI);
  let time = Date.now();
  const n = steps(initial(term));
  time = Date.now() - time;
  console.log(n, time, i);
  return i;
};
const reifyList = l => {
  const r = [];
  let i = 0;
  const term = app(
    l,
    Abs(Abs(
      app(
        Abs(Exec('push', () => r.push(i), Var(0))),
        app(
          Var(1),
          Abs(Exec('inc', () => i++, Var(0))),
          Exec('reset', () => i = 0, Var(0)))))), tI);
  const state = initial(term);
  steps(state);
  return r.reverse();
};
const makeNat = n => {
  let c = tz;
  for (let i = 0; i < n; i++) c = app(ts, c);
  return c;
};

const arr = reifyNat(makeNat(1000000));


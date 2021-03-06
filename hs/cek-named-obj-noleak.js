const VAR = 0;
const ABS = 1;
const APP = 2;
const EXEC = 3;
const Var = name => ({ tag: VAR, name });
const Abs = (name, body) => ({ tag: ABS, name, body });
const App = (left, right) => ({ tag: APP, left, right });
function app() { return Array.from(arguments).reduce(App) }
const Exec = (name, fn, body) => ({ tag: EXEC, name, fn, body });

const showTerm = term => {
  if (term.tag === VAR) return term.name;
  if (term.tag === ABS) return `(\\${term.name}. ${showTerm(term.body)})`;
  if (term.tag === APP) return `(${showTerm(term.left)} ${showTerm(term.right)})`;
  if (term.tag === EXEC) return `(exec ${term.name} ${showTerm(term.body)})`;
};

const extend = (name, clos, env) => {
  const nenv = {};
  for (let k in env) nenv[k] = env[k];
  nenv[name] = clos;
  return nenv;
};
const empty = {};
const lookup = (env, name) => env[name] || null;

const showEnv = env => {
  const r = [];
  for (let key in env) {
    r.push(`${key}: ${showClos(env[key])}`);
  }
  return `[${r.join(', ')}]`;
};

const free = (term, fr) => {
  if (term.tag === VAR) { fr[term.name] = true; return }
  if (term.tag === ABS) { free(term.body, fr); fr[term.name] = false; return };
  if (term.tag === APP) { free(term.left, fr); free(term.right, fr); return };
  if (term.tag === EXEC) { free(term.body, fr); return }
};
const makeEnv = (env, fr) => {
  const nenv = {};
  for (let k in env) {
    if (fr[k]) nenv[k] = env[k];
  }
  return nenv;
};
const Clos = (abs, env) => ({ abs, env });
const makeClos = (abs, env) => {
  const fr = {};
  free(abs, fr);
  return Clos(abs, makeEnv(env, fr));
};

const showClos = clos => `{${showTerm(clos.abs)}@${showEnv(clos.env)}}`;

const TOP = 0;
const ARG = 1;
const FUN = 2;
const Top = { tag: TOP };
const Arg = (term, env, rest) => ({ tag: ARG, term, env, rest });
const Fun = (abs, env, rest) => ({ tag: FUN, abs, env, rest });

const showCont = cont => {
  if (cont.tag === TOP) return 'Top';
  if (cont.tag === ARG) return `Arg(${showTerm(cont.term)}, ${showEnv(cont.env)}):${showCont(cont.rest)}`;
  if (cont.tag === FUN) return `Fun(${showTerm(cont.abs)}, ${showEnv(cont.env)}):${showCont(cont.rest)}`;
};

const State = (term, env, cont) => ({ term, env, cont });

const showState = st => `(${showTerm(st.term)}, ${showEnv(st.env)}, ${showCont(st.cont)})`;

const step = state => {
  const { term, env, cont } = state;
  if (term.tag === VAR) {
    const v = lookup(env, term.name);
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
    state.cont = Fun(term, env, cont.rest);
    return true;
  }
  if (cont.tag === FUN) {
    const fn = cont.abs;
    state.term = fn.body;
    state.env = extend(fn.name, makeClos(term, env), cont.env);
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
const initial = term => State(term, empty, Top);

const tI = Abs('x', Var('x'));
const tz = Abs('f', Abs('x', Var('x')));
const ts = Abs('n', Abs('f', Abs('x', App(Var('f'), app(Var('n'), Var('f'), Var('x'))))));

const reifyNat = t => {
  let i = 0;
  const term = app(t, Abs('x', Exec('inc', () => i++, Var('x'))), tI);
  let time = Date.now();
  const n = steps(initial(term));
  time = Date.now() - time;
  console.log(n, time, i);
  return i;
};
const makeNat = n => {
  let c = tz;
  for (let i = 0; i < n; i++) c = app(ts, c);
  return c;
};

const arr = reifyNat(makeNat(1000000));


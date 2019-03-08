const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });

const showExpr = e => {
  if (e.tag === 'Var') return e.name;
  if (e.tag === 'Abs') return `(\\${e.name} -> ${showExpr(e.body)})`;
  if (e.tag === 'App') return `(${showExpr(e.left)} ${showExpr(e.right)})`;
};

module.exports = {
  Var,
  Abs,
  App,

  showExpr,
};

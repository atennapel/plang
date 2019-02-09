const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });

const showExpr = e => {
  switch (e.tag) {
    case 'Var': return e.name;
    case 'Abs': return `(\\${e.name}. ${showExpr(e.body)})`;
    case 'App':
      return `(${showExpr(e.left)} ${showExpr(e.right)})`;
  }
};

module.exports = {
  Var,
  Abs,
  App,
  showExpr,
};

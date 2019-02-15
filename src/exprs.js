const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });
const Con = (con, arg) => ({ tag: 'Con', con, arg });
const Decon = (con, name, body) => ({ tag: 'Decon', con, name, body });
const LitStr = val => ({ tag: 'LitStr', val });

const showExpr = e => {
  switch (e.tag) {
    case 'Var': return e.name;
    case 'Abs': return `(\\${e.name}. ${showExpr(e.body)})`;
    case 'App':
      return `(${showExpr(e.left)} ${showExpr(e.right)})`;
    case 'Con': return `(${e.con} ${showExpr(e.arg)})`;
    case 'Decon': return `(\\${e.con} ${e.name}. ${showExpr(e.body)})`;
    case 'LitStr': return JSON.stringify(e.val);
  }
};

module.exports = {
  Var,
  Abs,
  App,
  Con,
  Decon,
  LitStr,
  showExpr,
};

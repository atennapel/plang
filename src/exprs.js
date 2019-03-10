const Var = name => ({ tag: 'Var', name });
const Abs = (name, body) => ({ tag: 'Abs', name, body });
const App = (left, right) => ({ tag: 'App', left, right });
const Let = (name, val, body) => ({ tag: 'Let', name, val, body });
const Con = (con, arg) => ({ tag: 'Con', con, arg });
const Decon = (con, name, body) => ({ tag: 'Decon', con, name, body });

const showExpr = e => {
  if (e.tag === 'Var') return e.name;
  if (e.tag === 'Abs') return `(\\${e.name} -> ${showExpr(e.body)})`;
  if (e.tag === 'App') return `(${showExpr(e.left)} ${showExpr(e.right)})`;
  if (e.tag === 'Let') return `(let ${e.name} = ${showExpr(e.val)} in ${showExpr(e.body)})`;
  if (e.tag === 'Con') return `(${e.con} ${showExpr(e.arg)})`;
  if (e.tag === 'Decon') return `\\(${e.con} ${e.name}) -> ${showExpr(e.body)}`;
};

module.exports = {
  Var,
  Abs,
  App,
  Let,
  Con,
  Decon,

  showExpr,
};

var serr = m => { throw new SyntaxError(m) };

var Var = 'Var';
var vr = name => ({
  tag: Var,
  name,
});

var App = 'App';
var app = (left, right) => ({
  tag: App,
  left,
  right,
});

var Lam = 'Lam';
var lam = (arg, body) => ({
  tag: Lam,
  arg,
  body,
});

var Let = 'Let';
var lt = (arg, val, body) => ({
  tag: Let,
  arg,
  val,
  body,
});

var toString = e => {
  if(e.tag === Var) return '' + e.name;
  if(e.tag === App)
    return '(' + toString(e.left) + ' ' + toString(e.right) + ')';
  if(e.tag === Lam)
    return '(\\' + e.arg + ' -> ' + toString(e.body) + ')';
  if(e.tag === Let)
    return '(let ' + e.arg + ' = ' + toString(e.val) + ' in ' +
      toString(e.body) + ')';
  serr('Invalid expression tag toString: ' + e.tag);
};

module.exports = {
  Var,
  vr,

  App,
  app,

  Lam,
  lam,

  Let,
  lt,

  serr,
  toString,
};

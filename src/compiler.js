const compile = e => {
  if (e.tag === 'Var') return e.name;
  if (e.tag === 'Abs') return `(${e.name} => ${compile(e.body)})`;
  if (e.tag === 'App') return `${compile(e.left)}(${compile(e.right)})`;
};

module.exports = {
  compile,
};

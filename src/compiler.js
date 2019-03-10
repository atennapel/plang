const compile = e => {
  if (e.tag === 'Var') return e.name;
  if (e.tag === 'Abs') return `(${e.name} => ${compile(e.body)})`;
  if (e.tag === 'App') return `${compile(e.left)}(${compile(e.right)})`;
  if (e.tag === 'Let') return `(${e.name} => ${compile(e.body)})(${compile(e.val)})`;
  if (e.tag === 'Con') return compile(e.arg);
  if (e.tag === 'Decon') return `(${e.name} => ${compile(e.body)})`;
};

module.exports = {
  compile,
};

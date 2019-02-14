const { TCon } = require('./types');

const primenv = {
  'prim.true': TCon('Prim.Bool'),
  'prim.false': TCon('Prim.Bool'),
};

module.exports = {
  primenv,
};

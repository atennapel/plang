const DType = (name, type) => ({ tag: 'DType', name, type });
const DValue = (name, expr) => ({ tag: 'DValue', name, expr });

module.exports = {
  DType,
  DValue,
};

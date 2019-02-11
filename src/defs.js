const DType = (name, tvs, utvs, etvs, type) => ({ tag: 'DType', name, tvs, utvs, etvs, type });
const DValue = (name, expr) => ({ tag: 'DValue', name, expr });

module.exports = {
  DType,
  DValue,
};

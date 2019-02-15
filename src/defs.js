const DType = (name, tvs, utvs, etvs, type) => ({ tag: 'DType', name, tvs, utvs, etvs, type });
const DValue = (name, expr) => ({ tag: 'DValue', name, expr });
const DDeclare = (name, type) => ({ tag: 'DDeclare', name, type });
const DForeign = (name, val) => ({ tag: 'DForeign', name, val });

module.exports = {
  DType,
  DValue,
  DDeclare,
  DForeign,
};

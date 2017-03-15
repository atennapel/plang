var T = require('./types');

var KCon = 'KCon';
var kcon = name => ({
  tag: KCon,
  name,
});

var KArr = 'KArr';
var karr = (left, right) => ({
  tag: KArr,
  left,
  right,
});

var Star = kcon('*');
var Row = kcon('#');

var equals = (a, b) => {
  if(a.tag === KCon && b.tag === KCon && a.name === b.name)
    return true;
  if(a.tag === KArr && b.tag === KArr)
    return equals(a.left, b.left) && equals(a.right, b.right);
  return false;
}

var toString = k => {
  if(k.tag === KCon) return k.name;
  if(k.tag === KArr)
    return '(' + toString(k.left) + ' -> ' + toString(k.right) + ')';
  T.terr('Invalid kind tag in toString: ' + k.tag);
};

module.exports = {
  KCon,
  kcon,

  KArr,
  karr,

  Star,
  Row,

  equals,
  toString,
};

var T = require('./types');

var KCon = 'KCon';
var kcon = name => ({
  tag: KCon,
  name,
});

var KArr = 'KArr';
var karr2 = (left, right) => ({
  tag: KArr,
  left,
  right,
});
var karr = function() {
  var l = arguments.length;
  if(l < 1) T.terr('karr needs at least 1 argument');
  if(l === 1) return arguments[0];
  var c = karr2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = karr2(arguments[i], c);
  return c;
};

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

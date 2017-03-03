var terr = m => { throw new TypeError(m) };

var KVar = 'KVar';
var id = {};
var fresh = name => {
  if(!id[name]) id[name] = 0;
  return id[name]++;
};
var vars = {};
var kvar = name_ => {
  var name = name_ || 'k';
  var _id = name + fresh(name);
  var v = {
    tag: KVar,
    name,
    id: _id,
    instance: null,
  };
  vars[_id] = v;
  return v;
};
var getKVar = id => vars[id] || null;

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
  if(l < 1) terr('karr needs at least 1 argument');
  if(l === 1) return arguments[0];
  var c = karr2(arguments[l - 2], arguments[l - 1]);
  for(var i = l - 3; i >= 0; i--) c = karr2(arguments[i], c);
  return c;
};

var kstar = kcon('*');
var krow = kcon('#');

var karrToString = (left, right, toplevel) =>
  (toplevel? '(': '') + toString(left) + ' -> ' +
    (right.tag === KArr?
      karrToString(right.left, right.right, false):
      toString(right)) +
  (toplevel? ')': '');
var toString = k =>
  k.tag === KVar?
    (k.id[k.id.length - 1] === '0'? k.id.slice(0, k.id.length - 1): k.id):
  k.tag === KCon? k.name:
  k.tag === KArr? karrToString(k.left, k.right, true):
  terr('Not a kind tag: ' + k.tag);

module.exports = {
  KVar,
  kvar,
  getKVar,

  KCon,
  kcon,

  KArr,
  karr,

  kstar,
  krow,

  toString,
};

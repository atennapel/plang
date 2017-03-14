var terr = m => { throw new TypeError(m) };

var TVar = 'TVar';
var tvar = id => ({
  tag: TVar,
  id,
});

var Con = 'Con';
var con = name => ({
  tag: Con,
  name,
});

var TApp = 'TApp';
var tapp = (left, right) => ({
  tag: TApp,
  left,
  right,
});

var TArr = con('->');
var tarr = (left, right) => tapp(tapp(TArr, left), right);

var Scheme = 'Scheme';
var scheme = (vars, type) => ({
  tag: Scheme,
  vars,
  type,
});

var toString = t => {
  if(t.tag === TVar) return '' + t.id;
  if(t.tag === Con) return '' + t.name;
  if(t.tag === TApp)
    return '(' + toString(t.left) + ' ' + toString(t.right) + ')';
  if(t.tag === Scheme)
    return 'forall' + (t.vars.length? ' ' + t.vars.join(' '): '') + ' . ' +
      toString(t.type);
  terr('Invalid type tag in toString: ' + t.tag);
};

module.exports = {
  TVar,
  tvar,

  Con,
  con,

  TApp,
  tapp,

  TArr,
  tarr,

  Scheme,
  scheme,

  terr,
  toString,
};

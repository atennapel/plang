var E = require('./exprs');

function splitArr(a, v) {
  var i = a.indexOf(v);
  if(i === -1) return a;
  var r = [];
  var c = [];
  for(var i = 0, l = a.length; i < l; i++) {
    if(a[i] === v) {
      r.push(c);
      c = [];
    } else {
      c.push(a[i]);
    }
  }
  r.push(c);
  return r;
}

var APP = ';';

function handleApp(x) {
  return handleApp2(handleApp1(x));
}

function handleApp1(x) {
  if(!Array.isArray(x)) return x;
  if(x.indexOf(APP) >= 0)
    return handleApp1(splitArr(x, APP)
      .map(handleApp1)
      .reduceRight((a, b) => b.concat([a])));
  return x.map(handleApp1);
}

function handleApp2(x) {
  if(typeof x === 'string') {
    if(x === 'end') return E.end;
    if(x === 'handlereturn') return E.handlereturn;
    if(x === 'pure') return E.pure;
    if(x === 'return') return E.retrn;
    return E.vr(x);
  }
  if(x.length === 0) return E.recordempty;
  if(x.length === 1) return handleApp(x[0]);
  var a = x.map(handleApp);
  var fn = a[0];
  if(fn.tag === E.Var) {
    if(fn.name === 'fn')
      return E.lam.apply(null,
        a.slice(1).map((v, i, a) => i <= a.length - 2? v.name: v));
    if(fn.name === 'let')
      return E.lt(a[1].name, a[2], a[3]);
    if(fn.name === 'letr')
      return E.ltr(a[1].name, a[2], a[3]);
    if(fn.name === 'do')
      return E.doo(a[1].name, a[2], a[3]);
    if(fn.name === 'if')
      return E.iff(a[1], a[2], a[3]);
    if(fn.name === 'typeof')
      return E.typeOf(a[1]);
    if(fn.name === 'sel')
      return E.app.apply(null, [E.select(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'extend')
      return E.app.apply(null, [E.extend(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'restrict')
      return E.app.apply(null, [E.restrict(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'rupdate')
      return E.app.apply(null, [E.recordupdate(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'inj')
      return E.app.apply(null, [E.inject(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'embed')
      return E.app.apply(null, [E.embed(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'elim')
      return E.app.apply(null, [E.elim(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'vupdate')
      return E.app.apply(null, [E.variantupdate(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'pack')
      return E.app.apply(null, [E.pack(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'unpack')
      return E.app.apply(null, [E.unpack(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'perform')
      return E.app.apply(null, [E.perform(a[1].name)].concat(a.slice(2)));
    if(fn.name === 'handle')
      return E.app.apply(null, [E.handle(a[1].name)].concat(a.slice(2)));
  }
  return E.app.apply(null, a);
}

function parse(s) {
  return handleApp(JSON.parse(('(' + s + ')')
    .replace(/\(/g, ' [ ')
    .replace(/\)/g, ' ] , ')
    .replace(/\;/g, '";" , ')
    .replace(/[a-z0-9]+/gi, x => JSON.stringify(x) + ' , ')
    .replace(/\,\s*\]/g, ']')
    .trim()
    .slice(0, -1)));
}

module.exports = {
  parse,
};

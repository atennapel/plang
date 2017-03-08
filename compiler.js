var E = require('./exprs');
var T = require('./types');

var id = 0;
var uniq = function() {return '_u' + (id++)};

function reduceInstR(a) {
  return a.reduce((y, x) =>
    '(' + x.inst + reduceInstR(x.children) + ')' + y, '');
}

function reduceInst(e, leftSide) {
  return reduceInstR(e.meta.inst
    .filter(x => x.leftSide === leftSide));
}

function compile(e) {
  //console.log(E.toString(e));
  //console.log(e.meta.inst);
  if(e.tag === E.Var)
    return e.name + reduceInst(e, false);
  if(e.tag === E.App) {
    if(e.meta.expl) {
      var v = uniq();
      return '(' + v + ' => (' + compile(e.left) + reduceInst(e, true) +
      ')(' + v + ')(' +
        (e.meta.lazy? 'lazy(_ => ' + compile(e.right) +
          reduceInst(e, false) + ')':
          e.meta.force? 'force(' + compile(e.right) +
            reduceInst(e, false) + ')':
          compile(e.right) + reduceInst(e, false)) +
      '))';
    }
    return compile(e.left) +
      reduceInst(e, true) +
      '(' +
        (e.meta.lazy? 'lazy(' + uniq() + ' => ' + compile(e.right) +
          reduceInst(e, false) + ')':
          e.meta.force? 'force(' + compile(e.right) +
            reduceInst(e, false) + ')':
          compile(e.right) + reduceInst(e, false)) +
      ')';
  }
  if(e.tag === E.Lam)
    return '(' + e.arg + ' => ' + compile(e.body) + ')';
  if(e.tag === E.Let)
    return e.meta.effect?
      '_do(' + compile(e.val) + ', ' + e.arg + ' => ' + compile(e.body) + ')':
      '(function() {var ' + e.arg + ' = ' + compile(e.val) +
        ';return(' + compile(e.body) + ')})()';
  if(e.tag === E.If)
    return '(' + compile(e.cond) + ' ? ' +
      compile(e.bodyTrue) + ' : ' +
      compile(e.bodyFalse) + ')';

  if(e.tag === E.Type)
    return '(function() {' +
      Object.keys(e.cases)
        .map(k => 'var ' + k + ' = ' +
          (e.cases[k].length === 0?
              'new _FO(' + JSON.stringify(k) + ')':
              e.cases[k].map((_, i) => '_u' + i).join(' => ') +
                ' => new _FO(' + JSON.stringify(k) + ', [' +
                e.cases[k].map((_, i) => '_u' + i).join(', ')
                + '])'))
        .join(';') +
      ';return(' + compile(e.body) + ')})()';

  if(e.tag === E.Anno)
    return compile(e.expr) + reduceInst(e, false);

  if(e.tag === E.Record)
    return '({' +
      Object.keys(e.map).map(k => JSON.stringify(k) + ': ' +
      compile(e.map[k])).join(', ') +
    '})';
  if(e.tag === E.Handle)
    return '_handle({' +
      Object.keys(e.map).map(k => JSON.stringify(k) + ': ' +
      compile(e.map[k])).join(', ') +
    '})';
  if(e.tag === E.Case)
    return '_case({' +
      Object.keys(e.map).map(k => JSON.stringify(k) + ': ' +
      compile(e.map[k])).join(', ') +
    '})';

  if(e.tag === E.List)
    return '[' + e.arr.map(compile).join(', ') + ']';

  if(e.tag === E.RecordEmpty)
    return '({})';

  if(e.tag === E.RecordSelect)
    return '_sel(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.RecordExtend)
    return '_extend(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.RecordRestrict)
    return '_restrict(' + JSON.stringify(e.label) + ')';
  if(e.tag === E.RecordUpdate)
    return '_update(' + JSON.stringify(e.label) + ')';

  if(e.tag === E.Perform)
    return '_perform(' + JSON.stringify(e.label) + ')';

  if(e.tag === E.Int) return '' + e.val;
  if(e.tag === E.Float) return '' + e.val;
  if(e.tag === E.Str) return JSON.stringify(e.val);

  throw new Error('Cannot compile ' + e);
}

module.exports = {
  compile,
};

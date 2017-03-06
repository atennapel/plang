var _T = require('./types');
var _K = require('./kinds');
var _E = require('./exprs');
var _tc = require('./typechecker');
var _parse = require('./parser').parse;
var _compile = require('./compiler').compile;
var _libenv = require('./libenv');
var _env = _tc.makeEnv(_libenv.env);

// for nodejs
var _fs = require('fs');
var _lib = _fs.readFileSync('lib.js', {encoding: 'utf8'});
eval(_lib);

function _stringify(x) {
  var t = typeof x;
  if(
    t === 'number' ||
    t === 'boolean' ||
    t === 'undefined' ||
    x === null
  ) return '' + x;
  if(t === 'string') return JSON.stringify(x);
  if(t === 'function') return '[Function]';
  if(x._fc || x._fcr || x._fo) return x.toString(_stringify);
  if(x instanceof Promise) return '(Promise)';
  if(Array.isArray(x))
    return '[' + x.map(_stringify).join(', ') + ']';
  var r = [];
  for(var k in x) r.push(k + ': ' + _stringify(x[k]));
  return '{' + r.join(', ') + '}';
}

var CMD = ':';

function _eval(s, cb) {
  if(s[0] === CMD) {
    var action = s.split(/\s+/g)[0].slice(1).toLowerCase();
    var s = s.slice(action.length + 1).trim();
    if(action.length === 0)
      return cb('Invalid action: [empty]', true);
    if(action === 't' || action === 'type') {
      try {
        var _expr = _parse(s);
        var _type = _tc.infer(_expr, _env);
        return cb(_T.toString(_type));
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'p' || action === 'parse') {
      try {
        var _expr = _parse(s);
        return cb(_E.toString(_expr));
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'k' || action === 'kind') {
      try {
        var _expr = _parse(s);
        var _type = _tc.infer(_expr, _env);
        return cb(_K.toString(_type.kind));
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'c' || action === 'compile') {
      try {
        var _expr = _parse(s);
        var _type = _tc.infer(_expr, _env);
        var _compiled = _compile(_expr);
        return cb(_compiled);
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'd' || action === 'debug') {
      try {
        var _expr = _parse(s);
        console.log(_E.toString(_expr));
        var _type = _tc.infer(_expr, _env);
        console.log(_T.toString(_type));
        var _compiled = _compile(_expr);
        console.log('' + _compiled);
        var _evalled = eval(_compiled);
        return cb(_stringify(_evalled) + ' : ' + _T.toString(_type));
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'let' || action === 'letr'
            || action === 'ilet' || action === 'iletr') {
      var name = s.split(/\s+/g)[0];
      var s = s.slice(name.length).trim();
      if(name.length === 0 || s.length === 0)
        return cb('Invalid ' + action, true);
      try {
        var _expr = _parse(action + ' ' + name + ' (' + s + ') ' + name);
        var _type = _tc.infer(_expr, _env);
        var _compiled = _compile(_expr);
        var _evalled = eval(_compiled);
        global[name] = _evalled;
        _env.typings[name] = _tc.generalize(_type);
        if(action[0] === 'i')
          _env.impl[name] = true;
        return cb(_stringify(_evalled) + ' : ' + _T.toString(_type));
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'lett' || action === 'lettype') {
      var name = s.split(/\s+/g)[0];
      var s = s.slice(name.length).trim();
      if(name.length === 0 || s.length === 0)
        return cb('Invalid ' + action, true);
      try {
        var _expr0 = _parse('type ' + name + ' ' + s + ' ; ()');
        var _expr = _parse('type ' + name + ' ' + s + ' ; {' +
          Object.keys(_expr0.cases).map(k => k + ' ' + k) + '}');
        var _type = _tc.infer(_expr, _env);
        var _compiled = _compile(_expr);
        var _evalled = eval(_compiled);
        var _row = _type.right;
        while(_row.tag === _T.TRowExtend) {
          var _label = _row.label;
          var _ltype = _row.type;
          global[_label] = _evalled[_label];
          _env.typings[_label] = _tc.generalize(_ltype);
          _row = _row.rest;
        }
        return cb('Defined type ' + name);
      } catch(e) {
        return cb(''+e, true);
      }
    } else if(action === 'import' || action === 'i') {
      var name = s.split(/\s+/g)[0];
      var s = s.slice(name.length).trim();
      if(name.length === 0)
        return cb('Invalid ' + action, true);
      if(s.length === 0) s = '@' + name;
      if(s[0] === '@') {
        try {
          fetch('/lib/' + s.slice(1), {mode: 'no-cors'})
            .then(r => r.text())
            .then(t => {
              try {
                var _expr = _parse(t);
                var _type = _tc.infer(_expr, _env);
                var _compiled = _compile(_expr);
                var _evalled = eval(_compiled);
                global[name] = _evalled;
                _env.typings[name] = _tc.generalize(_type);
                return cb(_T.toString(_type));
              } catch(e) {
                return cb(''+e, true);
              }
            })
            .catch(e => cb(''+e, true));
        } catch(e) {
          cb(''+e, true);
        }
      } else {
        try {
          _fs.readFile(s, {encoding: 'utf8'}, (err, _file) => {
            if(err) return cb(''+err, true);
            try {
              var _expr = _parse(_file);
              var _type = _tc.infer(_expr, _env);
              var _compiled = _compile(_expr);
              var _evalled = eval(_compiled);
              global[name] = _evalled;
              _env.typings[name] = _tc.generalize(_type);
              return cb(_T.toString(_type));
            } catch(e) {
              return cb(''+e, true);
            }
          });
        } catch(e) {
          cb(''+e, true);
        }
      }
    } else if(action === 'use' || action === 'u') {
      if(s.length === 0)
        return cb('Invalid ' + action, true);
      if(s[0] === '@') {
        try {
          fetch('/lib/' + s.slice(1), {mode: 'no-cors'})
            .then(r => r.text())
            .then(t => {
              try {
                var _expr = _parse(t);
                var _type = _tc.infer(_expr, _env);
                var _compiled = _compile(_expr);
                var _evalled = eval(_compiled);

                if(_type.tag !== _T.TApp || _type.left !== _T.TRec)
                  return cb('Cannot use this module', true);

                var _c = _type.right;
                while(_c.tag === _T.TRowExtend) {
                  var _label = _c.label;
                  var implicit = false;
                  if(_label.slice(0, 9) === 'implicit_') {
                    implicit = true;
                    _label = _label.slice(9);
                  }
                  var _ctype = _c.type;
                  global[_label] =
                    _evalled[(implicit? 'implicit_': '') + _label];
                  _env.typings[_label] = _tc.generalize(_ctype);
                  if(implicit) _env.impl[_label] = true;
                  _c = _c.rest;
                }

                return cb(_T.toString(_type));
              } catch(e) {
                return cb(''+e, true);
              }
            })
            .catch(e => cb(''+e, true));
        } catch(e) {
          cb(''+e, true);
        }
      } else {
        try {
          _fs.readFile(s, {encoding: 'utf8'}, (err, _file) => {
            if(err) return cb(''+err, true);
            try {
              var _expr = _parse(_file);
              var _type = _tc.infer(_expr, _env);
              var _compiled = _compile(_expr);
              var _evalled = eval(_compiled);

              if(_type.tag !== _T.TApp || _type.left !== _T.TRec)
                return cb('Cannot use this module', true);

              var _c = _type.right;
              while(_c.tag === _T.TRowExtend) {
                var _label = _c.label;
                var implicit = false;
                if(_label.slice(0, 9) === 'implicit_') {
                  implicit = true;
                  _label = _label.slice(9);
                }
                var _ctype = _c.type;
                global[_label] = _evalled[(implicit? 'implicit_': '') + _label];
                _env.typings[_label] = _tc.generalize(_ctype);
                if(implicit) _env.impl[_label] = true;
                _c = _c.rest;
              }

              return cb(_T.toString(_type));
            } catch(e) {
              return cb(''+e, true);
            }
          });
        } catch(e) {
          cb(''+e, true);
        }
      }
    } else if(action === 'read') {
      var name = s.split(/\s+/g)[0];
      var s = s.slice(name.length).trim();
      if(name.length === 0 || s.length === 0)
        return cb('Invalid ' + action, true);
      if(s[0] === '@') {
        try {
          fetch(s.slice(1), {mode: 'no-cors'})
            .then(r => r.text())
            .then(t => {
              try {
                var _evalled = t;
                global[name] = _evalled;
                _env.typings[name] = _tc.generalize(_T.TStr);
                return cb('Read file: ' + s);
              } catch(e) {
                return cb(''+e, true);
              }
            })
            .catch(e => cb(''+e, true));
        } catch(e) {
          cb(''+e, true);
        }
      } else {
        try {
          _fs.readFile(s, {encoding: 'utf8'}, (err, _file) => {
            if(err) return cb(''+err, true);
            try {
              var _evalled = _file;
              global[name] = _evalled;
              _env.typings[name] = _tc.generalize(_T.TStr);
              return cb('Read file: ' + s);
            } catch(e) {
              return cb(''+e, true);
            }
          });
        } catch(e) {
          cb(''+e, true);
        }
      }
    } else return cb('Invalid action: ' + action, true);
  } else {
    try {
      var _expr = _parse(s);
      var _type = _tc.infer(_expr, _env);
      var _compiled = _compile(_expr);
      var _evalled = eval(_compiled);
      return cb(_stringify(_evalled) + ' : ' + _T.toString(_type));
    } catch(e) {
      return cb(''+e, true);
    }
  }
}

module.exports = {
  eval: _eval
};

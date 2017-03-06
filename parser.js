var T = require('./types');
var E = require('./exprs');

var fs = require('fs');

function typeArrayFrom(e, mappings) {
  if(e.tag === E.Var) return [typeFrom(e, mappings)];
  if(e.tag === E.App)
    return typeArrayFrom(e.left, mappings)
      .concat([typeFrom(e.right, mappings)]);
  if(e.tag === E.RecordEmpty ||
    (e.tag === E.Record && Object.keys(e.map).length === 0))
    return [];
  E.serr('Invalid type syntax: ' + E.toString(e));
}

function typeFrom(e, mappings_) {
  var mappings = mappings_ || {};
  if(e.tag === E.Var) {
    if(/^[a-z][a-zA-Z0-9]*$/.test(e.name)) {
      if(!mappings[e.name]) mappings[e.name] = T.tvar(e.name);
      return mappings[e.name];
    }
    if(e.name === 'Rec') return T.TRec;
    if(e.name === 'Arr') return T.TArr;
    return T.tcon(e.name);
  }
  if(e.tag === E.Int)
    return T.tcon('' + e.val);
  if(e.tag === E.App)
    return T.tapp(typeFrom(e.left, mappings), typeFrom(e.right, mappings));
  if(e.tag === E.RecordEmpty)
    return T.trowempty;
  if(e.tag === E.Record) {
    var c = T.trowempty;
    for(var k in e.map) c = T.trowextend(k, typeFrom(e.map[k], mappings), c);
    return c;
  }
  E.serr('Invalid type syntax: ' + E.toString(e));
}

function labelFrom(x) {
  if(typeof x === 'string') return x;
  if(x.tag === E.Var) return x.name;
  if(x.tag === E.Int) return '' + x.val;
  if(x.tag === E.Float) return '' + x.val;
  if(x.tag === E.Str) return x.val;
  E.serr('Invalid label: ' + E.toString(x));
}

function getMap(e) {
  if(e.tag === E.RecordEmpty) return {};
  if(e.tag !== E.Record)
    E.serr('Handle can only take a record as argument');
  return e.map;
}

function objMap(o, f) {
  var n = {};
  for(var k in o) n[k] = f(o[k]);
  return n;
}

function handleType(a) {
  if(a.length !== 4
    || a[0].tag !== E.Var
    || a[1].tag !== E.List
    || a[2].tag !== E.Record) E.serr('Invalid type definition');
  var name = a[0].name;
  var mappings = {};
  var args = a[1].arr.map(x => typeFrom(x, mappings));
  var cases = objMap(a[2].map, x => x.arr.map(t => typeFrom(t, mappings)));
  var body = a[3];
  return E.type(name, args, cases, body);
}

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

function handleAppRR(r_) {
  var r = r_.map(v => Array.isArray(v)? handleAppRR(v): v);
  if(r.length === 2 && r[0].tag === E.Perform && r[0].label === 'Include') {
    var file = labelFrom(r[1]);
    var text = fs.readFileSync(file, {encoding: 'utf8'});
    var expr = parse(text);
    return expr;
  }
  if(r.length === 2 && r[0].tag === E.Perform && r[0].label === 'Read') {
    var file = labelFrom(r[1]);
    var text = fs.readFileSync(file, {encoding: 'utf8'});
    var expr = E.str(text);
    return expr;
  }
  if(r.length === 0)
    return E.recordempty();
  if(r.length === 1)
    return r[0];
  var first = r[0];
  if(first.tag === E.Var) {
    var name = first.name;
    if(name === 'fn')
      return E.lam.apply(
        null,
        r.slice(1).map((v, i, a) => i < a.length - 1? labelFrom(v): v)
      );
    if(name === 'ifn')
      return E.ilam.apply(
        null,
        r.slice(1).map((v, i, a) => i < a.length - 1? labelFrom(v): v)
      );
    if(name === 'let')
      return E.lete.apply(
        null,
        r.slice(1).map(
          (v, i, a) => i % 2 === 0 && i < a.length - 1? labelFrom(v): v
        )
      );
    if(name === 'letr')
      return E.letr.apply(
        null,
        r.slice(1).map(
          (v, i, a) => i % 2 === 0 && i < a.length - 1? labelFrom(v): v
        )
      );
    if(name === 'ilet')
      return E.ilet.apply(
        null,
        r.slice(1).map(
          (v, i, a) => i % 2 === 0 && i < a.length - 1? labelFrom(v): v
        )
      );
    if(name === 'iletr')
      return E.iletr.apply(
        null,
        r.slice(1).map(
          (v, i, a) => i % 2 === 0 && i < a.length - 1? labelFrom(v): v
        )
      );
    if(name === 'do')
      return E.doe.apply(
        null,
        r.slice(1).map(
          (v, i, a) => i % 2 === 0 && i < a.length - 1? labelFrom(v): v
        )
      );
    if(name === 'if')
      return E.iff.apply(null, r.slice(1));
    if(name === 'iapp')
      return E.iapp.apply(null, r.slice(1));

    if(name === 'type')
      return handleAppRR([handleType(
        r.slice(1, 5)
      )].concat(r.slice(6)));

    if(name === 'handle')
      return handleAppRR([
        E.handle(getMap(r[1]))
      ].concat(r.slice(2)));
    if(name === 'case')
      return handleAppRR([
        E.casee(getMap(r[1]))
      ].concat(r.slice(2)));

    if(name === 'anno')
      return handleAppRR([
        E.anno(r[1], typeFrom(r[2]))
      ].concat(r.slice(3)));

    if(name === 'sel')
      return handleAppRR([
        E.recordselect(labelFrom(r[1]))
      ].concat(r.slice(2)));
    if(name === 'extend')
      return handleAppRR([
        E.recordextend(labelFrom(r[1]))
      ].concat(r.slice(2)));
    if(name === 'restrict')
      return handleAppRR([
        E.recordrestrict(labelFrom(r[1]))
      ].concat(r.slice(2)));
    if(name === 'update')
      return handleAppRR([
        E.recordupdate(labelFrom(r[1]))
      ].concat(r.slice(2)));

    if(name === 'perform')
      return handleAppRR([
        E.perform(labelFrom(r[1]))
      ].concat(r.slice(2)));
  }
  return E.app.apply(null, r);
}

function lastIndexOfOp(r) {
  for(var i = r.length - 1; i >= 0; i--)
    if(r[i] instanceof OP) return i;
  return -1;
}

function handleAppR(r_) {
  var r = r_.map(x => Array.isArray(x)? handleAppR(x): x);
  var j = lastIndexOfOp(r);
  if(j < 0) return handleAppRR(r);
  var symbol = r[j].symbol;
  var expr = r[j].expr;
  var count = r[j].count;
  var noPartial = r[j].noPartial;
  var rev = r[j].rev;
  var label = r[j].label;
  if(count === 0) {
    r.splice(j, 1, expr);
    return handleAppR(r);
  } else if(count === 1) {
    if(r[j+1]) {
      r.splice(j, 2, [expr, r[j+1]]);
      return handleAppR(r);
    } else if(!noPartial) {
      if(label) E.serr('Operator lacks necessary argument: ' + symbol);
      r.splice(j, 1, expr);
      return handleAppR(r);
    }
  } else if(count === 2) {
    if(!r[j-1] && !r[j+1] && !noPartial) {
      if(label) E.serr('Operator lacks necessary argument: ' + symbol);
      if(rev) {
        r.splice(j, 1, [E.vr('flip'), expr]);
        return handleAppR(r);
      } else {
        r.splice(j, 1, [expr]);
        return handleAppR(r);
      }
    } else if(!r[j-1] && !noPartial) {
      if(rev) {
        r.splice(j, 2, [expr, r[j+1]]);
        return handleAppR(r);
      } else {
        if(label) E.serr('Operator lacks necessary argument: ' + symbol);
        r.splice(j, 2, [E.vr('flip'), expr, r[j+1]]);
        return handleAppR(r);
      }
    } else if(!r[j+1] && !noPartial) {
      if(rev) {
        if(label) E.serr('Operator lacks necessary argument: ' + symbol);
        r.splice(j-1, 2, [E.vr('flip'), expr, r[j-1]]);
        return handleAppR(r);
      } else {
        r.splice(j-1, 2, [expr, r[j-1]]);
        return handleAppR(r);
      }
    } else if(r[j-1] && r[j+1]) {
      if(rev) {
        r.splice(j-1, 3, [expr, r[j+1], r[j-1]]);
        return handleAppR(r);
      } else {
        r.splice(j-1, 3, [expr, r[j-1], r[j+1]]);
        return handleAppR(r);
      }
    }
  } else if(count === 3) {
    if(!r[j-1] && !r[j+1] && !r[j+2] && !noPartial) {
      if(label) E.serr('Operator lacks necessary argument: ' + symbol);
      if(rev) {
        r.splice(j, 1, [E.vr('flip'), expr]);
        return handleAppR(r);
      } else {
        r.splice(j, 1, [expr]);
        return handleAppR(r);
      }
    } else if(!r[j+1] && !r[j+2] && !noPartial) {
      if(rev) {
        if(label) E.serr('Operator lacks necessary argument: ' + symbol);
        r.splice(j-1, 2, [E.vr('flip'), expr, r[j-1]]);
        return handleAppR(r);
      } else {
        r.splice(j-1, 2, [expr, r[j-1]]);
        return handleAppR(r);
      }
    } else if(!r[j-1] && !r[j+2] && !noPartial) {
      if(rev) {
        r.splice(j, 2, [expr, r[j+1]]);
        return handleAppR(r);
      } else {
        if(label) E.serr('Operator lacks necessary argument: ' + symbol);
        r.splice(j, 2, [E.vr('flip'), expr, r[j+1]]);
        return handleAppR(r);
      }
    } else if(!r[j+2] && !noPartial) {
      if(rev) {
        r.splice(j-1, 3, [E.vr('flip'), [expr, r[j+1]], r[j-1]]);
        return handleAppR(r);
      } else {
        r.splice(j-1, 3, [expr, r[j-1], r[j+1]]);
        return handleAppR(r);
      }
    } else if(!r[j-1] && !noPartial) {
      if(rev) {
        r.splice(j, 3, [expr, r[j+1], r[j+2]]);
        return handleAppR(r);
      } else {
        if(label) E.serr('Operator lacks necessary argument: ' + symbol);
        r.splice(j, 3, [E.vr('flip'), [E.vr('flip'), expr, r[j+1]], r[j+2]]);
        return handleAppR(r);
      }
    } else if(r[j-1] && r[j+1] && r[j+2]) {
      if(rev) {
        r.splice(j-1, 4, [expr, r[j+1], r[j+2], r[j-1]]);
        return handleAppR(r);
      } else {
        r.splice(j-1, 4, [expr, r[j-1], r[j+1], r[j+2]]);
        return handleAppR(r);
      }
    }
  }
  E.serr('Invalid use of operator: ' + symbol);
}

function str(x) {
  if(Array.isArray(x)) return '[' + x.map(str).join(' ') + ']';
  return x === APP || x === PIPE || x instanceof OP? ''+x: E.toString(x);
}

function wrap(x) {return Array.isArray(x)? x: [x]}

function handlePipeAndApp(r) {
  var l = r.length;
  if(r[0] === APP || r[0] === PIPE || r[l-1] === APP || r[l-1] === PIPE)
    E.serr('Invalid use of ; or | or $ or ->');
  if(r.indexOf(PIPE) >= 0)
    return splitArr(r, PIPE).map(handlePipeAndApp).reduce((a, b) => b.concat([a]));
  if(r.indexOf(APP) >= 0)
    return splitArr(r, APP).map(handlePipeAndApp).reduceRight((a, b) => b.concat([a]));
  return r;
}

function handleApp(r) {
  return handleAppR(handlePipeAndApp(r));
}

function matchingBracket(c) {
  return (
    c === '('? ')':
    c === ')'? '(':
    c === '{'? '}':
    c === '}'? '{':
    c === '['? ']':
    c === ']'? '[':
    null
  );
}

var APP = {toString: () => '$'};
var PIPE = {toString: () => '|'};
function OP(symbol, expr, count, noPartial, rev, label) {
  this.symbol = symbol;
  this.expr = expr;
  this.count = count || 2;
  this.noPartial = noPartial || false;
  this.rev = rev || false;
  this.label = label || false;
}
function op(s, a, b, c, d, l) {return new OP(s, a, b, c, d, l)}

var EXPR = 0;
var NAME = 1;
var NUMBER = 2;
var STRING = 3;
var COMMENT = 4;
function rec(r) {
  if(r.length === 0) return E.recordempty();
  return E.reca(
    r.filter(x => x !== APP).map((v, i, a) => i % 2 === 0? labelFrom(v): v)
  )
}
function list(r) {
  return E.list(r);
}
function parse(s) {
  var state = EXPR, r = [], p = [], b = [], t = '', escape = false;
  for(var i = 0, l = s.length; i <= l; i++) {
    var c = s[i] || '\n';
    if(state === EXPR) {
      if(c === '"') state = STRING;

      else if(c === ';' && s[i+1] === ';') state = COMMENT, i++;

      else if(c === '|') r.push(PIPE);
      else if(c === ';') r.push(APP);
      else if(c === '$') r.push(APP);
      else if(c === '-' && s[i+1] === '>') r.push(APP), i++;

      else if(c === '<' && s[i+1] === '-')
        r.push(op('<-', E.vr('do'), 3, true)), i++;

      else if(c === '\\' && s[i+1] === '\\') r.push(E.vr('ifn')), i++;
      else if(c === '\\') r.push(E.vr('fn'));

      else if(c === '.' && s[i+1] === '+')
        r.push(op('.+', E.vr('extend'), 3, false, true, true)), i++;
      else if(c === '.' && s[i+1] === '-')
        r.push(op('.-', E.vr('restrict'), 2, false, true, true)), i++;
      else if(c === '.' && s[i+1] === '=')
        r.push(op('.=', E.vr('update'), 3, false, true, true)), i++;
      else if(c === '.')
        r.push(op('.', E.vr('sel'), 2, false, true, true));

      else if(c === ':')
        r.push(op(':', E.vr('anno'), 2, true));
      else if(c === '@')
        r.push(op('@', E.vr('iapp'), 2, true));
      else if(c === '!')
        r.push(op('!', E.vr('perform'), 1, true));

      else if(c === '+')
        r.push(op('+', E.vr('add'), 2));
      else if(c === '-' && !/[0-9]/.test(s[i+1]))
        r.push(op('-', E.vr('sub'), 2));
      else if(c === '*')
        r.push(op('*', E.vr('mul'), 2));
      else if(c === '/')
        r.push(op('/', E.vr('div'), 2));
      else if(c === '%')
        r.push(op('%', E.vr('rem'), 2));

      else if(c === '&' && s[i+1] === '&')
        r.push(op('&&', E.vr('and'), 2)), i++;
      else if(c === '|' && s[i+1] === '|')
        r.push(op('||', E.vr('or'), 2)), i++;

      else if(c === '=' && s[i+1] === '=')
        r.push(op('==', E.vr('eq'), 2)), i++;
      else if(c === '>' && s[i+1] === '=')
        r.push(op('>=', E.vr('geq'), 2)), i++;
      else if(c === '<' && s[i+1] === '=')
        r.push(op('<=', E.vr('leq'), 2)), i++;
      else if(c === '>') r.push(op('>', E.vr('gt'), 2));
      else if(c === '<' && s[i+1] === '>')
        r.push(op('<>', E.vr('append'), 2)), i++;
      else if(c === '<') r.push(op('<', E.vr('lt'), 2));

      else if(c === '=')
        r.push(op('let', E.vr('let'), 3, true, false));

      else if(c === '(' || c === '{' || c === '[') {
        b.push(c);
        p.push(r);
        r = [];
      } else if(c === ')' || c === '}' || c === ']') {
        if(b.length === 0)
          E.serr('Unmatched bracket: ' + c);
        var br = b.pop();
        if(matchingBracket(br) !== c)
          E.serr('Unmatched bracket: ' + br + ' and ' + c);
        var a = p.pop();
        a.push(
          c === '}'?
            rec(r):
          c === ']'?
            list(r):
            handleApp(r));
        r = a;
      }
      else if(/[a-z\_\$]/i.test(c)) state = NAME, t += c;
      else if(/[0-9\-]/.test(c)) state = NUMBER, t += c;
    } else if(state === NAME) {
      if(!/[a-z0-9\_\$]/i.test(c))
        r.push(E.vr(t)), t = '', i--, state = EXPR;
      else t += c;
    } else if(state === NUMBER) {
      if(!/[0-9\.\_]/.test(c)) {
        var n = +t.replace(/\_/g, '');
        if(isNaN(n)) E.serr('Invalid number: ' + t);
        r.push(
          t.indexOf('.') >= 0?
            E.float(n):
            E.int(n)
        ), t = '', i--, state = EXPR;
      } else t += c;
    } else if(state === STRING) {
      if(escape) t += c, escape = false;
      else if(c === '"') r.push(E.str(t)), t = '', state = EXPR;
      else if(c === '\\') escape = true;
      else t += c;
    } else if(state === COMMENT) {
      if(c === '\n') state = EXPR;
    }
  }
  if(state !== EXPR || b.length > 0)
    E.serr('Parsing failed');
  return handleApp(r);
}

module.exports = {
  parse,
};

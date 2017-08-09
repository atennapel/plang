import { createInterface } from 'readline';
import { readFileSync } from 'fs';
import parse from './parser';
import { Expr } from './exprs';
import { Result, Ok, Err } from './Result';
import InferState from './InferState';
import IdStore from './IdStore';
import Env from './Env';
import TVarSet from './TVarSet';
import {
	Type,
	tvar,
	tcon,
	tapp,
	tarrs,
	scheme,
	trowextend,
	trecord,
	tvariant,
	teff,
	trowempty,
	tnumber,
} from './types';
import {
	Kind,
	ktype,
	krow,
	karr,
} from './kinds';
import { id } from './Id';
import {
	cvalue
} from './constraints';

/**
 * TODO:
 * 	fix handle unification (#get (\_k k 10) $ !get ())
 */

function _inject(l: string) {return (v: any) => ({tag: l, val: v})}
function _select(l: string) {return (r: any) => r[l]}
function _extend(l: string) {return (v: any) => (r: any) => ({...r, [l]: v})}
function _restrict(l: string) {return (r: any) => {const x = {...r}; delete x[l]; return x}}
function _embed(l: string) {return (r: any) => r}
function _elim(l: string) {return (f: any) => (x: any) => (v: any) => v.tag === l? f(v.val): x(v)}
function _recordupdate(l: string) {return (f: any) => (r: any) => ({...r, [l]: f(r[l])})}
function _variantupdate(l: string) {return (f: any) => (v: any) => v.tag === l? ({tag: l, val: f(v.val)}): v}
function end() { throw new Error('impossible') }
function add(x: number) {return (y: number) => x + y}
const inc = add(1);
function choose(a: any) {return (b: any) => a}
var pure = function(x: any) {return x.val};
var _Return = 'Return';
var ret = function(x: any) {return {tag: _Return, val: x}};
var _Cont = 'Cont';
var _perform = function(label: any) {return function(v: any) {
  return {tag: _Cont, label: label, val: v, cont: ret};
}};
var _do = function(val: any) {return function(cb: any) {
  if(val.tag === _Cont)
    return {
      tag: _Cont,
      label: val.label,
      val: val.val,
      cont: (x: any) => _do(val.cont(x))(cb),
    };
  if(val.tag === _Return)
    return cb(val.val);
  throw new Error('Effect chain does not use Return');
}};
var _handle = function(label: any) {return function(fa: any) {
  return function(x: any): any {
    if(x.tag === _Cont) {
      if(x.label === label) {
        return _handle(label)(fa)(fa(x.val)(x.cont));
      } else {
        return {
          tag: _Cont,
          label: x.label,
          val: x.val,
          cont: (v: any) => _handle(label)(fa)(x.cont(v)),
        };
      }
    }
    if(x.tag === _Return) return x;
    throw new Error('Effect chain does not use Return');
  };
}};
var final = function(fa: any) {return function(x: any) {
  if(x.tag === _Cont)
    return {
      tag: _Cont,
      label: x.label,
      val: x.val,
      cont: (v: any) => final(fa)(x.cont(v)),
    };
  if(x.tag === _Return) return fa(x.val);
  throw new Error('Effect chain does not use Return');
}};

const a = id('a', 0);
const b = id('b', 0);
const r = id('r', 0);
const ta = tvar(a, ktype);
const tb = tvar(b, ktype);
const tr = tvar(r, krow);

const env = Env.of(
	['inc', tarrs(tnumber, tnumber).generalize()],
	['add', tarrs(tnumber, tnumber, tnumber).generalize()],
	
	['choose', scheme(TVarSet.of(ta), [], tarrs(ta, ta, ta))],
	
	['end', scheme(TVarSet.of(ta), [], tarrs(tapp(tvariant, trowempty), ta))],

	['pure', scheme(TVarSet.of(ta), [cvalue(ta)], tarrs(tapp(teff, trowempty, ta), ta))],
	['ret', scheme(TVarSet.of(ta, tr), [cvalue(ta)], tarrs(ta, tapp(teff, tr, ta)))],
	['final', scheme(TVarSet.of(ta, tb, tr), [cvalue(ta), cvalue(tb)], tarrs(tarrs(ta, tapp(teff, tr, tb)), tapp(teff, tr, ta), tapp(teff, tr, tb)))],	
);

const state = new InferState(new IdStore({ a: a.next(), b: b.next(), r: r.next() }));

function output(i: string) {
	const res = parse(i).then(e => {
		console.log(e.toString());
		return e.runInfer(env, state).map(([t, k]) => [e, t, k] as [Expr, Type, Kind])
	});
	if(res instanceof Ok) {
		const [e, t, k] = res.val;
		console.log(t.toString());
		// console.log(k.toString());
		const comp = e.compile()
		console.log(comp);
		try {
			console.log(eval(comp));
		} catch(e) {
			console.log(''+e);
		}
	} else {
		console.log(res.toString());
	}
}

const clinp = process.argv[2] || null;
if(clinp) {
	output(readFileSync(clinp, {encoding: 'utf8'}));
	process.exit();
} else {
	console.log('REPL');
	var repl = createInterface(process.stdin, process.stdout);
	process.stdin.setEncoding('utf8');
	const input = function input() {
		repl.question('> ', function(i) {
			output(i);
			setImmediate(input);
		});
	};
	input();
}

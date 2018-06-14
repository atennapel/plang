import { Result } from './Result';
import { impossible } from './util'; 

import { Type } from './types';
import { Context } from './context';

// errors
type IResult<T> = Result<TypeError, T>;
const err = <T>(msg: string): IResult<T> => Result.err(new TypeError(msg));
const ok = <T>(val: T): IResult<T> => Result.ok(val);

// find in context
const findVar = (ctx: Context, name: string): IResult<Type> => {
  const r = ctx.findVar(name);
  return r === null? err(`var ${name} not found in ${ctx}`): ok(r);
}
const findSolved = (ctx: Context, name: string): IResult<Type> => {
  const r = ctx.findSolved(name);
  return r === null? err(`var ${name} not found in ${ctx}`): ok(r);
}
const findTVar = (ctx: Context, name: string): IResult<null> => {
  return ctx.findTVar(name) === null? err(`tvar ${name} not found in ${ctx}`): ok(null);
}
const findEx = (ctx: Context, name: string): IResult<null> => {
  return ctx.findEx(name) === null? err(`ex ^${name} not found in ${ctx}`): ok(null);
}
const findMarker = (ctx: Context, name: string): IResult<null> => {
  return ctx.findMarker(name) === null? err(`marker |>${name} not found in ${ctx}`): ok(null);
}

// type wf
function typeWF() {

}

function contextWF() {
  
}

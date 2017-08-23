import { Result, Ok, Err} from './Result';
import { infer } from './typechecker';
import {
  Expr,
  eanno as O,
  eapp as A,
  evar as V,
  elam as L,
  eunit as U,
} from './exprs';
import { id as I } from './Id';
import {
  Type,
  tvar,
  tforall,
  tarr,
} from './types';

function run(e: Expr): Result<TypeError, Type> {
  console.log(''+e);
  return infer(e).map(({t, c}) => {console.log(`${t} in ${c}`); return t});
}
let total = 0;
let failed = 0;
function done() { console.log(`failed: ${failed}/${total}`) }
function ok(e: Expr): void {
  total++;
  const r = run(e);
  if(r instanceof Err) {
    console.log(`failed: ${r.val}`);
    failed++;
  }
  console.log();
}
function err(e: Expr): void {
  total++;
  const r = run(e);
  if(r instanceof Ok) {
    console.log(`failed: ${r.val}`);
    failed++;
  }
  console.log();
}

// TESTS
const x = I('x', -1);
const tx = tvar(x);
const id = L(['x'], V('x'));
ok(id);
ok(A(id, U));
ok(A(id, id));
ok(O(A(id, id), tforall([tx], tarr(tx, tx))));
// /TESTS

done();

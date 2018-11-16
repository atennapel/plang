import { infer } from "./inference";
import { initialContext } from "./initial";
import { abs, vr } from "./exprs";
import { name } from "./generic/NameRep";

const x = name('x');
console.log(
  '' + infer(initialContext, abs(x, vr(x))),
);

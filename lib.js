const True = true;
const False = false;
const if_ = c => a => b => c ? a : b;

const Z = 0;
const S = x => x + 1;
const caseNat = z => s => n => n > 0 ? s(n - 1) : z;
const iterNat = z => s => n => n > 0 ? s(iterNat(z)(s)(n - 1)) : z;
const recNat = z => s => n => n > 0 ? s(n - 1)(recNat(z)(s)(n - 1)) : z;

const Nil = [];
const Cons = h => t => [h].concat(t);
const caseList = n => c => l => l.length > 0 ? c(l[0])(l.slice(1)) : n;
const iterList = n => c => l => l.length > 0 ? c(l[0])(iterList(n)(c)(l.slice(1))) : n;
const recList = n => c => l => l.length > 0 ? c(l[0])(l.slice(1))(recList(n)(c)(l.slice(1))) : n;

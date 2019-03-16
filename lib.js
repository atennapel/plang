const True = true;
const False = false;
const if_ = c => a => b => c ? a : b;

const Z = 0;
const S = x => x + 1;
const caseNat = z => s => n => n > 0 ? s(n - 1) : z;
const iterNat = z => s => n => n > 0 ? s(iterNat(z)(s)(n - 1)) : z;
const recNat = z => s => n => n > 0 ? s(n - 1)(recNat(z)(s)(n - 1)) : z;


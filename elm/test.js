function Return(v) { this.val = v }
Return.prototype.toString = function() { return 'Return(' + this.val + ')' }
function Cont(l, v, c) { this.label = l; this.val = v; this.cont = c }
Cont.prototype.toString = function() { return 'Cont(' + this.label + ', ' + this.val + ', ...)' }

const ret = v => new Return(v);
const cont = (l, v, c) => new Cont(l, v, c);
const perform = l => v => cont(l, v, ret);
const pure = e => {
	if(e instanceof Return) return e.val;
	throw new Error('invalid pure: ' + e);
};

const seq = (e, f) => {
	if(e instanceof Return) return f(e.val);
	if(e instanceof Cont) return cont(e.label, e.val, (v, y) => seq(e.cont(v, y), f));
	throw new Error('invalid seq: ' + e);
};
const handler = m => e => {
	if(!m.value) m.value = ret;
	if(e instanceof Return) return m.value(e.val);
	if(e instanceof Cont) {
		if(m[e.label]) {
			return m[e.label](e.val, x => handler(m)(e.cont(null, x)));
		} else {
			return cont(e.label, e.val, x => handler(m)(e.cont(null, x)));
		}
	}
	throw new Error('invalid handler: ' + e);
};
const paramhandler = m => v => e => {
	if(!m.value) m.value = (a, b) => { console.log('val', a, b); return ret(b) };
	if(e instanceof Return) return m.value(v, e.val);
	if(e instanceof Cont) {
		if(m[e.label]) {
			return m[e.label](v, e.val, (v, x) => paramhandler(m)(v)(e.cont(v, x)));
		} else {
			return cont(e.label, e.val, (v, x) => paramhandler(m)(v)(e.cont(v, x)));
		}
	}
	throw new Error('invalid paramhandler: ' + e);
};

const get = perform('Get')();
const set = perform('Set');

const program =
	seq(get, (x, v) => {
		console.log('get', v, x);
	return seq(set(10), (_, __) =>
	seq(get, (y, _) =>
	ret(x + y)))});

// stateh : Eff { Get : {} -> v, Set : v -> {} | r } t -> Eff r (v -> Eff p t)
const stateh =
	handler({
		Get: (_, k) => ret(v => seq(k(v), f => f(v))),
		Set: (v, k) => ret(_ => seq(k(v), f => f(v))),
		value: v => ret(_ => ret(v))
	});
const state = v => e => pure(pure(stateh(e))(v));

const state2 =
	paramhandler({
		Get: (v, t, k) => {console.log(v,t,k);return k(v, v)},
		Set: (t, v, k) => {console.log(t, v, k);return k(v, null)}
	});

const r = state2(100)(program);
console.log(r);
console.log(''+r);

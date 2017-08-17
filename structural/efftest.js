function Return(v) { this.val = v }
Return.prototype.toString = function() {
	return 'Return(' + this.val + ')';
};
function Cont(l, v, c) {
	this.label = l;
	this.val = v;
	this.cont = c;
}
Cont.prototype.toString = function() {
	return 'Cont(' + this.label + ', ' + this.val + ', ...)';
};

const cont = (l, v, c) => new Cont(l, v, c);

// pure : Eff r v -> v
const pure = c => c.val;

// ret : v -> Eff r v
const ret = v => new Return(v);

// perform : v -> Eff { l : t -> v } v
const perform = l => v => cont(l, v, ret);

// seq : (Eff e1 a, a -> Eff e2 b) -> Eff e2 b
const seq = (e, f) => {
	if(e instanceof Return) return f(e.val);
	if(e instanceof Cont) return cont(e.label, e.val, (x, v) => seq(e.cont(x, v), f));
	throw new Error('invalid seq: ' + e);
}

const handle = m => v => e => {
	if(e instanceof Return) {
		if(!m.return) return e;
		return m.return(v, e.val);
	}
	if(e instanceof Cont) {
		if(m[e.label]) {
			return m[e.label](v, e.val, (x, v) => handle(m)(v)(e.cont(x, v)));
		} else {
			return cont(e.label, e.val, (x, v) => handle(m)(v)(e.cont(x, v)));
		}
	}
	throw new Error('invalid handle: ' + e);
}

const get = perform('Get')();
const set = perform('Set');

const state = handle({
	Get: (v, _, k) => k(v, v),
	Set: (_, v, k) => k(null, v),
});

const program =
	seq(get, x =>
	seq(set(100), _ =>
	seq(get, y =>
	ret(x / y))));

const e = state(10)(program);
console.log(e);
console.log(''+e);

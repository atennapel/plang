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

// handle : (a -> (b -> Eff r t) -> Eff r t) -> Eff { l : a -> b | r } t -> Eff r t 
const handle = l => f => c => {
	//console.log(l, ''+c);
	if(c instanceof Return) return c;
	if(c instanceof Cont) {
		if(c.label === l) {
			return handle(l)(f)(f(c.val)(x => handle(l)(f)(c.cont(x))));
		} else {
			return cont(c.label, c.val, x => handle(l)(f)(c.cont(x)));
		}
	}
	throw new Error('Invalid value in handle: ' + c);
};

// seq : Eff r a -> (a -> Eff r b) -> Eff r b
const seq = (c, f) => {
	if(c instanceof Return) return f(c.val);
	if(c instanceof Cont) return cont(c.label, c.val, x => seq(c.cont(x), f));
	throw new Error('Invalid value in seq: ' + c);
};

// final : (a -> Eff r b) -> Eff r a -> Eff r b
const final = f => c => seq(c, f);

const get = perform('Get')();
const set = perform('Set');
function compose() {
	const fs = Array.prototype.slice.call(arguments);
	return x => { let v = x; for(let i = fs.length - 1; i >= 0; i--) v = fs[i](v); return v };
}
const state = v => compose(
	handle('Get')(_ => k => k(v)),
	handle('Set')(v => k => state(v)(k()))
);
const state2 = compose(
	handle('Get')(_ => k => ret(v => seq(k(v), f => f(v)))),
	handle('Set')(v => k => ret(_ => seq(k(), f => f(v)))),
	final(v => ret(_ => ret(v)))
);

const flip = perform('Flip')();

const alwaysTrue = handle('Flip')(_ => k => k(true));
const alwaysFalse = handle('Flip')(_ => k => k(false));
const random = handle('Flip')(_ => k => k(Math.random() > 0.5));
const all = compose(
	handle('Flip')(_ => k => seq(k(true), a => seq(k(false), b => ret(a.concat(b))))),
	final(v => ret([v]))
);

const program =
	seq(get, x =>
	seq(set(10), _ =>
	seq(get, y =>
	ret(x + y))));

const result = pure(state2(program))(1);
console.log(result);
console.log(''+result);

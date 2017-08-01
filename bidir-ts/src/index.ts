enum Tag { A, B, C }

interface O { tag: Tag }
interface A extends O { tag: Tag.A, val: number }
interface B extends O { tag: Tag.B, val: string }
interface C extends O { tag: Tag.C }

function a(val: number): A { return { tag: Tag.A, val } };
function isA(x: O): x is A { return x.tag === Tag.A };
function b(val: string): B { return { tag: Tag.B, val } };
function isB(x: O): x is B { return x.tag === Tag.B };
const c: C = { tag: Tag.C };
function isC(x: O): x is C { return x.tag === Tag.C };

const myA = a(4);
const myB = b('asd');

function test(x: O): string {
	if(isA(x)) {
		return '' + x.val;
	}
	if(isC(x)) {
		return 'test';
	}
	if(isB(x)) {
		return x.val;
	}
	return '';
}

console.log(4);

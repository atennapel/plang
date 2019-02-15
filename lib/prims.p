Prim.void :: Prim.Void -> t
Prim.void ::= "() => { throw new Error('void') }"

Prim.unit :: Prim.Unit
Prim.unit ::= "{_tag: 'Unit', val: null}"

Prim.true :: Prim.Bool
Prim.true ::= "true"

Prim.false :: Prim.Bool
Prim.false ::= "false"

Prim.if :: Prim.Bool -> (Prim.Unit -> t) -> (Prim.Unit -> t) -> t
Prim.if ::= "c => a => b => c ? a(Prim_unit) : b(Prim_unit)"

Prim.pair :: a -> b -> Prim.Pair a b
Prim.pair ::= "a => b => ({ _tag: 'Pair', val: [a, b] })"

Prim.fst :: Prim.pair a b -> a
Prim.fst ::= "p => p.val[0]"

Prim.snd :: Prim.pair a b -> b
Prim.fst ::= "p => p.val[1]"

Prim.inl :: a -> Prim.Sum a b
Prim.inl ::= "a => ({ _tag: 'L', val: a })"

Prim.inr :: b -> Prim.Sum a b
Prim.inr ::= "b => ({ _tag: 'R', val: b })"

Prim.case :: (a -> r) -> (b -> r) -> Prim.Sum a b -> r
Prim.case ::= "a => b => s => s._tag === 'L' ? a(s.val) : b(s.val)"

Prim.appendStr :: Prim.Str -> Prim.Str -> Prim.Str
Prim.appendStr ::= "a => b => a + b"

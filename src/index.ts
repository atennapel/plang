/**
 * TODO:
 *  if expressions
 * 
 *  parser:
 *    records as tuples
 *    syntax for lists and arrays
 *    syntax for adt match
 *    operators
 *  typechecker:
 *    lazy types
 *    improve type errors (pretty print)
 *    higher ranked types in records and variants
 *    constrained row polymorphism
 *    tfun as a type constructor
 *    type alias
 *    implicits
 *    fix positivity check
 *    functor generation
 *    ADT codata?
 *    generate apa
 *    kind inference
 *    kind polymorphism
 *    typed holes ?x or _x
 *    ignored arguments _
 *  repl:
 *    tutorial command
 *    save/load/clear commands
 *  other:
 *    pretty printer expr
 *    simplify names in pp of types
 * 
 * PROBLEMS:
 *  force (force 4.)
 *  :def f = /\(x x:Type) -> \x -> x
 *  fresh vars in WF
 *  { x = \x -> x } : SRec { x : forall t. t -> t }
 *  caseBool (#Just @(forall t. t -> t) (\x -> x)) (#Nothing ()) True : forall (r : Row). SVar { Just : forall t. t -> t, Nothing : Unit | r }
 * 
 * MAYBE PROBLEMS:
 *  vupdX S (injY 0)
 *  \f r -> csX r (\n -> injX $ f n) (\r -> r)
 *  handler Read (\() k -> opSet ())
 *  \v -> csX v (\(v:Nat) -> injX v) id
 */

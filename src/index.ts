/**
 * TODO:
 *  parser:
 *    syntax for algebraic effects
 *    syntax for record selection, restriction and update
 *    syntax for variants
 *    syntax for lists and arrays
 *    operators
 *  typechecker:
 *    higher ranked types in records
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
 *    let with arguments
 *  other:
 *    pretty printer expr
 * 
 * PROBLEMS:
 *  { x = \x -> x } : SRec { x : forall t. t -> t }
 * 
 * MAYBE PROBLEMS:
 *  vupdX S (injY 0)
 *  \f r -> csX r (\n -> injX $ f n) (\r -> r)
 *  handler Read (\() k -> opSet ())
 *  \v -> csX v (\(v:Nat) -> injX v) id
 */

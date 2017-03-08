type Future [r a] {
  Future [(Arr (Arr a r) r)]
};

let andThen (\f k -> case {Future (\f -> f k)} f);

let futureOf (\x -> Future (\k -> k x));

let futureBind (\rest ktor -> Future (\k -> andThen ktor (\a -> andThen (rest a) k)));

let futureMap (\f -> futureBind (\x -> futureOf (f x)));

{
  Future Future
  andThen andThen
  futureOf futureOf
  futureBind futureBind
  futureMap futureMap
}

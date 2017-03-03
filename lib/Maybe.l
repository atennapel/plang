type Maybe [t] {
  Nothing []
  Just [t]
};

let withDefault (\v -> case {
  Nothing (\_ -> v)
  Just (\v -> v)
});

let maybe (handle {
  Fail (\_ k -> ret Nothing)
  return (\x -> ret (Just x))
});

let fail (!Fail ());

{
  Nothing Nothing
  Just Just

  withDefault withDefault

  maybe maybe
  fail fail
}

type Result [e t] {
  Err [e]
  Ok [t]
};

let result (handle {
  Error (\v k -> ret (Err v))
  return (\x -> ret (Ok x))
});

let error (!Error);

{
  Err Err
  Ok Ok

  result result
  error error
}

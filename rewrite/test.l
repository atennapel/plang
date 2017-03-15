pure (handle Get (fn v (fn k (k one))) (
  handle Set (fn v (fn k (k ()))) (
    (do x (perform Get ()) (do y (perform Set ()) (return (add one x))))
  )
))

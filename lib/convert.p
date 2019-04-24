type Convert a b = a -> b
let convert (Convert f) = f

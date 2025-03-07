let x = 10;;

let x = 1
and y = x + 2 in
x + y

let z = 10
let f x y = x * y * z
let h x = x

let f x =
  let g y z = x * y * z in
  g (h x) z
;;



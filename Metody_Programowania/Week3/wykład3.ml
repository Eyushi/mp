

let rec append xs ys =
  match xs with
  | [] -> ys
  | x:: xs' -> x :: append xs' ys
  ;;






  (* ver 2 -- bo xs nie jest używany -- odwzorowanief*)
  let rec map f = function
  | [] -> []
  | x :: xs' -> f x :: map f xs'
  ;;



  



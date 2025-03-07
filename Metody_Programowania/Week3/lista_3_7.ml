
type int_tree =
  | Leaf
  | Node of int_tree * int * int_tree



let rec flat_append t xs = 
  match t with
  | Leaf -> xs
  | Node (l, v, r) ->
    if l = Leaf && r = Leaf then v::xs
    else if l <> Leaf && r = Leaf then flat_append l (v::xs)
    else if l == Leaf && r <> Leaf then flat_append (v :: flat_append r xs)
    else flat_append l (v:: flat_append r xs)
    ;;




let rec flat_append2 t xs =
  match t with
  | Leaf -> xs
  |Node (l,v,r) -> flat_append l (v:: flat_append r xs)



  let flatten = flat_append t []
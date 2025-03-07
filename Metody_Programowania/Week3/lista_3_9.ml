
type int_tree =
  | Leaf
  | Node of int_tree * int * int_tree


let rec insert_bst x t =
  match t with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node (l, v, r) ->
    if v > x then Leaf (insert_bst x l, v, r)
    else  Leaf (l, v, insert_bst x r)
    ;;

(* Z WYKŁADU *)
(* type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree *)

type int_tree =
  | LeafI
  | NodeI of int_tree * int * int_tree

let rec fold_tree f a t =
  match t with
  | LeafI -> a
  | NodeI (l, v, r) -> f (fold_tree f a l) v (fold_tree f a r)


let tree_product t = fold_tree (fun l v r -> l * v * r) 1 t


let tree_flip t = fold_tree (fun l v r -> NodeI (r, v, l)) LeafI t
(* funkcje pom *)
let max2 a b = if a > b then a else b
let max3 a b c = if a > b && a > c then a else if b > c then b else c

(* wysokość *)
let tree_height t = fold_tree (fun l v r -> (max2 l r) + 1) 0 t


(* funkcje pom *)
let val_of_tree t =
  match t with
  | LeafI -> 0 (*Nieużywane*)
  | NodeI (_, v, _) -> v;;

let tree_span t =
  let min_tree = fold_tree (fun l v r -> if l = LeafI then NodeI(l,v,r) else l) LeafI t
  and max_tree = fold_tree (fun l v r -> if r = LeafI then NodeI(l,v,r) else r) LeafI t in
  (val_of_tree(min_tree), val_of_tree(max_tree));;





let preorder t = fold_tree (fun l v r -> v :: l @ r) [] t;;



(* przykłady *)

let t =
  NodeI (NodeI (LeafI, 2, LeafI), 5, NodeI (NodeI (LeafI, 6, LeafI), 8, NodeI (LeafI, 9, LeafI)))


let t2 = NodeI (NodeI (LeafI, 2, LeafI), 5, LeafI);;




(* ver 2 bst ~źle*)

let bst_span t =
  let max_node _ v r = max v r in
  let min_node _ v r = min v r in
  ((fold_tree max_node min_int t), (fold_tree min_node max_int t))



(* ver wykładowcy *)



let span t = 
  let f l v r = match l, r with
  |None, None -> Some(v,v)
  | None, Some (_, rv) -> Some (v, rv)
  | Some (lv, _), None -> Some (lv, v)
  | Some(lv, _), Some(_, pv) ->Some(lv, pv)
in fold_tree f None t
;;
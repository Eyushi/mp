(* Zadanie 5. *)

type int_tree =
  | LeafI
  | NodeI of int_tree * int * int_tree

let t =
  NodeI
    ( NodeI (LeafI, 2, LeafI)
    , 5
    , NodeI (NodeI (LeafI, 6, LeafI), 8, NodeI (LeafI, 9, LeafI)) )
;;

let rec insert_bst t a =
  match t with
  | LeafI -> NodeI (LeafI, a, LeafI)
  | NodeI (l, v, r) ->
    if a < v
    then NodeI (insert_bst l a, v, r)
    else if a > v
    then NodeI (l, v, insert_bst r a)
    else NodeI (l, v, r)
;;
(* przypadek kiedy już jest - bez powtórzeń *)

(* insert_bst t 7;;
   - : int_tree =
     NodeI (NodeI (LeafI, 2, LeafI), 5,
     NodeI (NodeI (LeafI, 6, NodeI (LeafI, 7, LeafI)), 8, NodeI (LeafI, 9, LeafI))) *)

(* wszystkie oprócz tego przed 7 i samej 7?? *)
(* lewe poddrzewo współdzielone oraz 9 też*)
(* skopiowana cała ścieżka *)
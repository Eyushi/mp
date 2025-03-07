module LeftistHeap = struct
  (* Z POLECENIA: *)
  type ('a, 'b) heap =
    | HLeaf
    | HNode of int * ('a, 'b) heap * 'a * 'b * ('a, 'b) heap

  (* ranga wierzchołka, lewe poddrzewo, priorytet elementu, element, prawe poddrzewo.*)
  let rank = function
    | HLeaf -> 0
    | HNode (n, _, _, _, _) -> n
  ;;

  (* ranga lewego poddrzewa jest nie mniejsza niż ranga prawego poddrzewa. *)
  let heap_ordered p = function
    | HLeaf -> true
    | HNode (_, _, p', _, _) -> p <= p'
  ;;

  let rec is_valid = function
    | HLeaf -> true
    | HNode (n, l, p, v, r) ->
      rank r <= rank l
      && rank r + 1 = n
      && heap_ordered p l
      && heap_ordered p r
      && is_valid l
      && is_valid r
  ;;

(* ###############################################################3   *)
  (* ROZWIĄZANIE: *)

  let make_node p v l r =
    if rank l >= rank r
      (* bo rank jest liczony z prawego poddrzewa + 1 *)
    then HNode (rank r + 1, l, p, v, r)
    else HNode (rank l + 1, r, p, v, l)
  ;;

  let rec heap_merge h1 h2 =
    match h1, h2 with
    | HLeaf, h2 -> h2
    | h1, HLeaf -> h1
    | HNode (ranga1, l1, p1, v1, r1), HNode (ranga2, l2, p2, v2, r2) ->
      (* mniejszy równy! wpp stack overflow*)
      if p1 <= p2
      then make_node p1 v1 l1 (heap_merge r1 h2)
      else make_node p2 v2 l2 (heap_merge r2 h1)
  ;;
end
(* ###############################################################3   *)

open LeftistHeap

(* przykłady *)

let heap1 = HNode (1, HNode (1, HLeaf, 52, "el2", HLeaf), 8, "el1", HLeaf)
let heap2 = HNode (1, HNode (1, HLeaf, 60, "el2", HLeaf), 11, "el1", HLeaf)

let heap3 = heap_merge heap1 heap2
let heap4 =
  HNode
    ( 2
    , HNode (1, HLeaf, 52, "el2", HLeaf)
    , 8
    , "el1"
    , HNode (1, HNode (1, HLeaf, 60, "el2", HLeaf), 11, "el1", HLeaf) )
;;

let heap5 = heap_merge heap1 heap4

(* sygnatura Z WYKŁADU *)
module type PRIO_QUEUE = sig
  type ('a, 'b) pq

  val empty : ('a, 'b) pq
  val insert : 'a -> 'b -> ('a, 'b) pq -> ('a, 'b) pq
  val pop : ('a, 'b) pq -> ('a, 'b) pq
  val min_with_prio : ('a, 'b) pq -> 'a * 'b
end

module ListPrioQueue : PRIO_QUEUE = struct
  type ('a, 'b) pq = ('a * 'b) list

  let empty = []

  let rec insert a x q =
    match q with
    | [] -> [ a, x ]
    | (b, y) :: ys -> if a < b then (a, x) :: q else (b, y) :: insert a x ys
  ;;

  let pop q = List.tl q
  let min_with_prio q = List.hd q
end

(* Z POPRZEDNIEGO ZADANIA: kopce lewicowe *)
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

  let make_node p v l r =
    if rank l >= rank r (* bo rank jest liczony z prawego poddrzewa + 1 *)
    then HNode (rank r + 1, l, p, v, r)
    else HNode (rank l + 1, r, p, v, l)
  ;;

  let rec heap_merge h1 h2 =
    match h1, h2 with
    | HLeaf, h2 -> h2
    | h1, HLeaf -> h1
    | HNode (rank1, l1, p1, v1, r1), HNode (rank2, l2, p2, v2, r2) ->
      (* mniejszy równy! wpp stack overflow*)
      if p1 <= p2
      then make_node p1 v1 l1 (heap_merge r1 h2)
      else make_node p2 v2 l2 (heap_merge r2 h1)
  ;;
end

(* ##########################################################3 *)
(*  ROZWIĄZANIE *)

(* implemantacja sygnatury PRIO QUEUE *)
module LeftistHeapPQ : PRIO_QUEUE = struct
  open LeftistHeap

  type ('a, 'b) pq = ('a, 'b) heap

  let empty = HLeaf

  (* priority, value, heap *)
  let insert p v h = heap_merge h (HNode (1, HLeaf, p, v, HLeaf))

  (*pierwsza - korzeń*)
  let pop h =
    match h with
    | HLeaf -> failwith "pop on empty leftist heap pq"
    | HNode (_, l, _, _, r) -> heap_merge l r
  ;;

  let min_with_prio h =
    match h with
    | HLeaf -> failwith "min_with_pirio on empty leftist heap pq"
    | HNode (_, _, p, v, _) -> p, v
  ;;
end

module PQSort (PQ : PRIO_QUEUE) = struct
  open PQ

  let rec new_queue xs pq =
    match xs with
    | [] -> pq
    | (prio, v) :: xs' -> new_queue xs' (insert prio v pq)
  ;;

  let rec new_list pq new_xs =
    if pq = empty then new_xs
    else new_list (pop pq) (min_with_prio pq :: new_xs)
  ;;

  let pqsort xs = List.rev (new_list (new_queue xs empty) [])
end

(* przykłady *)
module PQSortList = PQSort (ListPrioQueue)
module PQSortLeftistHeap = PQSort (LeftistHeapPQ)

let ex = [ 5, "e"; 3, "c"; 7, "g"; 1, "a"; 4, "d"; 6, "f"; 2, "b" ]
let list_ex = PQSortList.pqsort ex
let ex = [ 5, "e"; 3, "c"; 7, "g"; 1, "a"; 4, "d"; 6, "f"; 2, "b" ]
let heap_ex = PQSortLeftistHeap.pqsort ex

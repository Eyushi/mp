(* Z WYKŁADU *)

module type DICT = sig
  type ('a, 'b) dict

  val empty : ('a, 'b) dict
  val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
  val remove : 'a -> ('a, 'b) dict -> ('a, 'b) dict
  val find_opt : 'a -> ('a, 'b) dict -> 'b option
  val find : 'a -> ('a, 'b) dict -> 'b
  val to_list : ('a, 'b) dict -> ('a * 'b) list
end

module type PRIO_QUEUE = sig
  type ('a, 'b) pq

  val empty : ('a, 'b) pq
  val insert : 'a -> 'b -> ('a, 'b) pq -> ('a, 'b) pq
  val pop : ('a, 'b) pq -> ('a, 'b) pq
  val min_with_prio : ('a, 'b) pq -> 'a * 'b
end

(* ##########################################################3 *)
(* Z POLECENIA *)
module type HUFFMAN = sig
  type 'a code_tree
  type 'a code_dict

  val code_tree : 'a list -> 'a code_tree
  val dict_of_code_tree : 'a code_tree -> 'a code_dict
  val encode : 'a list -> 'a code_dict -> int list
  val decode : int list -> 'a code_tree -> 'a list
end

(* ROZWIĄZANIE *)
(* dodać : HUFFMAN  *)

(* Dodana linijka *)
module Huffman (Dict : DICT) (PrioQueue : PRIO_QUEUE) = struct

  (* korzystanie z modułów bez konieczności odnoszenia się do nich za każdym razem
     np Dict.insert *)
  open Dict
  open PrioQueue



  (* z wykładu *)
  type 'a code_tree =
    | CTNode of 'a code_tree * 'a code_tree
    | CTLeaf of 'a
(* wykład - w  module ListDict : DICT*)
  type 'a code_dict ('a, 'b) dict = ('a *. 'b) list;;

end

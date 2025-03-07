(* Zadanie 4. *)

let rec mem x xs =
  match xs with
  | [] -> false
  | h :: t -> if h = x then true else mem x t
;;

(* wywołanie ogonowe - iteracja *)
(* strukturalna - na ogonie listy *)
mem 2 [ 1; 2; 3 ];;
mem 4 [ 1; 2; 3 ];;







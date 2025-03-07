(* Zadanie 6. *)
(* DONE *)
let rec is_sorted xs =
  match xs with
  | [] -> true
  | h :: [] -> true
  | h :: t -> if h <= List.hd t then is_sorted t else false
;;

is_sorted [ 1; 2; 30; 5; 100 ];;

is_sorted [ 1; 2; 3; 5; 100 ];;

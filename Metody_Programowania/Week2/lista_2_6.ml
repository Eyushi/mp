(* Zadanie 6. *)

let rec suffixes xs =
  match xs with
  | [] -> [ [] ]
  | h :: t -> (h :: t) :: suffixes t
;;


(* NIE wywołanie ogonowe - iteracja ???????????????? *)
(* strukturalna - na ogonie listy *)

suffixes [ 2 ];;


suffixes [ 1; 2; 3; 4 ];;


suffixes []

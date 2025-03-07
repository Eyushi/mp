(* Zadanie 9. (2 pkt) *)




(* długość listy *)
let rec len l =
  match l with
  | [] -> 0
  | a :: b -> 1 + len b;;



let rec split xs =
  match xs with
  | [] -> [], []
  | a :: [] -> a, []
  | a :: b :: t ->





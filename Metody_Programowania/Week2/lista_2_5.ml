(* Zadanie 5. *)
(* wywołanie ogonowe - iteracja *)
(* strukturalna - na ogonie listy *)


let rec maximum xs =
  let rec help xs' max =
    match xs' with
    | [] -> max
    | h :: t -> if h > max then help t h else help t max
  in
  help xs neg_infinity
;;

maximum [ 1.; 5.; 0.; 7.; 1.; 4.; 1.; 0. ];;
maximum [];;

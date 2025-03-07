(* zadanie 1 *)

let fold_left f a xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (f acc x)
  in
  it xs a
  ;;


let new_sum xs = fold_left ( + ) 0 xs;;

(* spacje, bo inaczej traktuje jako niedokończony komentarz *)
let product xs = fold_left ( * ) 1 xs;;

product [ 1; 2; 3; 2 ];;
product [ 1; 2; 0 ];;
product [];;

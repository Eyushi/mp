let rec insert_asc n xs =
  match xs with
  | [] -> [ n ]
  | x :: xs' -> if n < x then n :: xs else x :: insert_asc n xs'
;;

let rec insert_desc n xs =
  match xs with
  | [] -> [ n ]
  | x :: xs' -> if n > x then n :: xs else x :: insert_asc n xs'
;;

let rec insertion_sort xs =
  match xs with
  | [] -> []
  | x :: xs' -> insert_asc x (insertion_sort xs')
;;

(* Wersja dla obu razem*)

let rec insert_gen lt n xs =
  match xs with
  | [] -> [ n ]
  | x :: xs' -> if lt n x then n :: xs else x :: insert_gen lt n xs'
;;

let rec insertion_sort_gen lt xs =
  match xs with
  | [] -> []
  | x :: xs' -> insert_gen lt x (insertion_sort_gen lt xs')
;;

insertion_sort_gen ( < ) [ 1, 2, 3, 5, 2, 2, 803, 3 ];;

(* - : (int * int * int * int * int * int * int * int) list =
     [(1, 2, 3, 5, 2, 2, 803, 3)] *)

insertion_sort_gen ( < ) [ (1, 2), (3, 1), (1, 3) ]

(* - : ((int * int) * (int * int) * (int * int)) list = [((1, 2), (3, 1), (1, 3))] *)

let pair_lex (a1, a2) (b1, b2) =
  if a1 < b1 then true else if b1 < a1 then false else a2 < b2
;;

let pair_lex_gen lt1 lt2 (a1, a2) (b1, b2) =
  if lt1 a1 b1 then true else if lt1 b1 a1 then false else lt1 a2 b2
;;

(* lt1 typ: 'a -> 'a -> bool, bo jest dwa razy na różnych pozycjach *)

pair_lex_gen ( < ) ( > ) (1, 3) (0, 3);;
insertion_sort_gen ( > ) [ (1, 2), (3, 1), (1, 3) ]

(* ################################### *)

let add1 x = x + 1

let rec add1_to_all xs =
  match xs with
  | [] -> []
  | x :: xs' -> add1 x :: add1_to_all xs'
;;

(* ver 2 -- bo xs nie jest używany*)
let rec add1_to_all' = function
  | [] -> []
  | x :: xs' -> add1 x :: add1_to_all' xs'
;;

(* ver 2 -- bo xs nie jest używany*)
let rec string_list_of_int_list = function
  | [] -> []
  | x :: xs' -> string_of_int x :: string_list_of_int_list xs'
;;

(* bardziej ogólnie *)

(* ver 2 -- bo xs nie jest używany -- odwzorowanief*)

(* ver 2 -- bo xs nie jest używany -- odwzorowanief*)
let rec map f = function
  | [] -> []
  | x :: xs' -> f x :: map f xs'
;;

map (map string_of_int) [ [ 2 ]; [ 3; 2; 3 ]; [ 2321; 2; 00; 2 ]; [ 0 ]; [] ]

(* [["2"]; ["3"; "2"; "3"]; ["2321"; "2"; "0"; "2"]; ["0"]; []] *)

let rec only_positive = function
  | [] -> []
  | x :: xs -> if x > 0 then x :: only_positive xs else only_positive xs
;;

only_positive [ 2; -3; 6; 0; -34; 0; 2 ]

(* - : int list = [2; 6; 2] *)

let rec only_nonempty = function
  | [] -> []
  | x :: xs -> if x <> [] then x :: only_nonempty xs else only_nonempty xs
;;

(* generycznie *)

let rec filter p = function
  | [] -> []
  | x :: xs -> if p x then x :: filter p xs else filter p xs
;;

(* generycznie *)
let rec fold_right f xs a =
  match xs with
  | [] -> a
  | x :: xs' -> f x (fold_right f xs' a)
;;

(* val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)
(* dla kolejnych elementów listy wywołuje f *)

let rec new_append xs ys = fold_right (fun x a -> x :: a) xs ys
let rec new_append xs ys = fold_right List.cons xs ys

(* to samo *)

let new_map f xs = fold_right (fun y ys -> f y :: ys) xs []
let new_filter p xs = fold_right (fun y ys -> if p y then y :: ys else ys) xs [];;

new_filter (fun x -> x > 0) [ 4; 0; 2; -4 ]

(* - : int list = [4; 2] *)

(* ################################################ *)

(* drugi wzorzec na listach -- iteracja z akumulatorem *)

let length xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (acc + 1)
  in
  it xs 0
;;

let sum xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (acc + x)
  in
  it xs 0
;;

let rev xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (x :: acc)
  in
  it xs []
;;

(* WZORZEC *)
let fold_left f a xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (f acc x)
  in
  it xs a
;;
(* funkcje iteracyjne *)
let new_lenght xs = fold_left (fun acc y -> acc + 1) 0 xs;;
let new_sum xs = fold_left (fun acc y -> acc + y) 0 xs;;

let new_sum2 xs = fold_left (+) 0 xs;;

let new_rev xs = fold_left (fun acc y -> y :: acc) [] xs;;




fold_left;;
(* iteracyjnie *)
(* - : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)

fold_right;;
(* ogólna rekursja *)
(* - : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun> *)

(* używanie zamiennie *)

(* let new_sum_worse xs = fold_right (+) xs 0;; *)
(* todo *)
(* why *)

let new_map_reversing f xs = fold_right (fun ys y -> f y :: ys) [] xs;;

let new_map' f xs = rev (new_map_reversing f xs);;

(* bez nawiasów *)

let new_map' f xs = new_map_reversing f xs |> rev;;



(|>);;
(* - : 'a -> ('a -> 'b) -> 'b = <fun> *)

(* fold_right f [x1, x2; x3] a ->)
   fold right f x1 (fold_right f [x2; x3] a)
   fold right f x1 (f x2 (fold_right f [x3] a))
   fold right f x1 (f x2 (f x3 (fold_right f [] a)))
fold _right (#) [x1;x2;x3] *)

   
(* fold left -- skopiować*)

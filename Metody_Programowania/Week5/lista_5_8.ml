(* Zadanie 8.
   Zdefiniuj predykat is_sorted : int list -> bool sprawdzający czy lista jest po-
   sortowana oraz funkcję insert : int -> int list -> int list wstawiającą ele-
   ment do listy posortowanej. Następnie udowodnij, że jeśli is_sorted xs ≡ true
   to is_sorted (insert x xs) ≡ true. *)

(* rosnąco, od min do max *)
let rec is_sorted xs =
  match xs with
  | [] -> true
  | x :: [] -> true
  | x :: x2 :: xs -> if x > x2 then false else is_sorted (x2 :: xs)
;;

let rec insert x xs =
  match xs with
  | [] -> [ x ]
  | x1 :: [] -> if x < x1 then [ x; x1 ] else [ x1; x ]
  | x1 :: x2 :: xs' ->
    if x < x1
    then x :: x1 :: xs'
    else if x < x2
    then x1 :: x :: x2 :: xs'
    else insert x (x2 :: xs')
;;



(* #################################### *)
(* ver 2 *)


let rec is_sorted = function
| [] | [_] -> true
|x:: y :: rest -> x <= y && is_sorted rest


let rec insert x = function
| [] -> [x]
| y :: rest -> if x <= y then x ::y :: rest else y:: insert x rest




















(* LISTY *)


[1;2];;
(* - : int list = [1; 2] *)

[1,2];;
(* - : (int * int) list = [(1, 2)] *)

(* Listy tego samego typu ! *)

(* [1; 2.];; *)
(* Error: This expression has type float but an expression was expected of type
         int *)






[Some 1; None];;



(* Może być lista funkcji *)


[ (+); (-)];;
(* - : (int -> int -> int) list = [<fun>; <fun>] *)

[fst; snd];;
(* - : ('a * 'a -> 'a) list = [<fun>; <fun>] *)
(* fst i snd są w tej samej liście więc muszą mieć te same typy
   (bardziej ograniczone niż jak było wcześniej) *)

fst;;
(* - : 'a * 'b -> 'a = <fun> *)

snd;;
(* - : 'a * 'b -> 'b = <fun> *)

(* KONSTRUKTORY *)

[];;
(* - : 'a list = [] *)
(* pusta lista *)

(* Tak jak listy jednokierunkowe *)

List.cons 1 (List.cons 2 []);;
(* - : int list = [1; 2] *)


List.cons;;
(* - : 'a -> 'a list -> 'a list = <fun> *)

(* ten sam konstruktor co cons: *)
1 :: 2 :: [];;
(* - : int list = [1; 2] *)
(* łączy w prawo *)


(* kończy się listą pustą, inaczej byłaby niesk długa *)


(* lista zawinięta, nieskończona:, ale w pamięci jest skończona ilość miejsca *)
let rec linf = 1 :: linf ;;
(* val linf : int list = [1; <cycle>] *)

let rec linf = 1 :: 2 ::  linf ;;
(* val linf : int list = [1; 2; <cycle>] *)




(* SELEKTORY LIST *)

(* 1 selektor :List.hd -- head*)


let l = 1 :: 2 :: 4 :: [];;


List.hd l;;
(* head - głowa *)


let xs = [10; 2];;

List.hd xs;;
(* - : int = 10 *)


(* 2 selektor :List.tl -- tail*)


List.tl [1;2;3;4];;
(* - : int list = [2; 3; 4] *)

(* 
Lista pusta nie ma głowy ani ogona: 
List.tl [];;
Exception: Failure "tl".

List.hd [];;
Exception: Failure "hd".
 *)


(* wyjęcie n - tego elementu *)

 let rec nth xs n = 
  if n = 0
    then List.hd xs
else nth (List.tl xs) (n - 1);;

(* val nth : 'a list -> int -> 'a = <fun> *)


nth [1;2;3;4] 2;;
(* - : int = 3 *)



nth [1;2;3;4] 12;;
(* Exception: Failure "tl". *)



let rec linf = 1 :: linf ;;
(* val linf : int list = [1; <cycle>] *)


nth linf 10000;;
(* - : int = 1 *)



(* Ostatni wskaźnik na listę pustą (nie na None czy coś takiego) *)


(* nth (1:: 2 :: []) 1
   -> if 1 = 0 then List.hd (1:: 2 :: []) else nth (List.t1 (1:: 2 :: [])) (1 - 1)
   -> nth (2 :: []) 0 -> ...
   -> List.hd (2:: [])
   -> 2*)

(* Wywołanie ogonowe - nie używamy wcześniejszych wart, iteracyjnie *)



(* SCALANIE DWÓCH LIST *)


(* Czas działania - dł pierwszej listy *)
let rec append xs ys = 
  if xs = []
    then ys
else List.hd xs :: append (List.tl xs) ys;;
(* val append : 'a list -> 'a list -> 'a list = <fun> *)


append [1;2;3] [4;4;10];;
(* - : int list = [1; 2; 3; 4; 4; 10] *)

append [2;2] [];;
(* - : int list = [2; 2] *)


append [] [1];;
(* - : int list = [1] *)


append [] [];;
(* - : 'a list = [] *)


(* listy jednostronnie łączone *)

(* :: - wyrażenie, obliczone na koniec *)

(*  append (1 :: 2 :: [])  (3 :: 4 :: [])
-> 1 :: append (2 :: [])  (3 :: 4 :: []))
-> 1 :: 2 ::  append ([])  (3 :: 4 :: []))
-> 1 :: 2 :: 3 :: 4 :: []*)


(* dopasowanie wzorca ver2*)
let rec append2 xs ys = 
  match xs with 
  | [] -> ys
  | x :: xs' -> x :: append2 xs' ys;;



let a, b = 1., 2;;
(* val a : float = 1.
val b : int = 2 *)



(* match [] with x :: xs -> xs;; *)

(* Line 1, characters 0-27:
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]

Line 1, characters 0-27:
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]

Exception: Match_failure ("//toplevel//", 1, 0). *)






(* ODWRACANIE LISTY - iteracyjnie, złożonośc liniowa*)

let rev xs = 
  let rec it xs ys =
    match xs with
    | [] -> ys
    | x :: xs' -> it xs' (x :: ys)

  in it xs [];;

(* it - akumulator *)



(* rev (1 :: 2 :: [])
   -> it (1 :: 2 :: []) []
   -> it (2 :: []) (1 :: [])
   -> it ([]) (2 :: 1 :: [])
   -> (2 :: 1 :: [])
    *)

let rec rev2 xs =
  match xs with
  | [] -> []
  | x :: xs' -> append (rev2 xs') [x]
  ;;


(* Bardzo duża złożoność - używanie append - złożonośc kwadratowa n^ 2, dokładnie: 1 + 2 + 3 + ... + n*)


let rec mk_list n =
  if n = 0 then [] else n :: mk_list (n - 1);;




let _ = rev (mk_list 100);;
(* liniowa *)

let _ = rev2 (mk_list 100);;
(* kwadratowa *)





let rec all_but_last xs = 
  match xs with
  | [] -> failwith "empty list"
  | [x] -> []
  | x :: xs' -> x :: all_but_last xs'
;;




let rec last xs = 
  match xs with
  | [] -> failwith "empty list"
  | [x] -> x
  | x :: xs' -> last xs'
;;

(* funkcja która nie jest rekurencyjnie strukturalna:  *)
let rec rev3 xs =
  if xs = [] then [] else last xs :: rev3 (all_but_last xs);;
(* wywołanie rek na wywołaniu innej funkcji *)



(* SORTOWANIE *)


(* sortowanie przez wstawianie*)

let rec insert n xs = 
  match xs with
  | [] -> [n]
  | x :: xs' -> 
    if n < x
      then n :: xs
  else x :: insert n xs';;


  insert 2 [0;1;8];;
  (* - : int list = [0; 1; 2; 8] *)



let rec insertion_sort xs = 
  match xs with
  | [] -> []
  | x :: xs' -> insert x (insertion_sort xs')
  ;;



let insertion_sort_iter xs = 
  let rec it xs ys =
    match xs with
    | [] -> []
    | x :: xs' -> it xs' (insert x ys)
  in it xs [];;



  insertion_sort_iter [1;2;5;1;0;23;1;8;2];;
(* - : int list = [0; 1; 1; 1; 2; 2; 5; 8; 23] *)





(* długość listy *)
let rec len l =
  match l with
  | [] -> 0
  | a :: b -> 1 + len b;;

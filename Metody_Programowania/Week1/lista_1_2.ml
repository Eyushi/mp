(* Zadanie 2. (3 pkt) *)

(* Dla każdego z poniższych wyrażeń stwierdź, czy jest poprawnie otypowane,
   jeśli tak, to podaj jego typ. Uzasadnij swoją odpowiedź. *)

" foo " ^ 42;;

(* NIE, "foo" to string, 42 to int *)
(* ^ czyli konkatenacja, konkatenować można tylko typu str *)

" foo " ^ string_of_int 42;;

(* TAK, "foo" to string, string_of_int 42 to string, ^ łączy dwa stringi *)

1. = 2;;

(* NIE, różne typy: 1. to float, 2 to string *)

fun a -> a + 5;;

(* - : int -> int = <fun> *)
(* TAK, przy czym a musi być typu ino *)

fun a -> if a > 5 then a else " foo ";;

(* NIE, a z a > 5 jest int, ale w if warunek then expression1 else expression2,
   expression1 oraz expression2 muszą mieć ten sam typ *)

fun a b -> if a > 5 then a else b;;

(* - : int -> int -> int = <fun> *)

fun a b ->
  let c = a = b in
  if a > 3 && b = " foo " then c else false
;;

(* NIE, najpierw c jest przyrównane do a oraz b (więc muszą mieć ten sam typ),
   z a > 3 wynika, że a, b oraz c mają typ int, zatem nie można porównać b = "foo",
   oraz else false (tu jest typ boolowski) *)


fun a ->
  let f a b = (a * a) + (b * b) in
  f (f a a) a
;;
(* - : int -> int = <fun> *)
(* funkcja fun przyjmuje jeden argument int a oraz zwraca jeden argument *)

let f a = a > 2 in
  if 3 > 2 then true else f (f 2);;
(* NIE, z jest int (bo a > 2), natomiast w if warunek then expr1 else expr2, *)
(* expr1 to true (czyli typ bool), natomiast f ... zwraca bool,
   więc nie można przyrównać z 2*)

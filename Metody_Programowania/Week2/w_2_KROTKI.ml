(* KROTKI *)


let t = (42, "foo");;
(* val t : int * string = (42, "foo") *)
(* iloczyn dwóch typów (mogą być różne) *)


let t3 = (42, "foo", false);;
(* val t3 : int * string * bool = (42, "foo", false) *)


let t3 = 42, "foo", false;;
(* nie trzeba pisać nawiasów *)


fst t;;
(* - : int = 42 *)






t3;;
(* - : int * string * bool = (42, "foo", false) *)

(* DOPASOWANIE WZORCA  - umożliwa uzyskanie pierwszego, drugiego itp elementu krotki (elementy składowe)*)


let a, b, c = t3;;
(* val a : int = 42
val b : string = "foo"
val c : bool = false *)


(* WZORCE W ARGUMENTACH FUNKCJI *)


let fst_of_3 (a, b, c) = a;;
(* val fst_of_3 : 'a * 'b * 'c -> 'a = <fun> *)
let fst_of_3 (a, _, _) = a;;


fst_of_3 t3;;
(* - : int = 42 *)


(* fst t3;; -- nie działa, zdefiniowane tylko dla krotek 2 elementowych*)
(* Error: This expression has type int * string * bool
       but an expression was expected of type 'a * 'b *)



let snd_of_3 (_, a, _) = a;;
(* val snd_of_3 : 'a * 'b * 'c -> 'b = <fun> *)

snd_of_3 t3;;
(* - : string = "foo" *)

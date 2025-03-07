(* typ złożony ma więcej konstruktorów *)

let o = Some 3

(* val o : int option = Some 3 *)
(* przyrostek - sufix innego typu *)

let on = None;;

(* val on : 'a option = None *)
(* none może być dla dowolnego typu option *)

Some "abc";;

(* - : string option = Some "abc" *)

(* SELEKTORY - wydobywanie składowych np fst, fst_of_3 *)

Option.get o;;

(* wybranie wartości z dostępnego modułu *)
(* - : int = 3 *)

Option.get on;;

(* Exception: Invalid_argument "option is None". *)

Option.get;;

(* - : 'a option -> 'a = <fun> *)

(* jak odróżnić some z none *)

(* PREDYKATY - FUNKCJE które zwracają wart bool *)

Option.is_some o;;

(* - : bool = true *)

Option.is_none on

let o = Some 3

(* val o : int option = Some 3 *)

let on = None;;

(* val on : 'a option = None *)

match o with
| Some a -> a
| None -> 0
;;

(* - : int = 3 *)

match on with
| Some a -> a
| None -> 0

(* - : int = 0 *)

let val_of_option o b =
  match o with
  | Some a -> a
  | None -> b
;;

(* val val_of_option : 'a option -> 'a -> 'a = <fun> *)

val_of_option on (-3);;

(* - : int = -3 *)

val_of_option (Some "foo") "bar"
(* - : string = "foo" *)



o;;
(* - : int option = Some 3 *)





(* DEFINIOWANIE WŁASNYCH TYPÓW *)

type vec2 = float * float;;
(* type vec2 = float * float *)

let mk_vec x y = (x, y)
(* val mk_vec : 'a -> 'b -> 'a * 'b = <fun> *)

mk_vec 2. 3.;;
(* - : float * float = (2., 3.) *)

let mk_vec x y = ((x,y) : vec2)
(* val mk_vec : float -> float -> vec2 = <fun> *)


let a, b = mk_vec 2. 3.;;
(* val a : float = 2.
val b : float = 3. *)













let vec_x v = fst v;;
(* val vec_x : 'a * 'b -> 'a = <fun> *)




let vec_x (v: vec2) = fst v;;
(* val vec_x : vec2 -> float = <fun> *)
(* bardziej ograniczony *)

let vec_y (v: vec2) = snd v;;



vec_x (mk_vec 1. 2.);;
(* - : float = 1. *)


vec_x (mk_vec 1. "foo");;
(* Error: This expression has type string but an expression was expected of type
         float *)


fst (1., "foo");;
 (* - : float = 1. *)





let is_vec_zero (v : vec2) = fst v = 0. && snd v = 0;;

(* to samo: *)

let is_vec_zero ((x, y) : vec2) = x = 0. && y = 0;;



let square x = x *. x;;


let vec_length v = sqrt(square(vec_x v) +. square(vec_y v));;




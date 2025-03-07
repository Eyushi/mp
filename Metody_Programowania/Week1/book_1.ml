(* LET EXPRESSIONS AND SCOPE *)

let x = 42

(* val x : int = 42 *)

let f y = x + y;;

(* val f : int -> int = <fun> *)

f 0

(* : int = 42 *)

let x = 22;;

(* val x : int = 22 *)

f 0

(* - : int = 42  x did not mutate! *)

(* FUNCTION DEFINITIONS *)

(* Option1       let fun_name = fun var_name -> expr *)
let inc x = x + 1
let avg x y = (x +. y) /. 2.

(* Option2       let fun_name var_name = expr        -- more used*)
let inc x = x + 1
let avg x y = (x +. y) /. 2.;;

(* Where:
   fun_name - our name of the function
   var_name - our name of the variable
   expr - our expression - can use the var_name*)

(* Ugly vs beautiful code: *)
(fun x -> x + 1) 2;;

(*Ugly*)
let x = 2 in
x + 1

(*Beautiful <3 *)

(* RECURSIVE FUNCTIONS *)

(* let f x = ...         NON - recursive *)
(* let rec f x = ...         RECURSIVE *)

(** [fact n] is [n]!.
    Requires: [n >= 0]. *)

(* SILNIA *)
let rec fact n = if n = 0 then 1 else n * fact (n - 1)
(*it won't work without the parenthesis,
  because OCaml understand this line like:
  else n * (fact n) - 1
  thus ending in an infinite loop ...*)

(* POTĘGA *)

(** [pow x y] is [x] to the power of [y].
    Requires: [n >= 0]. *)

let rec pow x y = if y = 1 then x else x * pow x (y - 1)

(* Mutually recursive function - with and *)

(** [even n] is whether [n] is even.
    Requires: [n >= 0]. *)
let rec even n = n = 0 || odd (n - 1)

(** [odd n] is whether [n] is odd.
    Requires: [n >= 0]. *)
and odd n = n <> 0 && even (n - 1)
;;

(* Anonymous Functions *)
fun x y -> x + y;;

(* - : int -> int = <fun> *)

(fun x y -> x + y) 2 3

(* Assertions *)
(*Check if the result is true:          let () = assert (f input1 = output1)*)
let inc x = x + 1
let () = assert (inc 2 = 3)
let () = assert (inc 5 = 6)

(* nothing *)

let () = assert (inc 2 = 2)
(* Exception: Assert_failure ("//toplevel//", 1, 9). *)

(* And && *)

(* Or || *)

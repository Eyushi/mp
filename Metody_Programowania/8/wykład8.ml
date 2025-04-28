(* W Y K Ł A D  7 *)

(* let a = fun x -> x * 2 *)

(* funkcja bierze funkcję *)
(* (fun x -> x * 2) (fun x -> x + 1) *)

(* funkcja zwraca funkcję *)

(* (fun x -> fun y -> x + y) 2 *)

(* aplikacja zawsze łączy w lewo?? *)

(* statyczne wiązanie zmiennych *)
let x = 4
let foo y = y + x
let x = 100

(* foo 1 *)
(* - : int = 5 *)

(* statyczne wiązanie zmiennych *)
(* let x = 4 in let foo y = x + y in let x = 10 in foo 1 *)
(* - : int = 5 -- nadal 5 *)

(* jest źle :(   ) *)
let rec even x = if x = 0 then true else not (odd (x - 1))
and odd x = if x = 0 then true else not (even (x - 1))

(* W Y K Ł A D   8 *)
(* Składnia konkretna (tekst) -- parser + lekser -- >
   Składnia abstrakcyjna (w formie wartości w OCamlu) -- evaluator -->
   WARTOŚĆ *)

(* Zadanie 1. *)
(* DONE *)

(* rekurencja z definicji *)
let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

(* tail recursion *)
(* iteracyjna z pomocniczą funkcją *)

let fib_iter n =
  if n = 0
  then 0 (*szczególny przypadek dla 0, inaczej fib_iter 0 = 1*)
  else (
    let rec it n acc1 acc2 =
      if n <= 0 then acc1 + acc2 else it (n - 1) acc2 (acc1 + acc2)
    in
    it (n - 2) 0 1)
;;

(* PORÓWNANIE *)

(* stos wywołań duży *)
(* fib 40 -- 4 sekundy *)
(* fib 42 -- 7 sekund *)
(* fib 43 -- 10 sekund *)

(* fib_iter 40 -- od razu *)
(* fib_iter 42 -- od razu *)
(* fib_iter 43 -- od razu *)

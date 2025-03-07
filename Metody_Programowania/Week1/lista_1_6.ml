let rec f () = f ()
let test x y = if x = 0 then 0 else y;;

test 0 (f ())

(* nawiasy do wyliczenia wart f() *)

(* OCaml używa gorliwej kolejności obliczania,
   przez to zamiast od razu sprawdzić, że dla x = 0 (tak jak tutaj),
   funkcja od razu zwraca 0 to liczy rekurencyjną funkcję f(),
   która powoduje przejście w nieskończoną pętlę *)

(* musi obliczyć argumenty najpierw *)
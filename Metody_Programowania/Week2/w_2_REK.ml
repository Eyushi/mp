let rec loop () = loop ()
(* rekurencja - potrzebne słowo rec przed nazwą funkcji*)

(* loop () -> loop () -> loop () -> ... *)

(* REKURENCJA *)

let rec fact n =
   if n = 0 then 1
   else n * fact (n - 1)

(* Stos obliczeń się rozrasta *)
(* fact 5 ->
   if 5 = 0 then 1 else 5 * fact (5 - 1) ->
   5 * fact (5 - 1) ->
   5 * fact 4 -> ... ->
   5 * (4* (3 * (2 * (1 * fact 0)))) ->
   5 * (4 * (3 * (2 * 1))) ->
   5 * 24 -> ... ->
   120

   if 5 = 0 then 1 else 5 * fact (5 - 1) ->
   if 5 = 0 then 1 else 5 * fact (5 - 1) ->*)


(* WYWOŁANIE OGONOWE *)

let fact_iter n =
  let rec it n acc = if n = 0 then acc else it (n - 1) (acc * n) in
  it n 1
;;

(* na początku dajemy do akumulatora 1 *)

(* acc - akumulator, pośredni wynik obliczeń *)
(* Nie rozszerza się stos - dzięki it *)
(* fact_iter t
   -> it 5 1
   -> if 5 = 0 then 1 else it (5 - 1) ( 1 * 5)
   -> it  (5 - 1) ( 1 * 5)
   -> it 4 5
   -> it 3 20
   -> it 2 60
   -> it 1 120
   -> it 0 120
   -> 120*)

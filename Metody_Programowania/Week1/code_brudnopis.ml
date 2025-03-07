let x = 2

let y = x + 2;;

let increment x = x + 1



let square x = x * x



let y = x + 2

(* let g a b = p + a * b  *)
(* zmienna wolna p *)



(* CTRL + K + C *)
(* 2 * 3 + 4 =>
   6 + 4 => 10 *)


(* 2 + sqrt 3 => 2 + 3 * 3 => 2 + 9 => 11 *)



(*Opcja 1:            GORLIWA - OCalm
najpierw obliczamy argumenty do wartości, żeby obliczyć funkcję
 2 + square (increment 1) =>
   2 + square ( 1 + 1) =>
   2 + 2 * 2 =>
   2 + 4 =>
   6 *)


(* 2 + sqrt 3 => 2 + 3 * 3 => 2 + 9 => 11 *)

(*Opcja 1:            LENIWA też: normalna 
Zalety: nie liczy nieużywanych wyrażeń (których wartość nie jest potrzebna)
wada : tutaj: policzenie increment jeden dwa razy
czytamy wyrażenie od środka (korzenia), od razu podstawiamy
 2 + square (increment 1) =>
 2 + increment 1 * increment 1 =>
 2 + 2 * increment 1 =>
 2 + 2 * 2 =>
 2 + 4 => 6 *
 Obie metody - ten sam wynik (brak modyfikowanych zmiennych),
 kolejność nie ma znaczenia) *)



(* let x = 2 + 3 in x * x =>
   let x = 5 in x * x =>
   5 * 5 => 25 *)





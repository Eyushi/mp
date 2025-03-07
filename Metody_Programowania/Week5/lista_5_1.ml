(* Zadanie 1.
Przypomnij sobie definicję funkcji map. Następnie pokaż, że dla dowolnych funkcji
f i g oraz listy xs zachodzi map f (map g xs) ≡ map (fun x -> f (g x)) xs. Mo-
żesz założyć, że funkcje f i g poprawnie obliczają się do wartości dla dowolnego
argumentu. *)




let rec map f = function
  | [] -> []
  | x :: xs' -> f x :: map f xs'


  
let rec map2 f xs =
  match xs with
  | [] -> []
  | x :: xs' -> f x :: map2 f xs'












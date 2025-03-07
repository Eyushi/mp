let cube_max a b c =
  if a >= c && b >= c
  then (a * a) + (b * b)
  else if a >= b && c >= b
  then (a * a) + (c * c)
  else (b * b) + (c * c)
;;

(* zsumować kwadraty 3 i odjąc najmniejszy kwadrat *)

(* napisać funkcję suma kwadratów *)
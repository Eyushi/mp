(* Zadanie 2. (2 pkt) *)

let t1 = 2, 3, 2, 5
let t2 = 3, 5, 7, 8

(* to samo: bez nawiasów *)
(* let t2 = (3, 5, 7, 8);; *)

(* FUNKCJE POMOCNICZE *)
let fst_of_4 (a, _, _, _) = a
let snd_of_4 (_, b, _, _) = b
let thi_of_4 (_, _, c, _) = c
let four_of_4 (_, _, _, d) = d


(* GDZIE:
   a       b
   c       d *)

  
let matrix_mult m n =
  ( (fst_of_4 m * fst_of_4 n) + (snd_of_4 m * thi_of_4 n)
  , (fst_of_4 m * snd_of_4 n) + (snd_of_4 m * four_of_4 n)
  , (thi_of_4 m * fst_of_4 n) + (four_of_4 m * thi_of_4 n)
  , (thi_of_4 m * snd_of_4 n) + (four_of_4 m * four_of_4 n) )
;;

let matrix_id (a, b, c, d) = a, b, c, d;;

let rec matrix_exp m k =
  if k = 1 then m
  else matrix_mult m (matrix_exp m (k - 1))
  ;;


(* fib_matrix *)

let fib_matrix k =
  snd_of_4(matrix_exp (1, 1, 1, 0) k)
;;



fib_matrix 10;;
(* - : int = 55 *)

(* PRZYKŁADY *)
matrix_mult t1 t2;;

(* - : int * int * int * int = (27, 34, 41, 50) -- poprawnie *)

let t1 = 1, 2, 3, 4
let t2 = 5, 6, 7, 8;;

matrix_mult t1 t2;;
(* - : int * int * int * int = (19, 22, 43, 50) -- dobrze *)



matrix_id t1;;

(* - : int * int * int * int = (1, 2, 3, 4) *)


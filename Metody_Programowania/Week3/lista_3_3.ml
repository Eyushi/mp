(* Zadanie 3. (2 pkt) *)

let build_list n f =
  let rec help n f i = if i = n then [] else f i :: help n f (i + 1) in
  help n f 0
;;

(* negatives n, zwracającą listę liczb ujemnych od −1 do −n, *)
let negatives n = build_list n (fun x -> 0 - (x + 1));;

negatives 5

(* - : int list = [-1; -2; -3; -4; -5] *)

(* reciprocals n, zwracającą listę odwrotności liczb od 1 do n (czyli 1, . . . , 1/ n ), *)
let reciprocals n = build_list n (fun x -> 1. /. float_of_int (x + 1));;

reciprocals 5

(* - : float list = [1.; 0.5; 0.333333333333333315; 0.25; 0.2] *)

(* evens n, zwracajacą listę pierwszych n liczb parzystych, *)

let evens n = build_list n (fun x -> 2 * x);;

evens 5

(* - : int list = [0; 2; 4; 6; 8] *)

(* identityM n, zwracającą macierz identycznościową o wymiarach n × n w
   postaci listy list:
   # identityM 3
   - : int list list = [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]] *)

let identityM n = build_list n (fun x -> build_list n (fun y -> if x = y then 1 else 0));;

identityM 5
(* - : int list list =
     [[1; 0; 0; 0; 0];
     [0; 1; 0; 0; 0];
     [0; 0; 1; 0; 0];
     [0; 0; 0; 1; 0];
     [0; 0; 0; 0; 1]] *)

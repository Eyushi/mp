(* Zadanie 4. (2 pkt) *)

(* – reprezentacja zbioru pustego, *)
let empty_set a = false

(* – zwraca zbiór zawierający wyłącznie element a *)
let singleton a = fun x -> x = a
(* typ 'a -> ('a -> bool) *)

(* – zwraca true gdy a należy do zbioru s, w przeciwnym wy-
   padku wynikiem jest false, *)
let in_set a s = s a

(* – zwraca sumę zbiorów s i t, *)

(* typ: 'a -> bool) -> ('a -> bool) -> ('a -> bool) *)
let union s t = fun a -> in_set a s || in_set a t

(* – zwraca przecięcie zbiorów s i t. *)
let intersect s t = fun a -> in_set a s && in_set a t
(* typ: 'a -> bool) -> ('a -> bool) -> ('a -> bool) *)


(* przykłady: *)

let a = union (singleton 1) (singleton 2)

let remove s t x = s x && t x
(* negacja *)
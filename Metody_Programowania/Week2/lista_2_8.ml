(* Zadanie 8. (2 pkt) *)

(* SORTOWANIE PRZEZ WYBÓR *)
(* fmin -- find min *)
(* dmin -- del min *)
(* na dwa obroty pętli -- znalezienie min, następnie usunięcie go *)
let rec select xs =
  (*strukturalnie rek*)
  let rec fmin xs' min =
    match xs' with
    | [] -> min
    | h :: t -> if h < min then fmin t h else fmin t min
  in
  let rec dmin xs min =
    match xs with
    | [] -> failwith "empty list"
    | h :: t -> if h = min then t else h :: dmin t min
  in
  let min = fmin xs (List.hd xs) in
  min, dmin xs min
;;

select [ 4; 3; 1; 2; 5 ]

(* - : int * int list = (1, [4; 3; 2; 5]) *)

let rec select_sort xs =
  (*NIE jest strukturalnie rek*)
  match xs with
  | [] -> []
  (* | h :: t -> fst (select xs) :: select_sort (snd (select xs))
     - dwa wywołania te same argumenty*)
  | h :: t ->
    let pom = select xs in
    fst pom :: select_sort (snd pom)
;;

select_sort [ 1; 5; 0; 7; 1; 4; 1; 0 ]

(* - : int list = [0; 0; 1; 1; 1; 4; 5; 7] *)

(* PRZEZ WSTAWIANIE -- WYKŁAD *)

let rec insert n xs =
  match xs with
  | [] -> [ n ]
  | x :: xs' -> if n < x then n :: xs else x :: insert n xs'
;;

let rec insertion_sort xs =
  match xs with
  | [] -> []
  | x :: xs' -> insert x (insertion_sort xs')
;;

let insertion_sort_iter xs =
  let rec it xs ys =
    match xs with
    | [] -> ys
    | x :: xs' -> it xs' (insert x ys)
  in
  it xs []
;;



let rec append xs ys =
  match xs with
  | [] -> ys
  | x:: xs' -> x :: append xs' ys
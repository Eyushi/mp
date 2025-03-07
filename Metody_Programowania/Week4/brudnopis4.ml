module type DICT = sig
  type ('a, 'b) dict

  val empty : ('a, 'b) dict
  val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
  val remove : 'a -> ('a, 'b) dict -> ('a, 'b) dict
  val find_opt : 'a -> ('a, 'b) dict -> 'b option
  val find : 'a -> ('a, 'b) dict -> 'b
  val to_list : ('a, 'b) dict -> ('a * 'b) list
end

module ListDict : DICT = struct
  type ('a, 'b) dict = ('a * 'b) list

  let empty = []
  let remove k d = List.filter (fun (k', _) -> k <> k') d
  let insert k v d = (k, v) :: remove k d
  let find_opt k d = List.find_opt (fun (k', _) -> k = k') d |> Option.map snd
  let find k d = List.find (fun (k', _) -> k = k') d |> snd
  let to_list d = d
end

module type PRIO_QUEUE = sig
  type ('a, 'b) pq

  val empty : ('a, 'b) pq
  val insert : 'a -> 'b -> ('a, 'b) pq -> ('a, 'b) pq
  val pop : ('a, 'b) pq -> ('a, 'b) pq
  val min_with_prio : ('a, 'b) pq -> 'a * 'b
end

module ListPrioQueue : PRIO_QUEUE = struct
  type ('a, 'b) pq = ('a * 'b) list

  let empty = []

  (* a, b -priority *)
  let rec insert a x q =
    match q with
    | [] -> [ a, x ]
    | (b, y) :: ys -> if a < b then (a, x) :: q else (b, y) :: insert a x ys
  ;;

  let pop q = List.tl q
  let min_with_prio q = List.hd q
end

(* częstość symboli *)

let id x = x

let find_else x a d =
  (* Option.fold ~none:a ~some:Fun.id(ListDict.find_opt x d) *)
  Option.value ~default:0 (ListDict.find_opt x d)
;;

let freq_dict xs =
  let rec it xs d =
    match xs with
    | [] -> d
    | x :: xs' -> it xs' (ListDict.insert x (1 + find_else x 0 d) d)
  in
  it xs ListDict.empty
;;

(* budowanie drzew kodowych *)

type 'a code_tree =
  | CTNode of 'a code_tree * 'a code_tree
  | CTLeaf of 'a

let initial_pq xs =
  List.fold_left (fun q (x, n) -> ListPrioQueue.insert n (Leaf x) q) ListPrioQueue.empty xs
;;

let rec algo q =
  let p1, t1 = ListPrioQueue.min_with_prio q
  and q1 = ListPrioQueue.pop q
in if q1 = ListPrioQueue.empty then t1
else let p2, t2 = ListPrioQueue.min_with_prio q1
and q2 = ListPrioQueue.pop q1
in algo (ListPrioQueue.insert (p1 + p2) (CTNode (t1, t2)) q2)




let make_code_tree d =
  (* kolejka drzew ? *)
  ListDict.to_list d |> initial_pq |> algo
;;
(* przykład *)


let ex_string = "konstantynopolitańczykowianeczka"


let list_of_string s = String.to_seq s |> List.of_seq

let ex_freq_dict = freq_dict (list_of_string ex_string)

let ex_code_tree = make_code_tree ex_freq_dict




let dict_of_code_tree = 
  let rec aux t rcpref d=
  match t with
  | CTLeaf x = ListDict.insert x (List.rev rcpref) d
  | CTNode (l, r) -> aux l (0:: rcpref) (aux r (1:: rcpref) d)
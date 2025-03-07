to do


module Queue = struct
    
  type 'a queue = Queue of 'a list * 'a list

  let empty = Queue([], [])


  let peek = function
  | Queue (x:: _, _) -> x
  | Queue ([], _) -> failwith "peek on empty queue"


  let mk_queue = function
  | ([], ys) -> Queue(List.rev ys, [])
  | (ys, xs) -> Queue(xs, ys)



  (* może się zdażyć tak, ze jedna jest pusta - sprawdzamy mk_queue*)
  let push x (Queue (xs, ys)) = mk_queue (xs, x::ys)


  let pop = function
  |Queue ( _ :: xs, ys) -> mk_queue (xs, ys)
  | Queue ([], _) -> failwith "pop on empty queue"

(* koniec modułu *)
end
(* wszystkie pola modułu dodaje do bierzącego środowiska *)

module Stack = struct
  let empty = []
  let peek = function
  | [] -> failwith "peek on empty stack"
  | x :: xs -> xs
  
  let pop = function
  | [] -> failwith "pop on empty stack"
  | _ :: xs -> xs
end



open Queue
(* przykład *)

let q = Queue.empty
let q = Queue.push 2 q

let q = Queue.push 3 q
let q = Queue.push 4 q
let q = Queue.pop q

(* ######################33 *)


type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree


let ex_tree = Node( Node(Node(Leaf,0, Leaf),1, Node(Leaf, 2, Leaf)),3,(Node(Leaf, 4, Node(Leaf, 5, Leaf))))


let bfs t =
  let rec it q xs = 
    if q = empty then List.rev xs
    else match peek q with
    | Leaf -> it (pop q) xs
    (* inna versja *)
    (* | Node (l, v, r) -> it (push r(push l(pop q))) (v:: xs) *)
    | Node (l, v, r) -> it (q |> pop |> push l |> push r) ( v :: xs)
  in it (push t empty) []




(* ######################33 *)








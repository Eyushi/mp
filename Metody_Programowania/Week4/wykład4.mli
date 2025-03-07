module Queue : sig
  type 'a queue = Queue of 'a list * 'a list

  val empty : 'a queue
  val peek : 'a queue -> 'a
  val mk_queue : 'a list * 'a list -> 'a queue
  val push : 'a -> 'a queue -> 'a queue
  val pop : 'a queue -> 'a queue
end

val q : 'a Queue.queue
val q : int Queue.queue
val q : int Queue.queue
val q : int Queue.queue
val q : int Queue.queue

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

val ex_tree : int tree
val bfs : 'a tree -> 'a list

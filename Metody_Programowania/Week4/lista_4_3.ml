(* z zadania 2 *)
module type DICT = sig
  type key
  type 'a dict

  val empty : 'a dict
  val insert : key -> 'a -> 'a dict -> 'a dict
  val remove : key -> 'a dict -> 'a dict
  val find_opt : key -> 'a dict -> 'a option
  val find : key -> 'a dict -> 'a
  val to_list : 'a dict -> (key * 'a) list
end


(* z polecenia zad 2 *)
module type OrderedType = sig
  type t
  val compare : t -> t -> int
  end

(* ########################################################################3 *)

module MakeListDict (KEY: Map.OrderedType) : DICT with type key = KEY.ty = struct
  type key = KEY.ty
  type 'a dict =  (key * 'a) list

  let empty = []
  let remove k d =  List.filter (fun k' -> if (KEY.compare k' k = 0) then false else true) d
  let insert k v d =  (k, v) :: remove k d
  end



(* odpowiedz:
   sygnatura *)

(* ########################################################################3 *)

(* WYKŁAD *)

module type DICT = sig
  type ('a, 'b) dict

  val empty : ('a, 'b) dict
  val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
  val remove : 'a -> ('a, 'b) dict -> ('a, 'b) dict
  val find_opt : 'a -> ('a, 'b) dict -> 'b option
  val find : 'a -> ('a, 'b) dict -> 'b
  val to_list : ('a, 'b) dict -> ('a * 'b) list
end

(* ########################################################################3 *)

module ListDict : DICT = struct
  type ('a, 'b) dict = ('a * 'b) list

  let empty = []
  let remove k d = List.filter (fun (k', _) -> k <> k') d
  let insert k v d = (k, v) :: remove k d
  let find_opt k d = List.find_opt (fun (k', _) -> k = k') d |> Option.map snd
  let find k d = List.find (fun (k', _) -> k = k') d |> snd
  let to_list d = d
end

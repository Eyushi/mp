


module MakeMapDict = functor (M:Map.OrderedType) -> struct
  module Dict = Map.Make(M) 
  (* wzięcie instacji modułu *)
  type key = Dict.key
  type 'a dict = 'a Dict.key

  let remove = Dict.remove
  let empty = Dict.empty
   (*reszta ma taką samą nazwę  *)

  let insert = Dict.add


    
  
end





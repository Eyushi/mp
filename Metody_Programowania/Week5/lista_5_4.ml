(* Zadanie 4.
   Zdefiniuj funkcję neg_nnf typu 'a nnf -> 'a nnf negującą formułę w negacyjnej
   postaci normalnej. Następnie pokaż, że neg_nnf (neg_nnf φ) ≡ φ dla dowolnej
   formuły φ.
*)

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf

let rec neg_nnf v =
  match v with
  (* not bo, zm *)
  | NNFLit (bo, zm) -> NNFLit ((if bo then false else true), zm)
  | NNFConj (v1, v2) -> NNFDisj (neg_nnf v1, neg_nnf v2)
  | NNFDisj (v1, v2) -> NNFConj (neg_nnf v1, neg_nnf v2)
  (* z de Morgana *)
;;


(* przykłady *)
let p = "p"
let q = "q"
let (v1 : 'v nnf) = NNFConj (NNFLit (true, p), NNFLit (false, q))
let v2 = neg_nnf v1



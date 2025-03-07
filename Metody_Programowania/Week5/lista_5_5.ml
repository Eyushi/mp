(* Zadanie 5. (2 pkt)
   Zdefiniuj funkcję eval_nnf typu ('a -> bool) -> 'a nnf -> bool interpretującą for-
   mułę w negacyjnej postaci normalnej, przy zadanym wartościowaniu zmiennych
   (funkcji typu 'a -> bool). Następnie pokaż, że dla dowolnej formuły φ i war-
   tościowania σ zachodzi eval_nnf σ (neg_nnf φ) ≡ not (eval_nnf σ φ). Możesz
   założyć, że funkcja σ zawsze się zatrzymuje. *)

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf

(* zad 4 *)
let rec neg_nnf v =
  match v with
  | NNFLit (bo, zm) -> NNFLit ((if bo then false else true), zm)
  | NNFConj (v1, v2) -> NNFDisj (neg_nnf v1, neg_nnf v2)
  | NNFDisj (v1, v2) -> NNFConj (neg_nnf v1, neg_nnf v2)
;;

(* ######################################################## *)
(* ROZWIĄZANIE *)
let rec eval_nnf sigma form =
  match form with
  | NNFLit (bo, zm) -> if bo == false then not (sigma zm) else sigma zm
  (* Conjunction -- koniunkcja *)
  | NNFConj (fi, psi) -> eval_nnf sigma fi && eval_nnf sigma psi
  (* Disjunction -- alternatywa (dysjunkcja) *)
  | NNFDisj (fi, psi) -> eval_nnf sigma fi || eval_nnf sigma psi
;;

(* ######################################################## *)

(* przykłady: *)
let sig1 a = if a = "p" then true else false
let p = "p"
let q = "q"
let w2 = eval_nnf sig1 (NNFLit (true, p))
let w3 = eval_nnf sig1 (NNFLit (false, p))
let w4 = eval_nnf sig1 (NNFLit (true, q))
let w5 = eval_nnf sig1 (NNFLit (false, q))

(* długie przykłady *)
let (v1 : 'v nnf) = NNFConj (NNFLit (true, p), NNFLit (false, q))
let (v2 : 'v nnf) = NNFDisj (NNFLit (false, p), NNFLit (true, q))
let w6 = eval_nnf sig1 v1
let w7 = eval_nnf sig1 v2

(* ####################################### *)
let a = not true

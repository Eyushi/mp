(* Zadanie 7. (2 pkt)
   Zdefiniuj funkcję eval_formula interpretującą formuły z poprzedniego zadania.
   Następnie pokaż, że eval_nnf σ (to_nnf φ) ≡ eval_formula σ φ. Możesz założyć,
   że funkcja σ zawsze się zatrzymuje. *)

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf

(* z zad 5 *)

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf

let rec eval_nnf sigma form =
  match form with
  | NNFLit (bo, zm) -> if bo == false then not (sigma zm) else sigma zm
  (* Conjunction -- koniunkcja *)
  | NNFConj (fi, psi) -> eval_nnf sigma fi && eval_nnf sigma psi
  (* Disjunction -- alternatywa (dysjunkcja) *)
  | NNFDisj (fi, psi) -> eval_nnf sigma fi || eval_nnf sigma psi
;;

(* ################################################# *)

(* z zad 6 *)

type 'v formula =
  | Var of 'v
  | Neg of 'v formula
  | Conj of 'v formula * 'v formula
  | Disj of 'v formula * 'v formula

(* zad 4 *)
let rec neg_nnf v =
  match v with
  | NNFLit (bo, zm) -> NNFLit (not bo, zm)
  | NNFConj (v1, v2) -> NNFDisj (neg_nnf v1, neg_nnf v2)
  | NNFDisj (v1, v2) -> NNFConj (neg_nnf v1, neg_nnf v2)
;;


(* ROZWIĄZANIE *)
let rec to_nnf fi =
  match fi with
  | Var p -> NNFLit (true, p)
  | Neg (psi) -> neg_nnf (to_nnf psi)
  | Disj (fi, psi) -> NNFDisj (to_nnf fi, to_nnf psi)
  | Conj (fi, psi) -> NNFConj (to_nnf fi, to_nnf psi)
;;


(* ################################################# *)
(* rozwiązanie *)

let rec eval_formula sigma form =
  match form with
  | Var v -> sigma v
  | Neg fi -> not (eval_formula sigma fi)
  | Conj (fi, psi) -> eval_formula sigma fi && eval_formula sigma psi (*koniunkcja*)
  | Disj (fi, psi) -> eval_formula sigma fi || eval_formula sigma psi (*alternatywa*)
;;

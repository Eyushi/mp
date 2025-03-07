(* Zadanie 6. (2 pkt)
   Formuły rachunku zdań możemy opisać następującym typem.
   type 'v formula =
   | Var of v
   | Neg of 'v formula
   | Conj of 'v formula * 'v formula
   | Disj of 'v formula * 'v formula
   Zdefiniuj funkcję to_nnf transformującą formułę do równoważnej formuły w ne-
   gacyjnej postaci normalnej. Możesz definiować funkcję pomocnicze, ale wszyst-
   kie funkcje (wzajemnie) rekurencyjne powinny używać rekursji strukturalnej. *)

type 'v formula =
  | Var of 'v
  | Neg of 'v formula
  | Conj of 'v formula * 'v formula
  | Disj of 'v formula * 'v formula

type 'v nnf =
  | NNFLit of bool * 'v
  | NNFConj of 'v nnf * 'v nnf
  | NNFDisj of 'v nnf * 'v nnf

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
  | Var p -> NNFLit (false, p)
  | Neg (psi) -> neg_nnf (to_nnf psi)
  | Disj (fi, psi) -> NNFDisj (to_nnf fi, to_nnf psi)
  | Conj (fi, psi) -> NNFConj (to_nnf fi, to_nnf psi)
;;


(* done *)

(* https://skos.ii.uni.wroc.pl/pluginfile.php/60045/mod_resource/content/2/skrypt.pdf *)
(* str 34 *)
(* ############################# *)
(* versja 2: (lepsza) *)

let rec to_nnf fi =
  match fi with
  | Var p -> NNFLit (false, p)
  | Neg (psi) -> neg_to_nnf psi
  | Disj (fi, psi) -> NNFDisj (to_nnf fi, to_nnf psi)
  | Conj (fi, psi) -> NNFConj (to_nnf fi, to_nnf psi)
  and
  neg_to_nnf f =match f with
  |Var v -> NNFLit(true,v)
  | Neg(fi) -> to_nnf(fi)
  | Disj (fi, psi) -> NNFDisj (neg_to_nnf fi, neg_to_nnf psi)
  | Conj (fi, psi) -> NNFConj (neg_to_nnf fi, neg_to_nnf psi)
;;



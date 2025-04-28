open Ast

let parse (s : string) : expr = Parser.prog Lexer.read (Lexing.from_string s)

type value =
  | VInt of int
  | VBool of bool

let eval_op (op : bop) (v1 : value) (v2 : value) : value =
  match op, v1, v2 with
  | Add, VInt i1, VInt i2 -> VInt (i1 + i2)
  | Sub, VInt i1, VInt i2 -> VInt (i1 - i2)
  | Mult, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Div, VInt i1, VInt i2 -> VInt (i1 / i2)
  | Eq, VInt i1, VInt i2 -> VBool (i1 = i2)
  | Lt, VInt i1, VInt i2 -> VBool (i1 < i2)
  | Gt, VInt i1, VInt i2 -> VBool (i1 > i2)
  | Leq, VInt i1, VInt i2 -> VBool (i1 <= i2)
  | Geq, VInt i1, VInt i2 -> VBool (i1 >= i2)
  | Neq, VInt i1, VInt i2 -> VBool (i1 <> i2)
  | _ -> failwith "type error"
;;

(* Evaluation via substitution *)

module Subst = struct
  let rec subst (x : ident) (s : expr) (e : expr) : expr =
    match e with
    | Binop (op, e1, e2) -> Binop (op, subst x s e1, subst x s e2)
    | If (p, t, e) -> If (subst x s p, subst x s t, subst x s e)
    | Var y -> if x = y then s else e
    | Let (y, e1, e2) ->
      if x = y then Let (y, subst x s e1, e2) else Let (y, subst x s e1, subst x s e2)
    | _ -> e
  ;;

  let expr_of_value (v : value) : expr =
    match v with
    | VInt a -> Int a
    | VBool a -> Bool a
  ;;

  let rec eval (e : expr) : value =
    match e with
    | Int n -> VInt n
    | Bool b -> VBool b
    | If (p, t, e) ->
      (match eval p with
       | VBool true -> eval t
       | VBool false -> eval e
       | _ -> failwith "type error")
    | Binop (And, e1, e2) ->
      (match eval e1 with
       | VBool true -> eval e2
       | VBool false -> VBool false
       | _ -> failwith "type error")
    | Binop (Or, e1, e2) ->
      (match eval e1 with
       | VBool false -> eval e2
       | VBool true -> VBool true
       | _ -> failwith "type error")
    | Binop (op, e1, e2) -> eval_op op (eval e1) (eval e2)
    | Let (x, e1, e2) ->
      let s = expr_of_value (eval e1) in
      eval (subst x s e2)
    | Var x -> failwith ("unbound value " ^ x)
  ;;

  let interp (s : string) : value = eval (parse s)

  (* DODANA FUNKCJA CP -- TYLKO TUTAJ *)
  (* let rec cp (e : expr) : expr =
     match e with
     | Int _ | Bool _ | Var _ -> e
     | Binop (op, e1, e2) ->
     (match cp e1, cp e2 with
     | Int i1, Int i2 when op = Add -> Int (i1 + i2)
     | Int i1, Int i2 when op = Mult -> Int (i1 * i2)
     | _ -> Binop (op, cp e1, cp e2))
     | If (p, t, e) -> If (cp p, cp t, cp e)
     | Let (x, e1, e2) ->
     let e1' = cp e1 in
     (match e1' with
     | Int _ | Bool _ -> Let (x, e1', cp (subst x e1' e2))
     | _ -> Let (x, e1', cp e2))
     ;; *)
end

(* Evaluation via environments *)

module Env = struct
  module M = Map.Make (String)

  type env = value M.t

  let rec eval_env (env : env) (e : expr) : value =
    match e with
    | Int n -> VInt n
    | Bool b -> VBool b
    | If (p, t, e) ->
      (match eval_env env p with
       | VBool true -> eval_env env t
       | VBool false -> eval_env env e
       | _ -> failwith "type error")
    | Binop (And, e1, e2) ->
      (match eval_env env e1 with
       | VBool true -> eval_env env e2
       | VBool false -> VBool false
       | _ -> failwith "type error")
    | Binop (Or, e1, e2) ->
      (match eval_env env e1 with
       | VBool false -> eval_env env e2
       | VBool true -> VBool true
       | _ -> failwith "type error")
    | Binop (op, e1, e2) -> eval_op op (eval_env env e1) (eval_env env e2)
    | Let (x, e1, e2) ->
      let r = eval_env env e1 in
      let new_env = M.update x (fun _ -> Some r) env in
      eval_env new_env e2
    | Var x ->
      (match M.find_opt x env with
       | Some v -> v
       | None -> failwith ("unbound value" ^ x))
  ;;

  (* ZADANIE 1 -- tylko w main*)
  (* dodana funkcja cp_helper (właściwa funkcja cp niżej) *)
  let rec cp_helper (env : env) (e : expr) : expr =
    match e with
    | Int _ -> e
    | Bool _ -> e
    | Binop (op, e1, e2) ->
      (* let żeby za każdym razem rek nie liczyło *)
      let e1' = cp_helper env e1 in
      let e2' = cp_helper env e2 in
      (match e1', e2' with
       | Int a, Int b ->
         (match op with
          (* od razu wyliczamy *)
          | Add -> Int (a + b)
          | Mult -> Int (a * b)
          | Div -> Int (a / b)
          | Sub -> Int (a - b)
          | _ -> failwith "operatory przyjmujące tylko int")
       | _ -> Binop (op, e1', e2'))
    | Var x ->
      (match M.find_opt x env with
       | Some (VInt i) -> Int i
       | Some (VBool b) -> Bool b
       (* zmienna nie ma jeszcze wartości *)
       | None -> e)
    | If (p, t, e) -> If (cp_helper env p, cp_helper env t, cp_helper env e)
    | Let (x, e1, e2) ->
      let e1' = cp_helper env e1 in
      (match e1' with
       | Int a -> cp_helper (M.add x (VInt a) env) e2
       | Bool b -> cp_helper (M.add x (VBool b) env) e2
       (* zmiennej x przypisujemy Var x, a nie e1' (w środowisku)
          -- bo e1' nie uprościło się "do końca", a za zmienną podstawwiamy tylko Int lub Bool*)
       | _ -> Let (x, e1', cp_helper env e2))
  ;;

  (* funkcja cp: *)
  let cp : expr -> expr = cp_helper M.empty
  
  let eval : expr -> value = eval_env M.empty
  let interp (s : string) : value = eval (parse s)
end

(* działa *)
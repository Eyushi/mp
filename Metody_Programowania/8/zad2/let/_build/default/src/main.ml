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

  let eval : expr -> value = eval_env M.empty
  let interp (s : string) : value = eval (parse s)
end

(* sposób -  ~1 (jedno środowisko i w nim pary) *)
(* ważne important -- na pracownię (1/3 pracowni !!) *)

type env = (ident * ident) list

let empty_env : env = []
let extend_env (x, y) env = (x, y) :: env

let rec lookup_env x = function
  | [] -> x
  | (a, b) :: ys -> if x = a then b else lookup_env x ys
;;

let rec inverse_lookup env y = function
  | [] -> y
  | (a, b) :: ys -> if y = b then a else inverse_lookup env y ys
;;

let rec alpha_equiv env1 env2 e1 e2 =
  match e1, e2 with
  | Int x, Int y -> x = y
  | Bool x, Bool y -> x = y
  | Var x, Var y ->
    let wrapped_x = lookup_env x env1 in
    let wrapped_y = lookup_env y env2 in
    wrapped_x = y && wrapped_y = x
  | Binop (op1, a1, b1), Binop (op2, a2, b2) ->
    op1 = op2 && alpha_equiv env1 env2 a1 a2 && op1 = op2 && alpha_equiv env1 env2 b1 b2
  | Let (x1, e1l, e1r), Let (x2, e2l, e2r) ->
    alpha_equiv env1 env2 e1l e2l
    && alpha_equiv (extend_env (x1, x2) env1) (extend_env (x2, x1) env2) e1r e2r
    (* alpha_equiv env1 env2 e1l e2l && alpha_equiv env1 env2 e2r e2r *)
  | _ -> failwith "false"
;;

let test e1 e2 = alpha_equiv empty_env empty_env e1 e2
(* zadanie 4 *)

(* wszystko w parserze? *)
(* w ast?? *)
%
%

let rec make_fun xs e=
match xs with
|[] -> e
|x ::y -> Fun(x, make_fun y e)





mixfix :
| FUN; xs = idents; ARR; e = mixfix {
  make_fun xs e  
}



idents:
| x = IDENTS { [x]}
| x = IDENTS; xs = idents {x :: xs}










(* zadanie 5 *)

(* ast *)
bez tego
(* type expr = *)
(* |LetRec of ident * ident * expr * expr *)


(* lekser *)
"rec" REC



(* w parserze: *)
token

(* lukier syntaktyczny -- rozszerzamy TYLKO PARSER !! *)
base:
(* ŻLE *)
| LETREC; f = IDENT; x = IDENT; EQ; e1 = expr; IN; e2 = expr {LetRec(f,x,e1,e2)}

(* np
let fib = fix (fun fib -> fun x -> if x <= 1 then x else fib (n-1) + fib (n - 2))
in fib 10


let rec fib x = if x <= 1 then x else fib (n-1) + fib (n - 2)) in fib 10

*)


(* let rec f x = e1 in e2 
   --->
    let f = fix (fun f -> fun x -> e1) in e2
    
    f --> f
    x --> x
    e1 --> e1
    e2 --> e2*)


(* POPRAWIONE: -- tylko parser i lexer *)
| LETREC; f = IDENT; x = IDENT; EQ; e1 = expr; IN; e2 = expr {
  Let(f, App(Var "fix", Fun(f, Fun (x, e))), e2)
  }

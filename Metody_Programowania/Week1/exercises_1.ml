(* Exercise: values [★] *)

7 * (1 + 2 + 3);;

(*type int 42*)

"CS " ^ string_of_int 3110;;

(*type str "CS 3110" *)

(* Notes: ^  -- concatenation of str*)

(* Exercise: operators [★★] *)

42 * 10;;
3.14 /. 2.0

let x = 4
let y = 7
let rec power x y = if y = 1 then x else x * power x (y - 1);;

power x y;;

(* Exercise: equality [★] *)
42 = 42;;
"hi" = "hi";;

(* - : bool = true *)

"hi" == "hi";;

(* - : bool = false *)

(*Notes:
  = and <> examine structural equality
  == and != examine physical equality *)

(* Exercise: assert [★] *)

assert true;;

(* - : unit = () *)

assert false

(* Exception: Assert_failure ("//toplevel//", 1, 0). *)

let () = assert (2110 <> 3110);;

(* nothing *)

(* Exercise: if [★] *)

if 2 > 1 then 42 else 7

(* Exercise: double fun [★] *)

let double x = x * 2

(* Exercise: more fun [★★] *)

let cube x = x *. x
let sign x = if x = 0 then 0 else if x > 0 then 1 else -1;;

(* Uwaga !  -- Liczby ujemne zapisywać w nawiasie *)
sign (-3)

(* zamiast: sign -3* -- nie działa*)

let circle rad = 3.14 *. rad *. rad
let () = assert (circle 2. = 12.56)
let () = assert (circle 3. = 1.)

(* Exception: Assert_failure ("//toplevel//", 1, 9). *)

(* Exercise: RMS [★★] *)

let rms x y = sqrt (((x *. x) +. (y *. y)) /. 2.)
let () = assert (rms 2. 4. = 3.16227766016837952)
let () = assert (rms 2. 4. = 3.)

(* Exception: Assert_failure ("//toplevel//", 1, 9). *)

let valid_date d m =
  if ((m = "Jan"
       || m = "Mar"
       || m = " May"
       || m = "Jul"
       || m = "Aug"
       || m = "Oct"
       || m = "Dec")
      && d > 0
      && d <= 31)
     || ((m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov") && d > 0 && d <= 30)
     || (m = "Feb" && d > 0 && d <= 28)
  then true
  else false
;;

(* let valid_date d m =
   if (m = "Jan" || m = "Mar" || m = " May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec") && d > 0 && d <= 31
   || ((m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov") && d > 0 && d <= 30)
   || (m = "Feb" && d > 0 && d <= 28)
   then true
   else false
   ;; *)

(* in case of formatting *)

(* Exercise: fib [★★] *)

let rec fib n = if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2)

(* Shift Enter -- writes the line of code into the terminal *)

(* Exercise: fib fast [★★★] *)

(* todo *)

(* Exercise: poly types [★★★] *)

let f x = if x then x else x
(* val f : bool -> bool = <fun> *)

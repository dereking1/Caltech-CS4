open Num

(* A.1 *)
(* The space complexity is O(n). With applicative order evaluation, the operands are evaluated
   before the operator is applied. Therefore, when we evaluate fib(n-1) + fib(n-2), the left
   operand gets fully evaluated first before evaluating the right operand and the operator.
   Thus, the stack does not expand to 2^n stacks as we just perform a depth-first search rather
   than a breadth-first search.*)
  
(* A.2 *)
(* 1. The function p is called 5 times for sine 12.15. *)
(* 2. The order of growth in space and number of steps is both O(log(a)). The number of recursive
   calls is log_3 (10a) and the function is called log_3 (10a) times, and each function call
   performs O(1) work. *)

(* A.3a *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
    | 0 -> 1
    | _ when is_even n -> square (fast_expt b (n / 2))
    | _ -> b * fast_expt b (n - 1)

(* A.3b *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter_helper a b n = 
      match n with
      | 0 -> a
      | _ when is_even n -> iter_helper a (square b) (n / 2)
      | _ -> iter_helper (a * b) b (n - 1)
  in iter_helper 1 b n

(* A.4 *)
let rec fast_mult b n =
  let double x = x + x in
  let halve x = x / 2 in
  let is_even x = x mod 2 = 0 in
    match n with
    | 0 -> 0
    | _ when is_even n -> double (fast_mult b (halve n))
    | _ -> b + fast_mult b (n - 1)

(* A.5 *)
let ifast_mult b n=
  let double x = x + x in
  let halve x = x / 2 in
  let is_even x = x mod 2 = 0 in
  let rec iter_helper a b n =
      match b with
      | 0 -> a
      | _ when is_even n -> iter_helper a (double b) (halve n)
      | _ -> iter_helper (a + b) b (n - 1)
  in iter_helper 0 b n
  
(* A.6 *)
(* The worst case time complexity is O(n) and the worst space complexity is O(log(n)).
   The function is a recursive process with a tree that has log(n) depth. Therefore,
   the space complexity is O(log(n)). The time complexity corresponds to the number of 
   function calls which is O(2^(log(n))) = O(n). *)
  
(* A.7 *)
(* 1. This function is a linear recursive process because the function makes one recursive
   call so the total number of calls is n+1. *)
(* 2. The time complexity is O(n) and the space complexity is O(n). This is because there are
   O(n) function calls and the stack has at most n pending operations. *)

(* B.1a *)
(* (fun x y -> x * (2 + y)) 20 (2 * 4) *)

(* B.1b *)
(* (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0 *)

(* B.1c *)
(* (fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1 *)

(* B.1d*)
(* (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1 *)

(* B.2 *)
(*
   Desugaring: 
(fun x y -> (fun y -> (fun z -> (x * y * z) 22) 14) (2 * 10) (3 + 4))
  Evaluate fun z -> x * y * z 22 
    Evaluate 22 -> 22 
    Apply fun z -> x * y * z to 22
    Substitute 22 for z in body -> x * y * 22
  Evaluate fun y -> x * y * 22 14
    Evaluate 14 -> 14
    Apply fun y -> x * y * 22 to 14
    Substitute 14 for y in body -> x * 14 * 22
    Evaluate * -> *
    Apply * 14, 22 -> x * 308
  Evaluate fun x y -> x * 308 to (2 * 10) (3 + 4)
    Evaluate 2 -> 2
    Evaluate 10 -> 10
    Evaluate 3 -> 3
    Evaluate 4 -> 4
    Apply * to 2, 10 -> 20
    Apply + to 3, 4 -> 7
    Apply fun x y -> x * 308 to 20, 7
    Substitute 20 for x in body -> 20 * 308
      Evaluate * -> *
      Evaluate 20 -> 20 
      Evaluate 308 -> 308 
      Apply * to 20, 308 
  Result: 6160
*)

(* B.3 *)
(* (fun x y z -> x + y + z) 10 (x * 2) (y + 3) *)
(* The "and" implies that none of the expressions depend on the binding of the let statements.
The operands 10, x * 2, and y + 3 are evaluated first before binding x, y, z to these expressions.
Ben can fix this with nested let statements:

let x = 10 
in let y = x * 2
in let z = y + 3
in x + y + z *)

(* C *)
let ni = num_of_int

(* C.1 *)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (result +/ (term a))
  in iter a (ni 0)

(* C.2 *)
let rec product_rec term a next b =
  if a >/ b
     then (ni 1)
     else term a */ (product_rec term (next a) next b)
  
let factorial_rec n = product_rec (fun x -> x) (ni 1) (fun x -> x +/ (ni 1)) n

let product_iter term a next b =
  let rec iter a result =
    if a >/ b
      then result
      else iter (next a) (result */ (term a))
  in iter a (ni 1)

let factorial_iter n =  product_iter (fun x -> x) (ni 1) (fun x -> x +/ (ni 1)) n

let pi_product n = 
  let pi_top x = x +/ (ni 2) -/ (mod_num x (ni 2)) in
	let pi_bot x = x +/ (ni 2) -/ (mod_num (x +/ (ni 1)) (ni 2)) in
  let next x = x +/ (ni 1) in
  let top = product_rec pi_top (ni 1) next n in
  let bot = product_rec pi_bot (ni 1) next n in
    ((ni 4) */ top) // bot

let pi_approx = float_of_num (pi_product (ni 2000))

(* C.3 *)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
    then null_value
    else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a >/ b
      then result
      else iter (next a) (combiner result (term a))
    in iter a null_value

let sum term a next b = accumulate_rec ( +/ ) (ni 0) term a next b
let product term a next b = accumulate_iter ( */ ) (ni 1) term a next b

(* C.4 *)
let compose f g x = f (g (x))

(* C.5 *)
let rec repeated f n =
  if n = 0
    then fun x -> x
    else compose f (repeated f (n - 1))

(* C.6 *)
let smooth dx f =
  fun x -> (f (x +. dx) +. f (x) +. f (x -. dx)) /. 3.0

let nsmoothed dx f n = 
  (repeated (smooth dx) n) f

(* D.1 *)
let is_prime n = 
  let sqrt_n = int_of_float (sqrt (float_of_int n)) in
  let rec helper i = 
    match i with 
      | _ when n < 2 -> false
      | _ when i > sqrt_n -> true
      | _ when n mod i = 0 -> false
      | _ -> helper (i + 1)
  in helper 2

(* D.2 *)
let smallest_prime_factor n =
  let rec helper i =
    if is_prime n || n < 2 then failwith "invalid_arg"
    else if n mod i = 0 then i
    else  helper (i + 1)
  in helper 2

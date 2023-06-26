(* A.1 *)

(* 1. int = 10 *)
(* 2. float = 10.*)
(* 3. int = 12*)
(* 4. Error: This expression has type float but an expression was expected of type int. 
   To add floats, we need to put a . after the + sign (+.)*)
(* 5. Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `3.'?
  To use (+.), 3 and 4 should be written as 3. and 4.*)
(* 6. Error: This expression has type float but an expression was expected of type
         int
    We are adding floats so 3 and + should be 3. and +.*)
(* 7. Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `3.'?
  We are adding floats so 3 should be 3.*)
(*8. float = 7.2*)
(*9. int = 5*)
(*10. int = 7*)
(*11. val a : int = 3*)
(*12. val b : int = 4*)
(*13. bool = false*)
(*14. bool = true*)
(*15. bool = false. This is different from the expression in 14. because
  we use a == instead of =. The == operator checks if the two objects are
  the same in memory while = checks if the two objects have the same value.*)
(*16. (int * int * int) list = [(1, 2, 3)]*)
(*17. (int * int * int) list = [(1, 2, 3)]. This gives the same result as the expression
in 16. Both (1, 2, 3) and 1, 2, 3 are tuples because OCaml allows you to omit parentheses
in this situation.*)
(*18. int = 4*)
(*19. Error: Syntax error. You have to use && instead of 'and', which is not valid syntax.*)
(*20. int = 6*)
(*21. int = 4. This is different from the previous case because the +2 is before the if statement
in the last case and in this case it is in a+2. The previous case will output 2 + b if b > a else 2 + a.
This case will output b if b > a else it will output a+2.*)
(*22. int = 6*)
(*23. Error: This expression has type int but an expression was expected of type 
       unit because it is in the result of a conditional with no else branch.
       We have an if clause without else, so the then clause must have type unit.
       We have type int with the then clause.*)

(* A.2 *)
let sum_of_squares_of_two_largest a b c =
if a < c && a < b then 
  b*b + c*c 
else if b < a && b < c then
  a*a + c*c
else a*a + b*b;;

(* A.3 *)
let a_plus_abs_b a b =
  (if b > 0 then (+) else (-)) a b;;
(* This function returns the sum of a and the absolute value of b.
   It first checks if b > 0, so if b > 0 then the absolute value of b is b
   so we can use the + operator. If b <= 0 then the absolute value of b
   is -b so we use can just use the - operator.*)

(* B.1 *)
(* If Ben uses an interpreter that uses applicative order evaluation, the program will recurse infinitely.
   This is because applicative order will evaluate the arguments first (0 and p ()) and then the function test.
   The argument p () will recurse infinitely so the program will recurse infinitely. If Ben uses an interpreter
   that uses normal order evaluation, the program will return 0. This is because the function is evaluated first,
   so the interpreter will first check if x = 0. If x = 0, then it will return 0, otherwise it will return y. We test
   x = 0 and y = p () and since x = 0, the interpreter will never evaluate p () and just return 0 instead.*)
  
(* B.2 *)
(* The program will recurse infinitely when Alyssa attempts to use this to compute square roots.
   This is because in applicative order evaluation, the arguments in the 'new_if' function
   will be evaluated first before matching the predicate to true or false. Alyssa attemps to recursively compute
   the square root, where 'sqrt_iter' calls 'new_if', and one of the arguments to 'new_if' is 
   'sqrt_iter'. Therefore, when 'sqrt_iter' is called, 'sqrt_iter' will be evaluated recursively before
   applying 'new_if'. Thus, the program will keep recursing on 'sqrt_iter' and recurse forever.*)
  
(* B.3 *)
(*1. add_a generates a recursive process and add_b generates an iterative process. add_a calls inc first
      and then add_a again. This builds up a chain of deferred operations (1 + 1 + ... 1 + b), which defines
      a recursive process. add_b calls add_b (dec a) (inc b) recursively, where (dec a) and (inc b) are evaluated
      first. We can keep track of the state of the process with the variables a and b, which would look like
      add_b a b, add_b a-1 b+1, .... add_b a-a b+a. Therefore, this is an iterative process.*)
(*2.
let rec add_a a b =
  if a = 0
     then b
     else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Evaluate (add_a 2 5)
  evaluate 2 -> 2
  evaluate 5 -> 5
  evaluate add_a -> fun a b -> if a = 0 then b else inc (add_a (dec a) b)
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else inc (add_a (dec 2) 5)
  evaluate (if 2 = 0 then 5 else inc (add_a (dec 2) 5)
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        evaluate 2 -> 2
        evaluate 0 -> 0
        evaluate = -> =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate inc (add_a (dec 2) 5)
        evaluate add_a (dec 2) 5
          evaluate (dec 2)
            evaluate 2 -> 2
            evaluate dec -> dec
            apply dec to 2 -> 1
          evaluate 5 -> 5
          evaluate add_a -> fun a b -> ...
          apply (fun a b -> if ...) to 1, 5
          substitute 1 for a, 5 for b in (if ...)
            -> if 1 = 0 then 5 else inc (add_a (dec 1) 5)
          evaluate (if 1 = 0 then 5 else inc (add_a (dec 0) 5))
            if is a special form, so evaluate the first operand:
              evaluate (1 = 0)
                evaluate 1 -> 1
                evaluate 0 -> 0
                evaluate = -> =
                apply = to 1, 0 -> false
        first argument of if is false, so evaluate the third operand:
          evaluate inc (add_a (dec 1) 5)
            evaluate add_a (dec 1) 5
              evaluate (dec 1)
                evaluate 1 -> 1
                evaluate dec -> dec
                apply dec to 1 -> 0
              evaluate 5 -> 5
              evaluate add_a -> fun a b -> ... 
              apply (fun a b -> if ...) to 0, 5
              substitute 0 for a, 5 for b in (if ...)
                -> if 0 = 0 then 5 else inc (add_a (dec 0) 5)
              evaluate (if 0 = 0 then 5 else inc (add_a (dec 0) 5))
              if is a special form, so evaluate the first operand:
                evaluate (0 = 0)
                  evaluate 0 -> 0
                  evaluate 0 -> 0
                  evaluate = -> =
                  apply = to 0, 0 -> true
              first argument of if is true, so evaluate the second operand:
                evaluate 5 -> 5
            evaluate inc -> inc
            apply inc to 5 -> 6
        evaluate inc -> inc
        apply inc to 6 -> 7
        result : 7
*)
(* 3.
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
  >>> evaluate 2 -> 2
  >>> evaluate 5 -> 5
  >>> evaluate add_b -> fun a b -> if a = 0 then b else add_b (dec a) (inc b)
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        >>> evaluate 2 -> 2
        >>> evaluate 0 -> 0
        >>> evaluate = -> =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
          >>> evaluate 2 -> 2
          >>> evaluate dec -> dec
          apply dec to 2 -> 1
        evaluate (inc 5)
          >>> evaluate 5 -> 5
          >>> evaluate inc -> inc
          apply inc to 5 -> 6
        >>> evaluate add_b -> fun a b -> ...
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              >>> evaluate 1 -> 1
              >>> evaluate 0 -> 0
              >>> evaluate = -> =
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
                >>> evaluate 1 -> 1
                >>> evaluate dec -> dec
                apply dec to 1 -> 0
              evaluate (inc 6)
                >>> evaluate 6 -> 6
                >>> evaluate inc -> inc
                apply inc to 6 -> 7
              >>> evaluate add_b -> fun a b -> ...
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                    >>> evaluate 0 -> 0
                    >>> evaluate 0 -> 0
                    >>> evaluate = -> =
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                  evaluate 7 -> 7
                  result: 7
*)

(* C.1 *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1);;
(* C.1a *)
let e_term n = 
  1. /. float_of_int (factorial n);;
(* C.1b *)
let rec e_approximation n = 
  if n = 0 then e_term 0
  else e_term n +. e_approximation (n - 1);;
(* C.1c*)
(* e_approximation 20: float = 2.71828182845904553
   exp 1.0: float = 2.71828182845904509 
   The results are very similar.*)
(* C.1d*)
(* e_approximation 100: float = infinity
   This happens because 100! causes an integer overflow, so it defaults 
   to 0. Therefore, the e terms will become infinity if n gets too large
   so the sum of e terms becomes infinity.*)

(* C.2 *)
let rec is_even n = 
  if n = 0 then true
  else is_odd (n - 1)
  and is_odd n =
  if n = 0 then false
  else is_even (n - 1);;

(* C.3 *)
let rec f_rec n = 
  if n < 3 then n
  else f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec (n - 3);;

let rec f_iter n =
  let rec helper x y z count n = 
    if count > n then x
    else helper (x + 2*y + 3*z) x y (count+1) n
  in if n < 3 then n
  else helper 2 1 0 3 n;;

(* C.4 *)
let rec pascal_coefficient n i = 
  match (n, i) with
  | (n2, i2) when n2 < 1 || i2 < 1 || i2 > n2 -> failwith "invalid arguments"
  | (n2, i2) when n2 = i2 -> 1
  | (_, 1) -> 1
  | (_, _) -> pascal_coefficient (n - 1) (i - 1) + pascal_coefficient (n - 1) i;;

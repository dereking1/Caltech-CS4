type point = { x : float; y : float }
type segment = { startp : point; endp : point }
(* A.1 *)
let midpoint_segment {startp ; endp} =
  let x = (startp.x +. endp.x) /. 2.0 in
  let y = (startp.y +. endp.y) /. 2.0 in
  { x; y }

let segment_length {startp ; endp} = 
  let dx = (endp.x -. startp.x) ** 2.0 in
  let dy = (endp.y -. startp.y) ** 2.0 in
  sqrt (dx +. dy)

let print_point {x ; y} =
  Printf.printf "(%g, %g)" x y

let make_point x y =
  {x ; y}

let get_coords { x; y} =
  (x, y)

let make_segment startp endp =
  {startp; endp}

let get_points {startp ; endp} =
  (startp, endp)

(* A.2 *)
type rectangle = { ll : point; ur : point }

let rectangle_lower_segment {ll ; ur} = 
  let lr = make_point ur.x ll.y in
  make_segment ll lr

let rectangle_upper_segment {ll ; ur} = 
  let ul = make_point ll.x ur.y in
  make_segment ul ur

let rectangle_left_segment {ll ; ur} =
  let ul = make_point ll.x ur.y in
  make_segment ll ul

let rectangle_right_segment {ll ; ur} = 
  let lr = make_point ur.x ll.y in
  make_segment lr ur

let rectangle_perimeter rectangle = 
  let base = segment_length (rectangle_lower_segment rectangle) in
  let height = segment_length (rectangle_left_segment rectangle) in
  2.0 *. (base +. height)

let rectangle_area rectangle = 
  let base = segment_length (rectangle_lower_segment rectangle) in
  let height = segment_length (rectangle_left_segment rectangle) in
  base *. height

let make_rectangle ll ur = 
  { ll = ll ; ur = ur }

type rectangle2 = { lx : float; ly : float ;  ux : float ; uy: float }
  
let rectangle_lower_segment2 {lx ; ly ; ux ; uy} = 
  let ll = make_point lx ly in 
  let lr = make_point ux ly in
  make_segment ll lr

let rectangle_upper_segment2 {lx ; ly ; ux ; uy} =
  let ul = make_point lx uy in
  let ur = make_point ux uy in
  make_segment ul ur

let rectangle_left_segment2 {lx ; ly ; ux ; uy} =
  let ll = make_point lx ly in
  let ul = make_point lx uy in
  make_segment ll ul

let rectangle_right_segment2 {lx ; ly ; ux ; uy} =
  let lr = make_point ux ly in
  let ur = make_point ux uy in
  make_segment lr ur

let rectangle_perimeter2 rectangle2 = 
  let base = segment_length (rectangle_lower_segment2 rectangle2) in
  let height = segment_length (rectangle_left_segment2 rectangle2) in
  2.0 *. (base +. height)

let rectangle_area2 rectangle2 =
  let base = segment_length (rectangle_lower_segment2 rectangle2) in
  let height = segment_length (rectangle_left_segment2 rectangle2) in
  base *. height

let make_rectangle2 lx ly ux uy = 
  { lx = lx ; ly = ly ; ux = ux ; uy = uy }

(* A.3 *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(* 
1. first (make_pair x y) => first (fun m -> m x y)
first (fun m -> m x y) => (fun m -> m x y) (fun x y -> x)
(fun m -> m x y) (fun x y -> x) => (fun x y -> x) x y
(fun x y -> x) x y => x

2. second (make_pair 1 2)
let make_pair x y = fun m -> m x y
Desugar this to:
let make_pair x y = fun x y -> (fun m -> m x y)
Bind the name "make_pair" to the value:
  fun x y -> (fun m -> m x y)
let second z = z (fun x y -> y)
Desugar this to:
let second z = fun z -> (z (fun x y -> y))
Bind the name "second" to the value:
  fun z -> (z (fun x y -> y))

Evaluate second (make_pair 1 2)
  evaluate (make_pair 1 2)
    evaluate 1 -> 1
    evaluate 2 -> 2
    evaluate make_pair -> fun x y -> (fun m -> m x y)
    apply fun x y -> (fun m -> m x y) to 1, 2
      substitute 1 for x, 2 for y in fun m -> m x y
        -> (fun m -> m 1 2)
  evaluate second -> fun z -> (z (fun x y -> y))
  apply fun z -> (z (fun x y -> y)) to (fun m -> m 1 2)
    substitute (fun m -> m 1 2) for z in (z (fun x y -> y))
      -> (fun m -> m 1 2) (fun x y -> y)
      evaluate (fun m -> m 1 2) (fun x y -> y)
        evaluate (fun x y -> y) -> (fun x y -> y)
        evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
        apply (fun m -> m 1 2) to (fun x y -> y)
          substitute (fun x y -> y) for m in (m 1 2)
            -> ((fun x y -> y) 1 2)
            evaluate (fun x y -> y) 1 2
              evaluate 1 -> 1
              evaluate 2 -> 2
              evaluate (fun x y -> y) -> (fun x y -> y)
              apply (fun x y -> y) to 1, 2
                substitute 1 for x, 2 for y in (y)
                  -> 2
                result: 2
*)

(* A.4 *)
let rec pow a b =
  if b = 0 then 1
  else if b mod 2 = 0 then (pow (a*a) (b/2))
  else a * pow a (b - 1)

let int_log base x =
  let rec iter base x count = 
    match x with
      | x when x mod base = 0 -> iter base (x / base) (count + 1)
      | _ -> count
  in iter base x 0

let make_pairi a b =
  (pow 2 a) * (pow 3 b)

let firsti pair =
  int_log 2 pair

let secondi pair =
int_log 3 pair

(* A.5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev = function
  | [] -> invalid_arg "Argument must be greater than unary zero"
  | () :: x -> x

let rec integer_to_unary n =
  if n = 0 then zero
  else succ (integer_to_unary (n - 1))

let rec unary_to_integer u =
  if is_zero u then 0
  else 1 + unary_to_integer (prev u)

let unary_add x y = 
  integer_to_unary ((unary_to_integer x) + (unary_to_integer y))

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' = function
  | Zero -> invalid_arg "Argument must be greater than unary zero"
  | Succ u -> u

(* The functions integer_to_unary, unary_to_integer, unary_add do not have to change
   besides changing the names. *)

let rec integer_to_unary' n =
  if n = 0 then zero'
  else succ' (integer_to_unary' (n - 1))

let rec unary_to_integer' u =
  if is_zero' u then 0
  else 1 + unary_to_integer' (prev' u)

let unary_add' x y = 
  integer_to_unary' ((unary_to_integer' x) + (unary_to_integer' y))

(* A.6 *)

(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. *)

let zerof = fun s -> fun z -> z
   (* or equivalently: let zerof = fun s z -> z *)
   (* or equivalently: let zerof s z = z *)
 
let add1 n = fun s -> fun z -> s (n s z)
  (* or equivalently: let add1 n = fun s z -> s (n s z) *)
  (* or equivalently: let add1 n s z = s (n s z) *)

let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = (m s) ((n s) z)

let church_to_integer n = n (fun z -> z + 1) 0

(* A.7 *)
(*
  val zerof : 'a -> 'b -> 'b
  Desugar to : 'a -> ('b -> 'b)

  val one : ('a -> 'b) -> 'a -> 'b
  Desugar to : ('a -> 'b) -> ('a -> 'b)

  val church_to_integer : ((int -> int) -> int -> 'c) -> 'c
  Desugar to : ((int -> int) -> (int -> 'c)) -> 'c

  When we evaluate church_to_integer zerof, (int -> int) comes from fun z -> z + 1 
  and the (int -> 'c) comes from (n (fun ...)) 0. n is the zerof function 
  and it has type of 'a -> ('b -> 'b) so (n (fun ...)) has type (int -> int) -> ('b -> 'b)
  and (n (fun ...)) 0 must have type (int -> int). Therefore, church_to_integer zerof 
  must have type ((int -> int) -> (int -> int)) -> int. 

  When we evaluate church_to_integer one, (int -> int) comes from fun z -> z + 1 
  and the (int -> 'c) comes from (n (fun ...)) 0. Similarly, n is the 
  one function and it has type of ('a -> 'b) -> ('a -> 'b), then 
  (n (fun z -> z + 1)) has type (int -> int) -> (int -> int).
  Therefore, church_to_integer one also has type int. 
*)

(* B.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* B.1a *)
let left_branch = function
  | Mobile (l, r) -> l

let right_branch = function
  | Mobile (l, r) -> r

let branch_length = function
  | Weight (l, w) -> l
  | Structure (l, m) -> l

let branch_structure = function
  | Weight (l, w) -> `Weight w
  | Structure (l, m) -> `Structure m

(* B.1b *)
let rec branch_weight1 = function
  | Weight (l, w) -> w
  | Structure (l, m) -> total_weight1 m
  and total_weight1 = function 
    | Mobile (l, r) -> (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 branch =
  match branch_structure branch with
    | `Weight w -> w
    | `Structure m -> total_weight2 m
  and total_weight2 m = 
    (branch_weight2 (left_branch m)) + (branch_weight2 (right_branch m))

(* B.1c *)
let rec is_balanced mobile =
  let is_branch_balanced branch = 
    match branch_structure branch with 
      | `Weight w -> true
      | `Structure m -> is_balanced m in
  let left = left_branch mobile in
  let right = right_branch mobile in
    if (branch_weight2 left) * (branch_length left) == (branch_weight2 right) * (branch_length right) 
      then (is_branch_balanced left) && (is_branch_balanced right)
    else false

(* B.1d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = { left = left ; right = right }

let make_weight' l w = Branch' (l, (Weight' w))

let make_structure' l m = Branch' (l, (Structure' m))

let left_branch' { left; right } = left

let right_branch' { left; right } = right

let branch_length' (Branch' (l, m)) = l

let branch_structure' (Branch' (l, m)) = 
    match m with 
      | Weight' w -> `Weight w
      | Structure' s -> `Structure s

let rec branch_weight' branch =
  match branch_structure' branch with
    | `Weight w -> w
    | `Structure m -> total_weight' m
  and total_weight' m = 
    (branch_weight' (left_branch' m)) + (branch_weight' (right_branch' m))

let rec is_balanced' mobile =
  let is_branch_balanced branch = 
    match branch_structure' branch with 
      | `Weight w -> true
      | `Structure m -> is_balanced' m in
  let left = left_branch' mobile in
  let right = right_branch' mobile in
    if (branch_weight' left) * (branch_length' left) == (branch_weight' right) * (branch_length' right) 
      then (is_branch_balanced left) && (is_branch_balanced right)
    else false

(* B.2 *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree (Tree t) = 
  let rec square = function
    | [] -> []
    | Sub x1 :: x -> Sub (square_tree x1) :: (square x)
    | Num x1 :: x -> Num (x1 * x1) :: (square x)
  in Tree (square t)

let rec square_tree' (Tree t) =
  let square = function 
    | Num x -> Num (x * x)
    | Sub x -> Sub (square_tree' x)
  in Tree (List.map square t)

(* B.3 *)
let rec tree_map f (Tree t) =
  let func = function 
    | Num x -> Num (f x)
    | Sub x -> Sub (tree_map f x)
  in Tree (List.map func t)

(* C.1 *)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

let rec simplify1 = function 
  | Int x -> Int x
  | Var x -> Var x
  | Add (Int x, Int y) -> Int (x + y)
  | Add (x, Int 0) | Add (Int 0, x) -> x
  | Add (x, y) -> Add (simplify x, simplify y)
  | Mul (Int x, Int y) -> Int (x * y)
  | Mul (x, Int 0) | Mul (Int 0, x) -> Int 0
  | Mul (x, Int 1) | Mul (Int 1, x) -> x
  | Mul (x, y) -> Mul (simplify x, simplify y)
  | Pow (x, 0) -> Int 1
  | Pow (x, 1) -> x
  | Pow (Int x, y) -> Int (pow x y)
  | Pow (x, y) -> Pow (simplify x, y)
and simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

(* C.2 *)
let rec derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d
  and deriv var = function
  | Int x -> Int 0
  | Var x when x = var -> Int 1
  | Var _ -> Int 0
  | Add (x, y) -> Add ((deriv var x), (deriv var y))
  | Mul (x, y) -> Add(Mul((deriv var x), y), Mul(x, (deriv var y)))
  | Pow (x, y) -> Mul(Mul(Int y, Pow (x, y - 1)), deriv var x)

(* A.1 *)

(*
FRAME 0 (initial environment)
  parent: none
  bindings:
    - : [primitive function -]
    * : [primitive function *]

FUNCTION 0 (fun n -> let rec iter m r = ...)
  env: FRAME 0
  param: n
  body: let rec iter m r = ...

FRAME 1 (let factorial = FUNCTION 0 in ...)
  parent: FRAME 0
  bindings:
    factorial: FUNCTION 0

FRAME 2 (FUNCTION 0 applied to 3)
  parent: FRAME 1
  bindings:
    n : 3
  
FRAME 3 (let rec iter m r = if m = 0 ...)
  parent: FRAME 2
  bindings:
    iter: FUNCTION 1

FUNCTION 1 (fun m r -> if m = 0 ...)
  env: FRAME 3
  param: m r
  body: if m = 0 ...

FRAME 4 (FUNCTION 1 applied to 3, 1)
  parent: FRAME 3
  bindings:
    m : 3
    r : 1

FRAME 5 (FUNCTION 1 applied to 2, 3)
  parent: FRAME 3
  bindings:
    m : 2
    r : 3

FRAME 6 (FUNCTION 1 applied to 1, 6)
  parent: FRAME 3
  bindings:
    m: 1
    r: 6

FRAME 7 (FUNCTION 1 applied to 0, 6)
  parent: FRAME 3
  bindings:
    m : 0
    r : 6
*)

(* A.2 *)
let factorial =
  let f = ref (fun n -> 0) in
  let helper n = 
    if n = 0 then 1
    else n * !f (n - 1) in
  begin
    f := helper;
  end;
  !f

(* B.1 *)
exception Stat_error of string

let make_stat_1 () = 
  let sum = ref 0. in
  let sumsq = ref 0. in
  let n = ref 0. in
  object
  method append x = 
    sum := !sum +. x;
    sumsq := !sumsq +. (x *. x);
    n := !n +. 1.
  method clear = 
    sum := 0.;
    sumsq := 0.;
    n := 0.
  method mean = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for mean")
    else !sum /. !n
  method variance = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for variance")
    else (!sumsq -. (!sum *. !sum /. !n)) /. !n
  method stdev = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for stdev")
    else sqrt ((!sumsq -. (!sum *. !sum /. !n)) /. !n)
  end

(* B.2 *)
let make_stat_2 () = 
  let sum = ref 0. in
  let sumsq = ref 0. in
  let n = ref 0. in
  let _variance n = 
    (!sumsq -. (!sum *. !sum /. !n)) /. !n in
  object
  method append a = 
    sum := !sum +. a;
    sumsq := !sumsq +. (a *. a);
    n := !n +. 1.
  method clear = 
    sum := 0.;
    sumsq := 0.;
    n := 0.
  method mean = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for mean")
    else !sum /. !n
  method variance = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for variance")
    else _variance n
  method stdev = 
    if !n = 0. then raise 
      (Stat_error "need at least one value for stdev")
    else sqrt (_variance n)
  end

(* C.1 *)

(* Signature for priority queues. *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)
    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty
    type elem = int
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf
    let is_empty t = (t = Leaf)

    let rank n = 
      match n with
        | Leaf -> 0
        | Node (r, _, _, _) -> r 

    let rec merge q1 q2 =
      let helper q1' q2' min_elem =
        let rank1 = rank q1' in 
        let rank2 = rank q2' in
          if rank2 > rank1
            then Node (rank1 + 1, min_elem, q2', q1')
            else Node (rank2 + 1, min_elem, q1', q2')
      in
        match (q1, q2) with 
          | (Leaf, _) -> q2
          | (_, Leaf) -> q1
          | (Node (rank1, elem1, l1, r1), Node (rank2, elem2, l2, r2)) ->
              if elem1 < elem2 
                then helper l1 (merge r1 q2) elem1
                else helper l2 (merge r2 q1) elem2
    
    let insert q new_elem =
      merge q (Node (1, new_elem, Leaf, Leaf))
    let find_min q = 
      match q with
        | Leaf -> raise Empty
        | Node (rank, elem, l, r) -> elem
    let delete_min q = 
      match q with 
        | Leaf -> raise Empty
        | Node (rank, _, l, r) -> merge l r
    let rec from_list lst = 
      match lst with
        | [] -> Leaf
        | x1 :: x -> insert (from_list x) x1
  end

let heap_sort lst =
  let rec iter result q = 
    if PriorityQueue.is_empty q 
      then List.rev result
      else 
        let min = PriorityQueue.find_min q in
        let q' = PriorityQueue.delete_min q in
          iter (min :: result) (q')
    in
      iter [] (PriorityQueue.from_list lst)

(* C.2 *)

(* Signature for ordered objects. *)
module type ORDERED_TYPE =
  sig
    type t
    val compare: t -> t -> int
  end

  module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end
  
  module MakePriorityQueue (Elt : ORDERED_TYPE)
    : (PRIORITY_QUEUE with type elem = Elt.t) =
    struct
      exception Empty
  
      type elem = Elt.t
  
      (*
       * Data type: either
       * -- a Leaf, or
       * -- a Node of (rank, item, left heap, right heap).
       *)
      type t = Leaf | Node of int * elem * t * t

      let empty = Leaf
  
      let is_empty q = (q = empty)
      
      let rank = function
        | Leaf -> 0
        | Node (r, _, _, _) -> r
      
      let rec merge q1 q2 = 
        let iter q1' q2' min =
          let r1 = rank q1' in
          let r2 = rank q2' in
            if r1 > r2 
              then Node (r2 + 1, min, q1', q2')
              else Node (r1 + 1, min, q2', q1')
        in
        match (q1, q2) with
          | (Leaf, _) -> q2
          | (_, Leaf) -> q1
          | (Node (_, elem1, l1, r1), Node (_, elem2, l2, r2)) ->
            if elem1 > elem2 
              then iter l2 (merge r2 q1) elem2
              else iter l1 (merge r1 q2) elem1
      
      let insert q elem = merge q (Node (1, elem, empty, empty)) 
      
      let find_min = function
        | Leaf -> raise Empty
        | Node (_, min, _, _) -> min
  
      let delete_min = function
        | Leaf -> raise Empty
        | Node (_, _, l, r) -> merge l r
      
      let rec from_list = function
        | [] -> empty
        | (x1 :: x) -> insert (from_list x) x1
    end
  
  module StringPQ = MakePriorityQueue(OrderedString)
  
  let heap_sort_2 l =
    let q = StringPQ.from_list l in
    let rec helper pq sorted =
      if StringPQ.is_empty pq then sorted
      else helper (StringPQ.delete_min pq) 
           (StringPQ.find_min pq :: sorted)
    in
    List.rev (helper q [])

let heap_sort_2 lst =
  let rec iter result q = 
    if StringPQ.is_empty q 
      then List.rev result
      else 
        let min = StringPQ.find_min q in
        let q' = StringPQ.delete_min q in
          iter (min :: result) (q')
    in
      iter [] (StringPQ.from_list lst)

(* D.1 *)
type 'a contents = Eval of 'a | Uneval of (unit -> 'a)
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Uneval e)
let force lz = 
  match !lz with 
    | Eval v -> v
    | Uneval expr ->
        begin
          lz := Eval (expr ());
          expr ()
        end

(* D.2a *)
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

let almost_sum =
  fun f -> 
    fun lst ->
      match lst with
        | [] -> 0
        | x1 :: x -> x1 + (f x)
let sum = y almost_sum

(* D.2b *)
let factorial2 n = 
  let iter = 
    fun f ->
      fun (n, r) -> 
        if n = 0
          then r
          else f (n - 1, n * r)
  in y iter (n, 1)

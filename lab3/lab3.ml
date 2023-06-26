(* A.1 *)
let rec last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [n] -> [n]
  | x1 :: x -> last_sublist x

(* A.2 *)
let reverse list =
  let rec iter_helper curr reversed =
    match curr with
      | [] -> reversed
      | x1 :: x -> iter_helper x (x1 :: reversed)
  in iter_helper list []

(* A.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: (square_list t)

let square_list2 items = List.map (fun x -> x * x) items

(* A.4 *)
(*
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t ((h * h) :: answer)
  in iter items []

Defining square_list in this way produces the list in reverse order because when we match
things with h :: t, h is the first item in the list, and we add it to the front of answer.
Therefore, we will add each item to the front of the list in each call so the resulting list
will be reversed. We should be adding each item squared to the end of the answer to get the
order we want.
*)
(*
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer :: (h * h))
  in iter items []
  
This doesn't work because when we use the :: constructor, the element to the left of the ::
is supposed to be a single element (type a') and not a list (type a' list). The argument
answer is of type a' list so we get a type error. The :: constructor also expects
the right side of :: to be of type a' list, which h*h is not.

Fix: 

let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [h * h])
  in iter items []

The resulting function is not efficient because with the @ operator, each element in the list
is copied and then the element is appended, which is an O(n) operation. Therefore,
the function would be O(n^2) which is not efficient. 
*)

(* A.5 *)
let rec count_negative_numbers = function
  | [] -> 0
  | x1 :: x ->
    if x1 < 0
      then 1 + count_negative_numbers x
      else 0 + count_negative_numbers x

(* A.6 *)
let power_of_two_list n =
  let rec pow a b =
    match b with
      | 0 -> 1
      | b' when b' mod 2 = 0 -> pow (a * a) (b' / 2)
      | _ -> a * pow a (b - 1)
    in
  let rec iter_helper i result =
    if i < 0
      then result
      else iter_helper (i - 1) ((pow 2 i) :: result)
    in iter_helper (n - 1) []

(* A.7 *)
let prefix_sum list =
  let rec iter_helper curr pref_sum =
    match curr with
      | [] -> []
      | x1 :: x -> (x1 + pref_sum) :: (iter_helper x (x1 + pref_sum))
  in iter_helper list 0

(* A.8 *)
let deep_reverse list =
  let rec iter_helper curr reversed =
    match curr with
      | [] -> reversed
      | x1 :: x -> iter_helper x ((reverse x1) :: reversed)
  in iter_helper list []

(* A.9 *)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let rec deep_reverse_nested list = 
  match list with
    | Value list -> Value list
    | List list ->
      let rec iter_helper curr reversed =
        match curr with
          | [] -> reversed
          | x1 :: x -> iter_helper x ((deep_reverse_nested x1) :: reversed)
      in List (iter_helper list [])
  
let rec filter predicate sequence =
  match sequence with
    | [] -> []
    | h :: t when predicate h -> h :: filter predicate t
    | _ :: t -> filter predicate t

(* B.1 *)
let rec quicksort cmp list =
  match list with
    | [] -> []
    | x1 :: x ->
      let left = filter (fun a -> cmp a x1) x in
      let right = filter (fun a -> not (cmp a x1)) x in
        (quicksort cmp left) @ (x1 :: quicksort cmp right)

(* B.2 *)
(* The quicksort function is an instance of generative recursion and not structural recursion.
   This is because we create a new list each recursive call and recurse on the new list.
   Then, the newly created sorted lists are combined to generate the new sorted list. *)

(* B.3 *)
(* This doesn't work because there is no case for a list of length 1, which recurses
    infinitely. This is because the odd_half copies the same 1 length list a_list and recurses on it.
    Therefore, we will recurse infinitely. *)

(* B.4 *)
let rec insert_in_order cmp new_result a_list =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order cmp new_result t

let rec insertion_sort cmp a_list =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order cmp h (insertion_sort cmp t)

(* This is an example of structural recursion. This is because we recurse on
   subsets of the original list without creating new objects. For each call, 
   we use the first element in the list and then recurse on the remaining
  elements of the list, which is a natural subset of the original list. *)

(* C.1 *)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun x -> h :: x) rest)

(* This function works by first removing the first element of the list and then
   finding all the subsets of the remaining list (while adding the subsets found to
   the resulting list of subsets). Then, we add the first element back to all the 
   subsets we have found to generate all subsets including the first element. *)

(* C.2 *)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1 

let length sequence =
  accumulate (fun x l -> l + 1) 0 sequence

(* C.3 *)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)

(* C.4 *)
let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h1 :: t1, h2 :: t2) -> (f h1 h2) :: (map2 f t1 t2) 

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun x -> dot_product x v) m

let transpose mat = accumulate_n (fun x r -> x :: r) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (matrix_times_vector cols) m

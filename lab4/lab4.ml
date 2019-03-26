(* A.1.A *)
type mobile = Mobile of branch * branch  (* left and right branches *)
  and branch = 
    | Weight    of int * int     (* length and weight *)
    | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch m = 
	match m with
	| Mobile (l, r) -> l

let right_branch m =
	match m with
	| Mobile (l, r) -> r

let branch_length br =
	match br with
	| Weight (l, w) -> l
	| Structure (l, sub) -> l

let branch_structure br =
	match br with
	| Weight (l, w) -> `Weight w
	| Structure(l, sub) -> `Structure sub

let rec branch_weight1 br = 
	match br with
	| Weight (_, w) -> w
	| Structure (_, sub) -> total_weight1 sub
	

and total_weight1 mob =
	match mob with
	| Mobile (l, r) -> (branch_weight1 l) + (branch_weight1 r)


let rec branch_weight2 br = 
	match (branch_structure br) with
	| `Weight w -> w
	| `Structure sub -> total_weight2 sub
	

and total_weight2 mob =
	branch_weight2 (left_branch mob) + branch_weight2 (right_branch mob)


let rec is_balanced mob =
	let right = right_branch mob in
	let left = left_branch mob in
	if (branch_weight2 right) * branch_length right = branch_weight2 left
			* branch_length left then 
			match (branch_structure right, branch_structure left) with
			| (`Weight _, `Weight _) -> true
			| (`Structure sub1, `Structure sub2) -> is_balanced sub1 && 
													is_balanced sub2
			| (`Structure sub1, `Weight _) -> is_balanced sub1
			| (`Weight _, `Structure sub2) -> is_balanced sub2
	else false


type mobile'  = { left: branch'; right: branch' }
  and  branch'  = Branch' of int * contents
  and  contents = Weight' of int | Structure' of mobile'


let make_mobile' lef righ = {left = lef; right = righ}
let make_weight' len weight = Branch' (len, (Weight' weight))
let make_structure' len mob = Branch' (len, (Structure' mob))
let left_branch' {left = lef ; right = righ} = lef
let right_branch' {left = lef ; right = righ} = righ
let branch_length' (Branch' (len, struc)) = len

let branch_structure' (Branch' (len, struc)) =
    match struc with
    | Weight' weight -> `Weight weight
    | Structure' mob -> `Structure mob


let rec branch_weight' br =
    match (branch_structure' br) with
    | `Weight weight -> weight
    | `Structure mob -> total_weight' mob
and total_weight' mob =
    (branch_weight' (right_branch' mob)) + (branch_weight' (left_branch' mob))

let rec is_balanced' mob =
	let right = right_branch' mob in
	let left = left_branch' mob in
	if (branch_weight' right) * branch_length' right = branch_weight' left
			* branch_length' left then 
			match (branch_structure' right, branch_structure' left) with
			| (`Weight _, `Weight _) -> true
			| (`Structure sub1, `Structure sub2) -> is_balanced' sub1 && 
													is_balanced' sub2
			| (`Structure sub1, `Weight _) -> is_balanced' sub1
			| (`Weight _, `Structure sub2) -> is_balanced' sub2
	else false

	(* A.2 *)
type tree = Tree of elem list
and elem = 
	| Num of int
    | Sub of tree

let rec square_tree (Tree t) =
	let rec helper leaf =
		match leaf with
		| [] -> []
		| Num h :: t -> Num(h*h) :: (helper t)
		| Sub h :: t -> Sub(square_tree h) :: (helper t)
	in Tree (helper t)

let rec square_tree' (Tree t) =
	let helper leaf = 
		match leaf with
		| Sub sub -> Sub (square_tree' sub)
		| Num n -> Num(n * n)
	in Tree(List.map helper t)


let rec tree_map (Tree t) op =
	let helper leaf =
		match leaf with
		| Sub sub -> Sub (tree_map sub op)
		| Num n -> Num (op n)
	in Tree(List.map helper t)

let square_tree'' tree = tree_map tree (fun n -> n * n) 

(* A.4 *)
let rec subsets = function
    | [] -> [[]]
    | h :: t -> let rest = subsets t in
        rest @ (List.map (fun x -> h :: x) rest)

(*
This workd because at each step the list is broken down and subsets is recursively
called upon the tail of the list until the base case returns [[]], at which point
each subset created is appended to the original list whichh, since the original list
is also technically a subset of the list, makes the set the complete power set
*)

(* A.5 *)
let rec accumulate op initial sequence =
    match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
    accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
    accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
    accumulate (fun a x  -> x + 1) 0 sequence

(* A.6 *)

let rec accumulate_n op init seqs =
    match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []
    | h :: t -> accumulate op init (List.map List.hd seqs) :: accumulate_n 
    				op init (List.map List.tl seqs)

(* A.7 *)

let rec map2 f x y =
    match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h :: t, h' :: t') -> (f h h') :: (map2 f t t')

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun x -> dot_product v x) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

let matrix_times_matrix m n =
    let matrix_columns = transpose n in
    map (fun x -> matrix_times_vector matrix_columns x) m


(* B.1 *)
let rec quicksort l cmp =
	match l with
	| [] -> []
	| h::t -> (quicksort (List.filter (fun x-> cmp x h) t) cmp) @
				(h::(quicksort (List.filter (fun x -> not (cmp x h)) t) cmp))

(* B.2 *)
(* structural recursion just manipulates data rather than creating
new objects this process creats objects so it is genreative
*)

(* B.3 *)
(*
If we don't have the 1 element base case,
a list of size one will be repeatedly split into a list of 1 
and a list of 0 and this would happen again and again on the
newly created size 1 list.
*)


(* B.4 *)
let rec insert_in_order new_result a_list cmp =
    match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t -> h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
    match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t cmp) cmp


(* C *)
type expr =
    | Int of int           (* constant *)
    | Var of string        (* variable *)
    | Add of expr * expr   (* expr1 + expr2 *)
    | Mul of expr * expr   (* expr1 * expr2 *)
    | Pow of expr * int    (* expr^n *)


let rec pow a b =
    if b = 0 then 1
    else a * pow a (b-1)

let rec simplify1 exp =
    match exp with
    | Add (a, Int 0) -> a
    | Add (Int 0, b) -> b
    | Add (Int a, Int b) -> Int (a + b)
    | Add (a, b) -> Add (simplify1 a, simplify1 b)
    | Mul (Int 0, b) -> Int 0
    | Mul (a, Int 0) -> Int 0
    | Mul (a, Int 1) -> a
    | Mul (Int 1, b) -> b
    | Mul (Int a, Int b) -> Int (a * b)
    | Mul (a, b) -> Mul (simplify1 a, simplify1 b)
    | Pow (Int a, b) -> Int (pow a b)
    | Pow (a,  0) -> Int 1
    | Pow (a, 1) -> a
    | Var (x) -> Var x
    | Int (a) -> Int a
    | Pow (a, i) -> Pow (simplify1 a, i)


let rec simplify expr =
    let e = simplify1 expr in
      if expr = e
        then expr
        else simplify e


(* C.2 *)
let rec deriv expr x =
    match expr with
    | Int (int) -> Int 0
    | Var expr -> if expr = x then Int 1 else Int 0
    | Add (expr, y) -> Add((deriv expr x), deriv y x)
    | Mul (expr, y) -> Add((Mul(deriv expr x, y)), (Mul(expr, (deriv y x))))
    | Pow (expr, y) -> Mul(deriv expr x, (Mul(Int y, (Pow(expr, (y - 1))))))


let derivative expr var =
    let e = simplify expr in
    let d = deriv e var in
    simplify d
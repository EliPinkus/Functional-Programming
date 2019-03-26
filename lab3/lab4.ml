(* A.1.A *)
type mobile = Mobile of branch * branch  (* left and right branches *)
  and branch = 
    | Weight    of int * int     (* length and weight *)
    | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch m = 
	match br with
	| Mobile (l, r) -> l

let right_branch m =
	match br with
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
	| 'Weight w -> w
	| 'Structure sub -> total_weight2 sub
	

and total_weight2 mob =
	branch_weight2 (left_branch mob) + branch_weight2 (right_branch mob)
(* name: Eli Pinkus *)
(* login: epinkus *)

(* 1.a *)
(*
This function's time complexity is O(log2 n)

We can see this by considering how many recursive calls
are needed in the worst case to reach the base case.
Whether or not n in any particular call is even,
the recursive call reduces n by approximately half.

So we are looking to solve approximately :
(1/2)^m * n = 1 for m
n = 2^m
which is the definition of log2 :)
*)

(* 1.b *)
(*
To consider the worst case of this call,
consider if the intial n is odd.
since the recursive call is done with
n-2, each subsequent n will also be odd
so the only change in n will be subtracting 2
each recursive call, which means we will approach
the base case in linear time with respect to n.
We are approximately solving:

n-2m = 1 for m:
m = (n-1)/2

Thus the complexity is
O(n/2) = O(n)
*)

(* 1.c *)
(*
we consider the way this function's iter helper works,
The local variables a and b both start at 1, then b
is multiplied by 2 at each call of iter, until b >= n,
Then the same is done for a at which point the base 
case is called.

As such we are asking how many times 1 needs to be doubled to exceed n
and then doing that twice. We can see that the question
of how many times 1 needs to be doubled to exceed n is solving:

1*2^m = n for m
which is the definition of log2(n)
so the time complexity if O(2*log2(n)) = O(log2 n)
*)

(* 1.d *)
(*
The time complexity of this sort is O(n log3(n))
where n is the length of the input list

Let time complexity of the sort = T(N)
Split into Thirds -> O(N) = c1*N
Sort thirds -> 3*(T(N/3))
Merge -> O(N) = c2*N
Let c=c1+c2
T(N) = c*N + 3*T(N/3)
     = c*N + 3*(c*N/3 + 3*T(N/9))
     = c*N + 3*(c*N/3 + 3*(c*N/9 + 3*T(N/27)))
     = c*N + 3*c*N/3  + 9*c*N/9 ...
     = c*N + c*N + c*N ...
T(1) = O(1)
T(N) = O(N) * log3(N) = O(N*log3(N))

Intuitively, 
We split log3(N) times
We do O(N) work each split:
to overall complexity = O(N*log3(N))
*)


(* 2.1 *)

let split3 l =
	let rec mod0_part l' =
		match l' with
		| [] -> []
		| [x] -> [x]
		| h :: _ :: _ :: t -> h :: mod0_part t
		| h :: _ :: _ -> [h]
	in
	let mod1_part l' = 
		match l' with
		| [] -> []
		| [_] -> []
		| _ :: t -> mod0_part t
	in
	let mod2_part l' =
		match l' with
		| [] -> []
		| [_] -> []
		| _ :: t -> mod1_part t
	in

	(mod0_part l, mod1_part l, mod2_part l)

let merge3 l1 l2 l3 =
	let rec merge2 l1' l2' =
		match (l1', l2') with
	   | ([], _) -> l2'
	   | (_, []) -> l1'
	   | (h1 :: t1, h2 :: _)
	       when h1 <= h2 ->
	         h1 :: merge2 t1 l2'
	   | (_, h2 :: t2) ->
	       h2 :: merge2 l1' t2
	in
	let t1 = merge2 l1 l2 in
	merge2 t1 l3

let rec merge_sort3 lst =
    match lst with
      | [] -> []
      | [_] -> lst
      | _ ->
        let (lst1, lst2, lst3) = split3 lst in
          merge3 (merge_sort3 lst1) (merge_sort3 lst2) (merge_sort3 lst3)


(* 2.2.a *)
let smallest_index l =
	let rec iter lst min count total =
		match lst with
		| [] -> total - count 
		| h :: t
			when h < min -> iter t h 0 (total+1)
		| _ :: t ->
							iter t min (count+1) (total+1)
	in
		match l with
		| [] -> invalid_arg "list empty!"
		| [_] -> 0
		| h::t -> iter t h 1 0

(* 2.2.b *)
let rec flip_n n lst =
	let rec iter old result m =
		match old with
		| [] -> if m = 0 then result 
			else invalid_arg "flip_n: not enough elemnts"
		| h::t when m > 0 -> iter t (h::result) (m-1)
		| h::t when m = 0 -> result @ (h::t)
	in
		iter lst [] n

(* 2.2.c *)
let block_sort1 lst =
	match lst with
	| [] -> []
	| _->flip_n (smallest_index lst + 1) lst

(* 2.2.d *)
let rec block_sort lst =
    match block_sort1 lst with
      | [] -> []
      | h :: t -> h :: block_sort t

(*
This is an example of structural recursion because the recursive call
is made on a subset of the original least. No calculations from the 
higher level calls influence the lower level calls. Thus it is structural.
*)

let block_sorti lst =
	let rec iter lst result =
		match block_sort1 lst with
		| [] -> result
		| h::t -> iter t (result @ [h])
	in
		iter lst []


(* 3.1.a *)
let linrec is_base on_base splitter combine =
    let rec f x =
      if is_base x then
        on_base x
      else
      	let temp = splitter x in
      	match temp with
      	| (a, b) -> combine a (f b)
        (* TODO *)
    in f  (* return the resulting recursive function *)


let factorial =
    let is_base n = n = 0 in
    let on_base _ = 1 in
    let splitter n = (n, n - 1) in
    let combine a b = a * b in   (* or just: combine = ( * ) *)
      linrec is_base on_base splitter combine

(* 3.1.b *)
let insert_r item =
    (* two base cases: the empty list 
     * and when the item < the first element of the list *)
    let is_base lst = lst = [] || item <= List.hd lst in
  
    (* for both base cases, put the item on the front of the list *)
    let on_base lst = item :: lst in
  
    (* split the list.  Hint: structural recursion. *)
    let splitter lst = (List.hd lst, List.tl lst) in
  
    (* combine to get the final result *)
    let combine first rest_after_rec = (first :: rest_after_rec) in
  
      linrec is_base on_base splitter combine

(* 3.1.c *)
let insertion_sort =
    (* base case: the list is empty *)
    let is_base lst = lst = [] in
  
    (* if it's a base case, return the empty list *)
    let on_base _ = [] in
  
    (* otherwise, split (hint: structural recursion again) *)
    let splitter lst = (List.hd lst, List.tl lst) in
  
    (* then combine *)
    let combine first rest_after_rec = insert_r first rest_after_rec in
  
      linrec is_base on_base splitter combine

(* 3.2.a *)

let binrec is_base on_base splitter combine =
    let rec f x =
      if is_base x then
        on_base x
      else
     	let temp = splitter x in
     	match temp with
     	| (a,b,c) -> combine a (f b) (f c)
    in f  (* return the resulting recursive function *)

let fib =
    let is_base n = n < 2 in
    let on_base n = n in
    let splitter n = (0, n-1, n-2) in
    let combine _ n1 n2 = n1 + n2 in
      binrec is_base on_base splitter combine

(* 3.2.b *)
let quicksort =
    let is_base lst = lst = [] in
    let on_base _ = [] in
    let splitter lst =
      match lst with
        | [] -> invalid_arg "quicksort: can't split"
        | h :: t -> (h, (List.filter (fun x -> x < h) t), (List.filter(fun x -> x >= h) t))
    in
    let combine pivot lt ge = lt @ [pivot] @ ge in
      binrec is_base on_base splitter combine



(* 3.3.a *)


let tailrec is_base on_base next =
    let rec f inputs =
    	if is_base inputs then on_base inputs
    	else f (next inputs) 
    in f  (* return the tail-recursive function *)

let fact_iter n =
    let is_base (n, _) = n = 0 in
    let on_base (_, r) = r in
    let next (n, r) = (n - 1, r * n) in
    let iter = tailrec is_base on_base next in
      iter (n, 1)

(* 3.3.b *)
let insert_i item lst =
    let is_base (_, rest) = rest = [] || item <= List.hd rest in
    let on_base (prev, rest) = prev @ (item :: rest) in
    let next (prev, rest) = (prev @ [List.hd rest], List.tl rest) in
    let iter = tailrec is_base on_base next in
      iter ([], lst)

(* 3.3.c *)
let insertion_sort_i lst =
    let is_base (_, rest) = rest = [] in
    let on_base (prev, _) = prev in
    let next (prev, rest) =  (insert_i (List.hd rest) prev, List.tl rest) in
    let iter = tailrec is_base on_base next in
      iter ([], lst)

(* 4.1 *)

type tree =
    | Leaf
    | Node of int * int * tree * tree   (* level, value, left/right subtrees *)

let rec member n tree =
	match tree with
	| Leaf -> false
	| Node(_, v, left, right) ->
			match n with
			| _ when v = n -> true
			| _ when v > n -> member n left
			| _ when v < n -> member n right


let skew root =
	match root with
	| Node (lvl, v, Node (lvl2, v2, ll, lr), right)
		when lvl = lvl2 -> Node(lvl2, v2, ll, Node(lvl2, v, lr, right))
	| _ -> root


let split root =
	match root with
	| Node (lvl, v, left, Node(lvl2, v2, rl, Node(lvl3, v3, rrl, rrr)))
		when lvl = lvl2 && lvl2 = lvl3 -> 
		Node(lvl + 1, v2, Node(lvl, v, left, rl), Node(lvl3, v3, rrl, rrr))
	| _ -> root


let rec insert item t =
    match t with
      | Leaf -> Node(1, item, Leaf, Leaf)
      | Node (lvl, v, l, r) when item = v -> t
      | Node (lvl, v, l, r) ->
      		match item with
      		| _ when item > v -> 
      			let right = split (skew (insert item r)) in
      				split (skew (Node(lvl, v, l, right)))
      		| _ when item < v ->
      			let left = split (skew (insert item l)) in
      				split (skew (Node(lvl, v, left, r)))
(* A.1 *)
(*
FRAME 0 (initial environment)
  parent: none
  bindings:
    - : [built-in function -]
    * : [built-in function *]
FUNCTION 0 (fun: n -> let rec iter m r = if ...)
  env: FRAME 0
  param: n
  body: let rec iter m r ...
FRAME 1 (let factorial n = ... in factorial 3)
  parent: FRAME 0
  bindings:
      factorial: FUNCTION 0
FRAME 2 (factorial 3)
  parent: FRAME 0
  bindings:
      n: 3
FUNCTION 1 (fun m r -> if m = 0 ...)
  env: FRAME 3
  param: m r
  body: if m = 0 ...
FRAME 3 (let rec iter m r = if ...)
  parent: FRAME 2
  bindings:
      iter: FUNCTION 1
FRAME 4 (iter n 1)
  parent: FRAME 3
  bindings:
      m = 3
      r = 1
FRAME 5 (iter 2 3)
  parent: FRAME 3
  bindings:
      m = 2
      r = 3
FRAME 6 (iter 1 6)
  parent: FRAME 3
  bindings:
      m = 1
      r = 6
FRAME 7 (iter 0 6)
  parent: FRAME 3
  bindings:
      m = 0
      r = 6
  
*)

(* A.2 *)
let factorial = 
	let f = ref (fun x -> 0) in
	let help n = 
		if n =0 then 1
		else n* !f(n-1) in
	f := help;
	!f

(* B.1 *)
exception Stat_error of string

let make_stat_1 n =
	let sum = ref 0.0 in
	let sum_squared = ref 0.0 in
	let n = ref 0 in

	object
		method append num =
			begin
				sum := !sum +. num;
				sum_squared := !sum_squared +. (num *. num);
				n := !n +1
			end
		method mean =
			match !n with
			| 0 -> raise (Stat_error "need at least one value for mean")
			| _ -> !sum /. (float_of_int !n)

		method variance =
			match !n with
			| 0 -> raise (Stat_error "need at least one value for variance")
			| _ -> (!sum_squared -. ((!sum *. !sum) /. (float_of_int !n))) /.
						(float_of_int !n)

		method stdev =
			match !n with
				| 0 -> raise (Stat_error "need at least one value for stdev")
				| _ -> sqrt ((!sum_squared -. ((!sum *. !sum) /. 
							(float_of_int !n))) /.(float_of_int !n))

		method clear =
			begin
				sum := 0.0;
				sum_squared := 0.0;
				n := 0;
			end
		end	


let make_stat_2 n =
	let sum = ref 0.0 in
	let sum_squared = ref 0.0 in
	let n = ref 0 in

	object (self)
		method private _variance =(!sum_squared -. ((!sum *. !sum) /. 
						(float_of_int !n)))/. (float_of_int !n)
		method append num =
			begin
				sum := !sum +. num;
				sum_squared := !sum_squared +. (num *. num);
				n := !n +1
			end
		method mean =
			match !n with
			| 0 -> raise (Stat_error "need at least one value for mean")
			| _ -> !sum /. (float_of_int !n)

		method variance =
			match !n with
			| 0 -> raise (Stat_error "need at least one value for variance")
			| _ -> self#_variance

		method stdev =
			match !n with
				| 0 -> raise (Stat_error "need at least one value for stdev")
				| _ -> sqrt (self#_variance)

		method clear =
			begin
				sum := 0.0;
				sum_squared := 0.0;
				n := 0;
			end
		end	


(* C.1 *)

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
		let is_empty h = (h = empty)

		let rank h =
			match h with
			| Leaf -> 0
			| Node (r,n,x,y) -> r

		let rec merge h1 h2 =
			let help h1 h2 res =
				match () with
				| _ when rank h1 < rank h2 -> Node(rank h1 + 1, res, h2, h1)
				| _ -> Node(rank h2 + 1, res, h1, h2)
			in

			match (h1, h2) with
			| (Leaf, _) -> h2
			| (_, Leaf) -> h1
			| (Node(r1, n1, x1, y1), Node(r2, n2, x2, y2)) ->
				if n2 > n1 then help x1 (merge y1 h2) n1
				else  help x2 (merge y2 h1) n2

		let insert h n =
			merge h (Node (1, n, Leaf, Leaf))

		let find_min h =
			match h with
			| Leaf -> raise Empty
			| Node (r, n, x, y) -> n

		let delete_min h = 
			match h with
			| Leaf -> raise Empty
			| Node(r, n, x, y) -> merge x y

		let rec from_list l =
			match l with
			| [] -> Leaf
			| h::t -> insert (from_list t) h
	end

let heap_sort l =
	let rec iter h res =
		if PriorityQueue.is_empty h then List.rev res
		else
			let min = PriorityQueue.find_min h in
			let h' = PriorityQueue.delete_min h in
			iter h' (min::res)
		in
	iter (PriorityQueue.from_list l) []


(* C.2 *)
(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
sig
	type t
	val cmp: t -> t -> comparison
end
module OrderedString =
    struct
      type t = string
      let cmp x y = 
        if x = y then EQ else if x < y then LT else GT
    end

module MakePriorityQueue (Elt : ORDERED) : 
		(PRIORITY_QUEUE with type elem = Elt.t) =
	struct
		exception Empty
		type elem = Elt.t
		type t = Leaf | Node of int * elem * t * t

		let empty = Leaf
		let is_empty h = (h = empty)

		let rank h =
			match h with
			| Leaf -> 0
			| Node (r,n,x,y) -> r

		let rec merge h1 h2 =
			let help h1 h2 res =
				match () with
				| _ when rank h1 < rank h2 -> Node(rank h1 + 1, res, h2, h1)
				| _ -> Node(rank h2 + 1, res, h1, h2)
			in

			match (h1, h2) with
			| (Leaf, _) -> h2
			| (_, Leaf) -> h1
			| (Node(r1, n1, x1, y1), Node(r2, n2, x2, y2)) ->
				if Elt.cmp n1 n2 = LT then help x1 (merge y1 h2) n1
				else  help x2 (merge y2 h1) n2

		let insert h n =
			merge h (Node (1, n, Leaf, Leaf))

		let find_min h =
			match h with
			| Leaf -> raise Empty
			| Node (r, n, x, y) -> n

		let delete_min h = 
			match h with
			| Leaf -> raise Empty
			| Node(r, n, x, y) -> merge x y

		let rec from_list l =
			match l with
			| [] -> Leaf
			| h::t -> insert (from_list t) h
	end

module StringPQ = MakePriorityQueue(OrderedString)


let heap_sort_2 l =
	let rec iter h res =
	if StringPQ.is_empty h
		then List.rev res
	else
		let min = StringPQ.find_min h in
		let h' = StringPQ.delete_min h in
		iter h' (min::res) in
	iter (StringPQ.from_list l) []


(* D.1 *)
type 'a conts = Result of 'a | Expr of (unit -> 'a)
type 'a lazy_t = 'a conts ref
let make_lazy e = ref (Expr e)
let force lz = 
	match !lz with
	| Result r -> r
	| Expr e -> 
				let r = e () in begin
					lz := Result (r);
					r
				end

(* D.2 *)
let y = 
    fun f -> 
      (fun z -> z (`Roll z)) 
      (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

(* D.2.1 *)
let almost_sum =
	fun f l ->
		match l with
		| [] -> 0
		| h::t -> h + (f t) 

let sum = y almost_sum

(* D.2.2 *)
let factorial2 n =
	let iter =
		fun f ->
			fun n ->
				fun r ->
					if n = 0 then r
					else f (n-1) (r * n)
	in (y iter) n 1
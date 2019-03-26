open Num
let ni = num_of_int

(* A.1*)
(*
The space complexity of the fib call is O(n).
This is clear because with applicative order, the first
argument of the recursive fib call (i.e. the fib (n-1))
call, is evaluated before the second, so the function evaluates
all of those calls to the full recursive depth of n, before returning
to the top level call and proceeding to do the same with the
fib (n-2) call. We can see that the space taken at any given time
does not scale faster than n.
*)

(* A.2.a *)
(*
We know that each call to p, reduces the angle size by a factor of 1/3
We know that we stop calling p when the angle is < 0.1
So to determine how many calls are needed for sine 12.5
We ask what value of x solves:
12.5 / 3^x < 0.1
x = 4.39
so we would need 5 recursive calls to get an angle < 0.1 
*)

(* A.2.b *)
(* 
The space complexity is determined by the largest number of pending
function calls that can occur for a given angle. Since the angle
is divided by 3 each call we have on order of log3(a) calls
so the space complexity is O(log(a))

In this case the number of pending operations is on the same order as
the total operations so the time complexity is also O(log(a))
*)

(* A.3.1 *)
let rec fast_expt b n =
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
      match n with
      	| 0 -> 1
        | a when is_even a -> square (fast_expt b (a/2))
        | a -> b * fast_expt b (a-1)


(* A.3.2 *)
let ifast_expt b n =
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
	let rec iter a b n = 
		match n with 
		| 0 -> a
		| m when is_even m -> iter a (square b) (n / 2)
		| m -> iter (a * b) b (n-1)
	in
	iter 1 b n

(* A.3.4 *)
let rec fast_mult x y =
	let is_even m = m mod 2 = 0 in
	let double a = a + a in
	let halve b = b / 2 in
	match x, y with
	| a, 1 -> a
	| a, b when is_even b -> fast_mult (double a) (halve b)
	| a, b -> a + fast_mult a (b-1)

(* A.3.5 *)
let ifast_mult x y =
	let is_even m = m mod 2 = 0 in
	let double a = a + a in
	let halve b = b / 2 in
	let rec iter a b n =
		match n with
		| 0 -> a
		| m when is_even m -> iter a (double b) (halve m)
		| m -> iter (a + b) b (m-1)
	in
	iter 0 x y

(* A.3.6 *)
(*
we know by applicative order that the recursive call
associated with the first argument to foo will excecute completely,
and then the second call will do the same.
We consider that each call to foo creates 2 more calls to foo later,
we also note that since n is being halved, the depth of the recursive tree
scales with log2(n) but since the number of calls scales with 2^n, the 
time complexity is O(n) collectively for all calls to execute.
Because we are using applicative order, only one full branch of 
the recursive tree is ever on hold at one time, as such,
the space complexity scales with the depth of the recursion which is O(log(n))
*)

(* A.3.7 *)
(*
This function is linear recursive. We can see
that there is exactly 1 recursive call in the function body
and since the recursive call executes on n-1 each time, 
we know the total number of function calls is on the order of n
so the pattern is recursive linear.

This implies that the time complexity is O(n),
the space complexity is also O(n) because the only thing being
stored is a linearly scaling number of stack frames needed to 
execute the linear recursion pattern (we also have the 2 tuple,
but that is a constant amount of space throughout execution)
space complexity: O(n)
*)

(* B.1.a*)
(*
(fun x y -> x * (2 + y)) 20 (2 * 4)
*)

(* B.1.b*)
(*
(fun a b c -> sqrt(b *. b -. 4.0*. a *. c)) 1.0 20.0 3.0
*)

(* B.1.c *)
(*
(fun x = let y =2 in let z = 3 in x * y * z) 1
	(fun x -> (fun y -> let z =3 in x * y * z) 2) 1
		(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1
*)

(* B.1.d *)
(*
(fun x -> let x = 2 in let x = 3 in x * x * x) 1
      (fun x -> (fun x -> let x = 3 in x * x * x) 2) 1
         (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1

*)

(* B.2 *)
(*
(fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
(fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 * 4)

eval (2 * 10) -> 20
eval 3 + 4 -> 7

eval fun 20 7
	eval 20 -> 20
	eval 7 -> 7
	fun y -> (fun z -> 20 * y * z) 22) 14
		sub 14 for y
		eval fun z
		eval 22 -> 22
	(fun z = x + y + z) 22
		sub 22 for z in x * y * z
		yields x * y *22
	yields x * 14 * 22
	sub 20 for x
	yields 20 * 14 * 22

	yields 6160 
*)

(* B.3 *)
(*
(fun x y z -> x + y + z) 10 (x + 2) (y + 3)
*)

(*
It is clear by the desugaring that this won't work since
x and y are not defined when they are passed to the function

Ben should use all let in's so that each variable is evaluated
in order.
*)

let rec sum term a next b = 
    if a >/ b
       then (ni 0)
       else term a +/ (sum term (next a) next b)

(* C.1 *)
let isum term a next b =
    let rec iter a result =
      if a >/ b
         then result
         else iter (next a) (result +/ term a)
    in 
      iter a (ni 0)

(* C.2.a *)
let product term a next b =
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (result */ term a)
	in
		iter a (ni 1)

let rec product_rec term a next b =
	if a >/ b then (ni 1)
	else term a */ (product_rec term (next a) next b)



let factorial_rec n = 
	product_rec (fun x -> x) (ni 1) (fun x -> x +/ ni 1) n

(* C.2.b *)
let product_iter term a next b =
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (result */ term a)
	in
		iter a (ni 1)

let factorial_iter n = 
	product_iter (fun x -> x) (ni 1) (fun x -> x +/ ni 1) n

let pi_product n =
	let is_even m = mod_num m (ni 2) = (ni 0) in
  	let numerator m = if is_even m then m +/ (ni 2) else m +/ (ni 1) in
  	let denominator m = if is_even m then m +/ (ni 1) else m +/ (ni 2) in
  	let next m = m +/ (ni 1) in
  	(ni 4) */ (product_iter numerator (ni 1) next n) 
  		// (product_iter denominator (ni 1) next n)


let pi_approx =  float_of_num (pi_product (ni 1001))

(* C.3.a *)
let rec accumulate_rec combiner null_value term a next b =
	if a >/ b then null_value
	else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

(* C.3.b *)
let accumulate_iter combiner null_value term a next b =
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (combiner result (term a))
		in
			iter a null_value


(* C.4 *)
let compose f g =
	fun x -> f (g x)

(* C.5 *)

let rec repeated f n =
	if n = 0 then (fun x -> x)
	else compose f (repeated f (n-1))

(* C.6 *)
let smooth dx f =
	fun x -> (f (x -. dx) +. f x +. f (x +. dx)) /. 3.0

let nsmoothed dx f n =
	(repeated (smooth dx) n) f

(* D.1 *)

let is_prime n =
	if n < 2 then false
	else
		let rec iter m =
			match m with
			| p when p * p > n -> true
			| p when n mod p = 0 -> false
			| _ -> iter (m + 1)
		in
			iter 2

(* D.2 *)
let smallest_prime_factor n =
	match n with
	| n when is_prime n -> invalid_arg "input is prime"
	| n when n < 2 -> invalid_arg "input < 2"
	| n -> let rec iter m =
				if n mod m = 0 && is_prime m then m else iter (m+1)
		in iter 2
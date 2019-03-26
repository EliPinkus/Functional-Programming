(*
A.1.1
- : int = 10
A.1.2
- : float = 10.
A.1.3
- : int = 12

A.1.4
Error: This expression has type float but an expression was expected of type
         int

this is because there are different operations for floating point nums

A.1.5
Error: This expression has type int but an expression was expected of type
         float

This is because we tried to apply float addition to integers


A.1.6
Error: This expression has type float but an expression was expected of type
         int
we tried to apply int addition to an int and a float

A.1.7
Error: This expression has type int but an expression was expected of type
         float

we tried to apply float addition to an int and a float

A.1.8
- : float = 7.2

A.1.9
- : int = 5

A.1.10
- : int = 7

A.1.11
val a : int = 3

A.1.12
val b : int = 4

A.1.13
- : bool = false

A.1.14
- : bool = true

A.1.15
- : bool = false

this operator checks for address equivilence, not value equivilence

A.1.16
- : (int * int * int) list = [(1, 2, 3)]

A.1.17
- : (int * int * int) list = [(1, 2, 3)]
This is the same syntax that most languages use to define lists
however OCaml interperrets this as a list of tuples because the () 
around a tuple are implied if not there.

A.1.18
- : int = 4

A.1.19
Error: Syntax error

typing and isn't an operator in OCaml

A.1.20
- : int = 6

A.1.21
Error: This expression has type int but an expression was expected of type
         unit

With no else clause the implied type if else operates is unit type,
so since the expression after the if clause is an int, the implied 
else is inconsistent and throws an error.
*)

(* A.2 *)
let sum_of_squares_of_two_largest a b c = 
	match a, b, c with
			| a, b, c when a >= c && b >= c -> a * a + b * b
			| a, b, c when a >= b && c >= b -> a * a + c * c
			| _ -> b * b + c * c

(* A.3 *)
(*
Because a function is a type in OCAML, we can have the if then else clause
evaluate to a certain operator (+) if b >0 and (-) otherwise.
The resulting operator can then be wrapped in paranthesis,
turning it into a two-arg function, and it can be applied
to the inputs a and b

*)

(* B.1 *)
(*
Using normal order:
the program would not evaluate the arguments immediately but instead
check the predicate, which only refers to x,
as such the x = 0 will evaluate to true and the program will output 0
right away

Using applicative order:
the program would evaluate both arguments which will lead to an infinite loop
with let rec p () = p ()
*)

(* B.2 *)
(*
With applicative order, we evaluate the arguments to a function immediately,
since the new_if is in an argument to a recursive that depends on the result
of the recursive call, we enter an infinite loop were the new_if can never 
return a result and the recursion will continue indefinately.
*)

(* B.3 *)
(*

eval means evaluate
sub means substitute

FIRST CALL
add_a 2 5
	eval 2->2
	eval 5->5
    eval add_a -> fun a b -> if ...
	sub 2 for a
	sub 5 for b
    eval if 2 = 0
        eval 2-> 2
        eval 2=0 -> false
    else inc (add_a (dec 2) 5)
    eval inc (add_a (dec 2) 5)-> fun add_a (dec 2) 5...
        eval add_a (dec 2) 5 -> func (dec 2) 5 -> if ...
            eval dec 2 -> 1
            eval 5 -> 5
            eval 1 -> 1
            eval add_a 1 5 -> fun 1 5 -> if...
                evaluate if 1 = 0
                    eval 1-> 1
                    eval 0-> 0
                    eval 1=0 -> false
            else inc (add_a (dec 1) 5)
            eval inc (add_a (dec 1) 5)-> fun (dec 1) 5 -> if ...
                eval dec 1 -> 0
                eval 5 -> 5
                eval 0->
                eval add_a 0 5 -> fun 0 5 -> if...
                    eval if 0=0
                        eval 0->0
                        eval 0->0
                        eval 0=0 -> true
                    returns 5
            eval inc 5 -> 6
    eval inc 6 -> 7

    answer is 7


		
OTHER CALL
add_b 2 5
    eval add_b a b -> fun a b -> if... 
    	eval 2 ->2
    	eval 5 ->5
    	sub 2 for a
    	sub 5 for b
    	
    	if 2=0
            eval 2->2
            eval 0 -> 0
    		eval 2->2
    		eval 0->0
	        eval 2 = 0 -> false

	eval add_b (dec 2) (inc 5) -> fun a b -> if...
		dec 2 -> 1
		inc 5 -> 6
	add_b 1 6
		if 1=0
            eval 1-> 1
            eval 0-> 0

		eval 1=0 -> false
		eval add_b (dec 1) (inc 6) -> fun a b -> if...
			eval dec 1 -> 0
			eval inc 6 -> 7
		add_b 0 7
			if 0=0
                eval 0 -> 0
                eval 0 -> 0

			eval 0=0 -> true
			add_b 0 7 -> 7
		eval add_b 1 6 -> 7
	eval add_b 2 5 -> 7
	final answer 7
	

the first function is recursive and the second one is iterative.
*)


let rec factorial n =
    if n = 0 then 1 else n * factorial (n - 1)

(* C.1.a *)
let e_term n =
	1.0 /. float_of_int (factorial n)

(* C.1.b *)
let rec e_approximation n = 
	if n = 0 then 1.0
	else (e_term n) +. e_approximation (n - 1)

(* C.1.c *)
(* my function gets 2.71828182845904553
   the built in function gets 2.71828182845904509
*)

(* C.1.d *)
(*
For large n it returns float = infinity
In OCAML 100! is overflows and the integer type replaces it with 0
so it would evaluate e_term 100 to infinity when it added it to the result.
(realistically the overflow occurs before n=100 but the above 
illustrates the point)
*)

(* C.2 *)
let rec is_even n = 
	if n = 0 then true
	else is_odd (n - 1)

and is_odd n =
	if n = 0 then false
	else is_even (n - 1)


(* C.3 *)

let rec f_rec n =
	if n < 3 then n
	else f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec(n - 3)

let f_iter n =
	let rec iter a b c max cur =
		if cur = max then a + 2 * b + 3 * c
		else iter (a + 2 * b + 3 * c) a b max (cur + 1)
	in
		if n < 3 then n
		else iter 2 1 0 n 3

 



(* C.4 *)
let rec pascal_coefficient row ind = 
	match row, ind with
		| r, i when r < 1 || i < 1 || i > r-> failwith "invalid arguments"
		| _, 1 -> 1
		| 1, _ -> 1
		| row, ind when row = ind -> 1
		| row, ind -> pascal_coefficient (row - 1) (ind - 1) + 
							pascal_coefficient (row - 1) ind
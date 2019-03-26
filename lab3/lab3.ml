(* A.1 *)
type point = { x : float ; y : float}
type segment = { startp : point; endp : point}

let midpoint_segment { startp ; endp } =
	{ x = (startp.x +. endp.x) /. 2.0 ; y = (startp.y +. endp.y) /. 2.0 } 

let segment_length { startp ; endp } =
	let dx = startp.x -. endp.x in
	let dy = startp.y -. endp.y in
	sqrt (dx *. dx +. dy *. dy)

let print_point {x = x; y = y} =
    Printf.printf "(%g, %g)\n" x y

let make_point x y =
	{ x = x ; y = y }

let make_segment p1 p2 =
	{ startp = p1 ; endp = p2}

let get_coords pt = 
	(pt.x, pt.y)

let get_points seg =
	(seg.startp, seg.endp)


(* A.2 *)
type rectangle = { low_left : point ; up_right : point }
type rectangle2 = {x1 : float; x2 : float; y1 : float; y2 : float}


let rectangle_lower_segment { low_left ; up_right } =
	make_segment low_left (make_point up_right.x low_left.y)

let rectangle_upper_segment { low_left ; up_right } =
	make_segment up_right (make_point low_left.x up_right.y)

let rectangle_left_segment { low_left ; up_right } =
	make_segment low_left (make_point low_left.x up_right.y)

let rectangle_right_segment { low_left ; up_right } =
	make_segment up_right (make_point up_right.x low_left.y)

let rectangle_perimeter rect =
    let s1 = rectangle_left_segment rect in
    let s2 = rectangle_upper_segment rect in

    2.0 *. ((segment_length s1) +. (segment_length s2))

let rectangle_area rect =
	let s1 = rectangle_left_segment rect in
    let s2 = rectangle_upper_segment rect in
    segment_length s2 *. segment_length s1

let make_rectangle low_left up_right =
    {low_left = low_left; up_right = up_right}


let rectangle_lower_segment2 {x1 = x1; x2 = x2; y1 = y1; y2 = y2} =
    let p1 = make_point x1 y1 in
    let p2 = make_point x2 y1 in
    make_segment p1 p2

let rectangle_upper_segment2 {x1 = x1; x2 = x2; y1 = y1; y2 = y2} =
    let p1 = make_point x1 y2 in
    let p2 = make_point x2 y2 in
    make_segment p1 p2

let rectangle_left_segment2 {x1 = x1; x2 = x2; y1 = y1; y2 = y2} =
    let p1 = make_point x1 y1 in
    let p2 = make_point x1 y2 in
    make_segment p1 p2

let rectangle_right_segment2 {x1 = x1; x2 = x2; y1 = y1; y2 = y2} =
    let p1 = make_point x2 y1 in
    let p2 = make_point x2 y2 in
    make_segment p1 p2
    
let rectangle_perimeter2 rect =
    let s1 = rectangle_left_segment2 rect in
    let s2 = rectangle_upper_segment2 rect in

    2. *. ((segment_length s1) +. (segment_length s2))


let rectangle_area2 rect =
    let s1 = rectangle_left_segment2 rect in
    let s2 = rectangle_upper_segment2 rect in
    (segment_length s1) *. (segment_length s2)

let make_rectangle2 x1 x2 y1 y2 =
    {x1 = x1; x2 = x2; y1 = y1; y2 = y2}


(* A.3 *)
let make_pair x y = fun m -> m x y
  (* Or, equivalently: let make_pair x y m = m x y *)
  let first z = z (fun x y -> x)
  let second z = z (fun x y -> y)

(*
first (make pair x y)
	eval make_pair x y
	first (fun m -> m x y)
	eval first  to fun z (fun x y -> x)
	fun (fun x y -> x) -> (fun x y -> x) x y -> x

second (make_pair 1 2)
	eval make_pair 1 2
		eval 1 -> 1, 2 -> 2
		eval make_pair to fun x y -> (fun m -> m x y)
		apply fun x y (...) to 1 and 2
		fun m -> m 1 2
	eval second to ffun x -> x (fun x y -> y)
	apply fun x (...) to fun m -> m 1 2
		(fun m -> m 1 2) (fun x y -> y)
		apply fun m -> m 1 2 to fun x y -> y
			(fun x y -> y) 1 2
			eval 1 -> 1 and 2 -> 2
			apply fun x y (...) to 1 and 2
				-> 2


*)

(* A.4 *)
let pow b n =
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
	let rec iter a b n = 
		match n with 
		| 0 -> a
		| m when is_even m -> iter a (square b) (n / 2)
		| m -> iter (a * b) b (n-1)
	in
	iter 1 b n

let int_log a b =
	let div m = m / a in

	let rec iter a b count =
		match b with
		| 1 -> count
		| b when b mod a = 0 -> iter a (div b) (count + 1)
		| _ -> count
	in
	iter a b 0

let make_pairi a b =
	pow 2 a * pow 3 b

let firsti n =
	int_log 2 n

let secondi n =
	int_log 3 n

(* A.5 *)
let zero = []
  
let is_zero = function
	| [] -> true
	| () :: _ -> false
  
let succ u = () :: u

let prev = function
	| [] -> invalid_arg "arg is 0"
	| () :: t -> t

let integer_to_unary n =
	let rec iter count lst =
		if count = n then lst
		else iter (count + 1) (succ lst)
	in
	iter 0 []

let rec unary_to_integer lst = 
	if is_zero lst then 0
	else 1 + unary_to_integer (prev lst)

let rec unary_add l1 l2 =
	if is_zero l2 then l1
	else unary_add (succ l1) (prev l2)


type nat = Zero | Succ of nat
  
let zero' = Zero

let is_zero' = function
	| Zero -> true
	| Succ _ -> false

let succ' u = Succ u


let zero' = Zero

let is_zero' = function
    | Zero -> true
    | Succ _ -> false

let succ' u = Succ u

let prev' = function
    | Zero -> invalid_arg "Can't pass 0"
    | Succ n -> n

let integer_to_unary' n =
    let rec iter count result =
        if count = n then result
        else iter (count + 1) (succ' result)

    in iter 0 zero'

let unary_to_integer' un =
    let rec iter current result =
        if current = zero' then result
        else iter (prev' current) (result + 1)

    in iter un 0

let unary_add' a b =
    let int_result = (unary_to_integer' a) + (unary_to_integer' b) in
    integer_to_unary' int_result

(*
definitions are the same and don't need changes :)
*)

(* A.6 *)
(* zerof = "functional zero"; we call it this so as not to be confused with
     zero or zero' previously defined. *)
  
  let zerof = fun s -> fun z -> z  
    (* or equivalently: let zerof = fun s z -> z *)
    (* or equivalently: let zerof s z = z *)
  
  let add1 n = fun s -> fun z -> s (n s z)
    (* or equivalently: let add1 n = fun s z -> s (n s z) *)
    (* or equivalently: let add1 n s z = s (n s z) *)
(* A.6 *)


let add1 n = fun s -> fun z -> s (n s z)

let one s z = s z
let two s z = s (s z)
let three s z = s (s (s z))
let four s z = s (s (s (s z)))
let five s z = s (s (s (s (s z))))
let six s z = s(s (s (s (s (s z)))))
let seven s z = s(s (s (s (s (s (s z))))))
let eight s z = s(s (s (s (s (s (s (s z)))))))
let nine s z = s(s (s (s (s (s (s (s (s z))))))))
let ten s z = s(s (s (s (s (s (s (s (s z))))))))

let add m n s z =
    m s (n s z)

let church_to_integer m =
    m (fun x -> x+1) 0

(* A.7 *)
(*
We can see that the type signature of zero is see a' -> 'b -> 'b
Consider the application of the church_to_integer functions type signature
to this function. 
Type signature of church_to_integer is 
((int -> int ) -> int -> 'a')->  'a'
We can see that the 'a is the zero is subbed w/ (int -> int) 
and we know s has type (int -> int) if we substitute int for the first 'b
in zero we can tell that the next arg to zero should be an int as well.
We can also see that the last piece of zero is the same as the second arg 
which is an int, thus the result is an int.

'a -> 'b ->'b
(int -> int) -> int -> int

for one we see type sig ('a -> 'b) -> 'a -> 'b
just like for zero we have that ('a -> 'b) goes to (int -> int) so 
since 'a and 'b are ints we have a result of type 'b which is then
also an int
*)

(* B.1 *)
let rec last_sublist lst =
	match lst with
	| [] -> invalid_arg "last_sublist: empty list"
	| [n] -> [n]
	| h::t -> last_sublist t

(* B.2 *)
let reverse lst =
	let rec iter old result =
		match old with
		| [] -> result
		| h::t -> iter t (h :: result)
	in
	iter lst []

(* B.3 *)
let rec square_list = function
    | [] -> []
    | h :: t -> h * h :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* B.4 *)
(* 
This iterative version will not work because it looks 
at the element from the front and puts its square at the front
of a results list, it then repeats, making later squares
end up at the beginning of the list
*)

(* B.4 *)
(*
the change doesn't work because the :: "operator"
expects an arbitrary item on the left and a list
on the right, as such operating on a list on
the left and a number of the right will result in nested lists
as in the following example:
[1;2,3;] :: 3
same as
[1;2;3] :: [3]
results in
[[1;2;3]; 3]

We could replace the :: with @ but the efficiency is suboptimal since
for each appending, we copy every value from the old list, with occurs in O(n)
so the total is O(n^2) which isn't great.
*)

(* B.5 *)
let count_negative_numbers lst =
	let rec iter lst count = 
		match lst with
		| [] -> count
		| h::t when h < 0 -> iter t (count + 1)
		| _::t -> iter t count
	in
	iter lst 0

let power_of_two_list n = 
	let rec iter num lst = 
	if num = -1 then lst 
	else iter (num - 1) ((pow 2 num) :: lst)
in iter (n - 1) []

let power_of_two_listi n =
	let rec iter count lst =
		if count = 0 then lst
		else iter (count - 1) (count * count :: lst)
	in
		iter (n-1) []


let prefix_sum lst =
	let rec iter sum result old =
		match old with
		| [] -> result
		| h::t -> iter (sum + h) (result @ [sum + h]) t
	in
		iter 0 [] lst


(* B.6 *)
let deep_reverse lst =
    let rec iter l result =
        match l with
        | [] -> reverse result
        | h::t -> (iter t result)@[(reverse h)]
    in iter lst []

(* B.7 *)
type 'a nested_list =
    | Value of 'a
    | List of 'a nested_list list

let rec deep_reverse_nested lst =
    let reversal lst =
        let rec iter l2 result =
            match l2 with
            |[] -> result
            |h::t -> iter t ((deep_reverse_nested h) :: result)
        in iter lst []
    in match lst with
    | Value a -> Value a
    | List l2 -> List (reversal l2)
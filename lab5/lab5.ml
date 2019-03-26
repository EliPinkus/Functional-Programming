(* A.1 *)

let fibonacci n =
	if n = 0 then 0 else
	let t1 = ref 1 in
	let t2 = ref 1 in
	let count = ref 2 in
		while !count < n do
			count := !count + 1;
			t2 := !t2 + !t1;
			t1 := !t2 - !t1
		done;
	!t2

let fibonacci2 n =
	if n = 0 then 0 else
	let t1 = ref 1 in
	let t2 = ref 1 in
		for i = 3 to n do
			t2 := !t2 + !t1;
			t1 := !t2 - !t1
		done;
	!t2

(* A.2 *)
let bubble_sort arr = 
	for i = 0 to ((Array.length arr) - 2) do
		for j = 0 to ((Array.length arr) - 2) do
			if arr.(j) > arr.(j + 1) then
			let t = ref arr.(j) in
			arr.(j) <- arr.(j+1);
			arr.(j+1) <- !t
		done
	done

(* B.a *)
let meters_per_foot = 0.3048
  
let get_meters len =
	match len with
		| `Meter m -> m
		| `Foot f -> f *. meters_per_foot
		| `Inch i -> i /. 12.0 *. meters_per_foot
  
let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.b *)
let grams_per_slug = 14593.903203
let grams_per_kilo = 1000.0

let get_grams m =
	match m with
	| `Gram g -> g
	| `Kilo k -> grams_per_kilo *. k
	| `Slug s -> grams_per_slug *. s

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds t =
	match t with
	| `Second s -> s
	| `Minute m -> 60.0 *. m
	| `Hour h -> 3600.0 *. h
	| `Day d -> 86400.0 *. d

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* B.c *)

let unit_add a b =
	match (a, b) with
	| (`Length l1, `Length l2) -> `Length (length_add l1 l2)
	| (`Mass m1, `Mass m2) -> `Mass (mass_add m1 m2)
	| (`Time t1, `Time t2) -> `Time (time_add t1 t2)
	| _ -> failwith "units don't match"

(*
We do not get combinatorial explision when adding a unit
since we only need to add one more case to the addition function
*)

(* C.1 *)
let rec make_gram g =
	let grams_per_slug = 14593.903203 in
	object
	method get_grams = g
	method get_slugs = g /. grams_per_slug
	method unit_type = `Gram
	method compatible obj =
	match obj#unit_type with
	| `Gram -> true
	| `Slug -> true
	| _ -> false
	method add obj =
	match obj#unit_type with
	| `Gram -> make_gram (g +. obj#get_grams)
	| `Slug -> make_gram (g +. obj#get_grams)
	| _ -> failwith "not compatible"
end

(* C.2 *)

(* Define a number as a message-passing object. *)
  (* "i" is an int. *)
  let rec make_number i =
    object
      method value = i
      method show = string_of_int i
      method is_zero = i = 0
      method is_number = true
      method evaluate _ _ = make_number i  (* must evaluate to an object *)
      method derive _ = make_number 0  (* derivative of a number is 0 *)
    end
  
  (* Define a variable as a message-passing object. *)
  (* "v" is a string. *)
  let rec make_variable v =
    object
      method value = failwith "variable has no numerical value"
      method show  = v
      method is_zero = false
      method is_number = false
      method evaluate v' n =
        if v = v'
          then make_number n
          else make_variable v
      method derive v' =
        if v = v'
          then make_number 1  (* d/dx(x) = 1 *)
          else make_number 0  (* d/dx(y) = 0 *)
    end
  
  (* Define a sum as a message-passing object. *)
  let rec make_sum expr1 expr2 =
    match () with
      | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
      | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
      | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
            make_number (expr1#value + expr2#value)
      | _ ->  (* create a new object representing the sum *)
            object
              method value = failwith "sum expression has no numerical value"
              method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
              method is_zero = false
              method is_number = false
              method evaluate v n = 
                make_sum (expr1#evaluate v n) (expr2#evaluate v n)
              method derive v = 
                make_sum (expr1#derive v) (expr2#derive v)
            end
  
  (* Evaluate a message-passing expression with a number 
     substituted for a variable. *)
  let evaluate expr v n = expr#evaluate v n
    
  (* Return the string representation of an expression. *)
  let show expr = expr#show
  
  (* Return the derivative of an expression. *)
  let differentiate expr v = expr#derive v

  (* C.2.a *)
  let rec make_product e1 e2 =
  	(* use instead of if else chain *)
  	match () with
  	| _ when e1#is_zero || e2#is_zero -> make_number 0
  	| _ when e1#is_number && e1#value = 1 -> e2
  	| _ when e2#is_number && e2#value = 1 -> e1
  	| _ when e1#is_number && e2#is_number -> make_number (e1#value * e2#value)
  	| _ -> object
  			method value = failwith "product expression has no numerical value"
  			method show = "(" ^ e1#show ^ " * " ^ e2#show ^ ")"
  			method is_zero = false
  			method is_number = false
  			method evaluate v n =
  				make_product (e1#evaluate v n) (e2#evaluate v n)
  			method derive v =
  				make_sum (make_product (e1#derive v) e2)
  					(make_product e1 (e2#derive v))

  		end

(*
	1:
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>

	2:
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>

	3:
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + 
	(3 * ((x * (y * y)) + (x * (y * y)))))"

	4:
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"

	5:
"558"

	6:
"396"
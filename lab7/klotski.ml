(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:                *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Pervasives.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b = 
	let dest = LocSet.of_list [(3,1); (3,2); (4,1); (4,2)] in
		CharMap.exists (fun _ y -> LocSet.equal y dest) b.pieces

let compare b1 b2 = 
	let d1 = (CharMap.bindings b1.pieces) in
	let d2 = (CharMap.bindings b2.pieces) in
	let s1 = LocSetSet.of_list (List.map snd d1) in
	let s2 = LocSetSet.of_list(List.map snd d2) in
	LocSetSet.compare s1 s2

let remove c ({ pieces = p; unoccupied = u } as b) = 
	if CharMap.mem c p then
		let u' = LocSet.union u (CharMap.find c p) in
		{pieces = CharMap.remove c p; unoccupied =u'}
	else b

let add (c, p) { pieces = ps; unoccupied = u } = 
	let possible = (LocSet.subset p u) && (not (CharMap.mem c ps)) in
		if possible then
			Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}
		else None

let rec make_move (c, d, i) b =
	let mover d loc = 
		match d with
		| Up -> LocSet.map (fun (x,y) -> (x - 1, y)) loc
		| Right -> LocSet.map (fun (x,y) -> (x, y+1)) loc
		| Down -> LocSet.map (fun (x,y) -> (x + 1, y)) loc
		| Left -> LocSet.map (fun (x,y) -> (x, y - 1)) loc
	in


	if i < 1 || not (CharMap.mem c b.pieces) then None
	else
		let loc = CharMap.find c b.pieces in
		let loc' = mover d loc in
		let b' = add (c, loc') (remove c b) in
		match b' with
		| None -> b'
		| _ when i = 1 -> b'
		| Some b'' -> make_move (c, d, i-1) b''


let next b =
	let rec iter ps d i res =
		match (ps, i) with
		| ([], _) -> res
		| ((c, p)::t, 0) -> iter t d 4 res
		| ((c, p)::t, i) ->
			match make_move (c, d, i) b with
			| None -> iter ps d (i-1) res
			| Some b' -> iter ps d (i-1) (b'::res)
	in
	let binds = CharMap.bindings b.pieces in
		(iter binds Up 4 []) @ (iter binds Right 4 []) @
		(iter binds Left 4 []) @ (iter binds Down 4 [])



(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end


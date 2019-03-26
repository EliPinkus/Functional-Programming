open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    let search nrows ncols start_row start_col print =
    	let get_min_val loc_counts =
    		let rec get_min_iter l min =
    			match l with
    			| [] -> min
    			| h::t -> if (snd h) < min then get_min_iter t (snd h)
    					  else get_min_iter t min
    		in get_min_iter loc_counts (snd (List.hd loc_counts))
    	in
    	let get_mins loc_counts min =
    		let rec get_mins_iter l res count =
    			match l with
    			| [] -> (res, count)
    			| h::t -> if (snd h) = min then 
    						get_mins_iter t ((fst h)::res) (count + 1)
    					  else get_mins_iter t res count
    		in get_mins_iter loc_counts [] 0
    	in 

    	let board = B.place (B.make nrows ncols) (start_row, start_col) in

    	let rec iter b l =
    		if B.is_solved b then Some l
    		else
    			match B.get_loc_counts_from_loc b (B.get_last b) with
    			| [] -> None
    			| loc_counts ->
    				let (mins, count) = get_mins loc_counts (get_min_val loc_counts) in
    				let next = List.nth mins (Random.int count) in
    					iter (B.place b next) (next::l)
    		in
    	iter board [(start_row, start_col)]

  end


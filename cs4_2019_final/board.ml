open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)
    let get_locs row col = [(row+2,col+1);(row+2, col-1);(row-2,col+1);
    				(row-2,col-1);(row+1,col+2);(row+1,col-2);
    				(row-1,col+2);(row-1,col-2)]

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc

    let init_reachable nrows ncols =
    	
    	let count_valid row col =
    		let locs = get_locs row col in
    		let rec iter locs' count =
    			match locs' with
    			| [] -> count
    			| h::t ->
    					if ok_loc nrows ncols h then
    						iter t (count+1)
    					else iter t count
    			in iter locs 0
    		in

    	let rec iter_row row row_res =
    		let rec iter_col col col_res =
    			if col = ncols then col_res
    			else iter_col (col+1) (S.set col_res (row, col) 
    							(count_valid row col))
				in 
					if row = nrows then row_res
					else iter_row (row+1) (iter_col 0 row_res)
			in

    			iter_row 0 (S.make nrows ncols)




    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    let get_loc_counts_from_loc board loc = 
    	if not (check_bounds board loc) then invalid_arg "loc off board"
    	else
    		let row = fst loc in
    		let col = snd loc in
    		let locs = get_locs row col in
    		let rec iter locs' res =
    			match locs' with
    			| [] -> res
    			| h::t -> match check_bounds board h with
    					  | true ->
    					  		(
    					  		match S.get board.reachable h with
    					  		| None -> iter t res
    					  		| Some a -> iter t ((h,a)::res)
    					  		)

	    				  | false -> iter t res


    			
    						
    			in iter locs []
    
    let place board loc = 
    	let change_reachable reachable loc =
    		let reachable = S.remove reachable loc in
    		let row = fst loc in
    		let col = snd loc in
    		let locs = get_locs row col in
    		let rec iter locs' res =
    			match locs' with 
    			| [] -> res
    			| h::t ->
    				match S.get reachable h with 
    				| None -> iter t res
    				| Some a -> iter t (S.set res h (a-1))
    		in iter locs reachable
    	in


    	if (not (check_bounds board loc)) then invalid_arg "off board loc"
    	else
    		match S.get board.indices loc with
    		| Some a -> invalid_arg "knight already there"
    		| None -> 
    			if board.last_index = 0 then
    				{	
						board with
						placed = loc::board.placed;
						last_index = (board.last_index+1);
						indices = S.set board.indices loc 
											(board.last_index+1);
						reachable = change_reachable board.reachable loc
					}
				else
	    			let (row, col) = get_last board in
	    			let locs = get_locs row col in
	    				if not (List.mem loc locs) then
	    					invalid_arg "not reachable from last knight"
	    				
	 					else
						{	
							board with
							placed = loc::board.placed;
							last_index = (board.last_index+1);
							indices = S.set board.indices loc 
												(board.last_index+1);
							reachable = change_reachable board.reachable loc
						}

    let undo board = 
    	let change_reachable reachable loc =
    		let count_valid (row, col) =
    			let locs = get_locs row col in
    			let rec iter locs' count =
    				match locs' with
    				| [] -> count
    				| h::t->
    					if check_bounds board h then (
    					match S.get board.indices h with
    					| None -> iter t (count + 1)
    					| _ -> iter t count)
    					else iter t count
				in iter locs 0
			in
			let fix_neghbors reachable' (row, col) =
				let locs = get_locs row col in
				let rec iter locs' res =
					match locs' with
					| [] -> res
					| h::t ->
						match S.get reachable' h with 
	    				| None -> iter t res
	    				| Some a -> iter t (S.set res h (a+1))
	    		in iter locs reachable'
	    	in
	    		let result = S.set reachable loc (count_valid loc) in
	    		fix_neghbors result loc

	    in



    	if board.last_index = 0 then board
    	else
    		{
    			board with
    			placed = List.tl board.placed;
    			last_index = board.last_index - 1;
    			indices = S.remove board.indices (List.hd board.placed);
    			reachable = change_reachable board.reachable 
    												(List.hd board.placed)

    		}

    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end

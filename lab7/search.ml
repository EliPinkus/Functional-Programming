(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let search init = failwith "TODO"

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"

    let search init =
    	let storage = S.create() in
    	let hist = [init] in
    	S.push hist storage;
    	let visited = DS.empty in

    	let rec iter s visited =
    		if S.is_empty s then raise Not_found
    		else 
    			let next_hist = S.pop s in
    			match () with
    			| _ when DS.mem (List.hd next_hist) visited -> iter s visited
    			| _ when D.is_solved (List.hd next_hist) -> next_hist
    			| _ ->
    				let kids = D.next (List.hd next_hist) in
    				let rec iter' kid s' visited' =
    					match kid with
    					| [] -> iter s' visited'
    					| h::t -> S.push (h::next_hist) s';
    							iter' t s' (DS.add (List.hd next_hist) visited')
					in
						iter' kids s visited
			in
				iter storage visited
  end


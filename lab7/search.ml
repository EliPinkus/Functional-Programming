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
    	let rec help s visited =
    		if S.is_empty s then raise Not_found
    		else 
    			let hist = S.pop s in
    			match () with
    			| _ when DS.mem (List.hd hist) visited -> help s visited
    			| _ when D.is_solved (List.hd hist) -> hist
    			| _ ->
    				let visited = DS.add (List.hd hist) visited in
    				let kids = D.next (List.hd hist) in
    				begin
    					List.iter (fun x -> S.push (x::hist) s) kids;
    					help s visited;
    				end
    			in
    	let storage = S.create() in
    	let hist = [init] in
    	let visited = DS.empty in
    	begin
    		S.push hist storage;
    		help storage visited;
    	end

  end


module Loc =
  struct
    type t = int * int

    let compare = Pervasives.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = 
    	if nrows > 0 && ncols > 0 then
    	Array.make_matrix nrows ncols (-1)
    	else invalid_arg "row/col negative or zero"

    let get data (row, col) = 
    	match () with
    	| _ when row >= Array.length data -> None
    	| _ when row < 0 -> None
    	| _ when col >= Array.length data.(0) -> None
    	| _ when col < 0 -> None
    	| _ when data.(row).(col) = -1 -> None
    	| _ -> Some (data.(row).(col))


    	

    let set data (row, col) i = 
    	match () with
    	| _ when row >= Array.length data -> invalid_arg "invalid row"
    	| _ when row < 0 -> invalid_arg "invalid row"
    	| _ when col >= Array.length data.(0) -> invalid_arg "invalid col"
    	| _ when col < 0 -> invalid_arg "invalid col"
    	| _ when i < 0 -> invalid_arg "invalid number"
    	| _ -> data.(row).(col) <- i; data

    let has_loc data (row, col) =
    	match get data (row, col) with
    	| None -> false
    	| _ -> true

    let remove data (row, col) =
      	match () with
    	| _ when row >= Array.length data -> invalid_arg "invalid row"
    	| _ when row < 0 -> invalid_arg "invalid row"
    	| _ when col >= Array.length data.(0) -> invalid_arg "invalid col"
    	| _ when col < 0 -> invalid_arg "invalid col"
    	| _ -> data.(row).(col) <- -1; data
  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)
module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols =
    	match () with
    	| _ when nrows <= 0 -> invalid_arg "rows is negative"
    	| _ when ncols <= 0 -> invalid_arg "cols is negative"
    	| _ ->
	    	{
	    		contents = LocMap.empty;
	    		nrows;
	    		ncols
			}

    
    let get data (row, col) = 
    	if LocMap.mem (row, col) data.contents then
    		Some (LocMap.find (row, col) data.contents)
    	else None

    let set data (row, col) i =
    	match () with
    	| _ when row >= data.nrows || row < 0 
    		-> invalid_arg "invalid row"
    	| _ when col >= data.ncols || col < 0
    		-> invalid_arg "invalid col"
    	| _ when i < 0 -> invalid_arg "value must be >= 0"
    	| _ -> {data with contents = LocMap.add (row,col) i data.contents}

    let has_loc data (row, col) = 
    	match get data (row, col) with
    	| None -> false
    	| _ -> true

    let remove data (row, col) =
    	{data with contents = LocMap.remove (row, col) data.contents}
  end


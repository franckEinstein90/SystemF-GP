(**********************************
 Utils contains module a number of low-level 
 facilities used by the other modules
   in the typechecker/evaluator. 
***********************************)

(* Some pervasive abbreviations -- opened everywhere ****)

module Pervasive : sig
	val command : string->unit
	val un : 'a -> unit  
	val open_file : string->string list ref->in_channel 
	val fresh_lcid : unit->string
	val fresh_ucid : unit->string
end  

module ListSpecial : sig
	val index_of : 'a list -> ('a -> 'a -> bool) -> 'a -> int
	val permutations : 'a list -> 'a list list
	val cartesianProduct : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
	val union : ('a->'a->bool) -> 'a list -> 'a list -> 'a list
	val keys : ('a, 'b) Hashtbl.t -> 'a list	
	val removeMultiples : ?comp:('a -> 'a -> bool) -> 'a list -> 'a list
	val pred_union : 'a -> (('a -> bool) * ('a -> 'b list)) list -> 'b list
end

module StringSpecial : sig
	val pr : string -> unit
	val prf : ?filename:string option -> string -> unit 
	val toCharList : string -> char list
	val fromCharList : char list -> string
end

module RandSelect : sig
	val choose_random : ?selectionSize:int -> 'a list -> 'a list
	val rouletteWheelSelect : ?size:int -> 'a list -> ('a->float) -> 'a list
	val probHappen : ?prob:float -> 'a -> 'a -> 'a 
end


(* ------------------------------------------------------------------------ *)
(* Error printing utilities -- opened everywhere by convention *)
module Error : sig
  (* An exception raised by the low-level error printer; exported
     here so that it can be caught in module Main and converted into
     an exit status for the whole program. *)
  exception Exit of int

  (* An element of the type info represents a "file position": a 
     file name, line number, and character position within the line.  
     Used for printing error messages. *)
  type info
  val dummyinfo : info

  (* Create file position info: filename lineno column *)
  val createInfo : string -> int -> int -> info
  val printInfo : info -> unit

  (* A convenient datatype for a "value with file info."  Used in
     the lexer and parser. *)
  type 'a withinfo = {i: info; v: 'a}

  (* Print an error message and fail.  The printing function is called
     in a context where the formatter is processing an hvbox.  Insert
     calls to Format.print_space to print a space or, if necessary,
     break the line at that point. *)
  val errf : (unit->unit) -> 'a
  val errfAt : info -> (unit->unit) -> 'a

  (* Convenient wrappers for the above, for the common case where the
     action to be performed is just to print a given string. *)
  val err : string -> 'a
  val error : info -> string -> 'a

  (* Variants that print a message but do not fail afterwards *)
  val warning : string -> unit
  val warningAt : info -> string -> unit
end


module Symbols : sig
    val s_Pi : string
	val s_Lambda : string
	val s_lambda  : string	
	val s_TyInt : string
	val s_TyUnit : string
	val s_TyFloat : string
	val s_TyBool : string
	val s_TyString : string
	val s_TmTrue : string
	val s_TmFalse : string
end


module Statistics : sig
	val mean : 'a list -> ('a -> float) -> float
	val variance : 'a list -> ('a -> float) -> float
	val median : 'a list -> ('a -> float) -> float
end
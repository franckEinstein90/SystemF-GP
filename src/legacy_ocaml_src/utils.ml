(**********************************
 Utils contains module a number of low-level 
 facilities used by the other modules
   in the typechecker/evaluator. 
***********************************)


open Format
module Error = struct

exception Exit of int

type info = FI of string * int * int | UNKNOWN
type 'a withinfo = {i: info; v: 'a}

let dummyinfo = UNKNOWN
let createInfo f l c = FI(f, l, c)

let errf f = 
  print_flush(); 
  open_vbox 0; 
  open_hvbox 0; f(); print_cut(); close_box(); print_newline();
  raise (Exit 1)

let printInfo =
  (* In the text of the book, file positions in error messages are replaced
     with the string "Error:" *)
  function
    FI(f,l,c) ->
      print_string f; 
      print_string ":"; 
      print_int l; print_string "."; 
      print_int c; print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; print_space(); f())

let err s = errf (fun()-> print_string "Error: "; print_string s; print_newline())

let error fi s = errfAt fi (fun()-> print_string s; print_newline())

let warning s =
  print_string "Warning: "; print_string s;
  print_newline()

let warningAt fi s =
  printInfo fi; print_string " Warning: ";
  print_string s; print_newline()

end


(* Some pervasive abbreviations -- opened everywhere ****)

module Pervasive = struct

type info = Error.info
let un statement = ignore(statement) 
let command com =
	un(Thread.create (fun cmd -> Sys.command cmd) com)
let open_file infile search_path =  
	let rec trynext l = match l with 
	[] -> Error.err ("Could not find " ^ infile) 
	       | (d::rest) ->  
	           let name = if d = "" then infile else (d ^ "/" ^ infile) in 
	           try open_in name 
	             with Sys_error m -> trynext rest 
	   in trynext !search_path 
let fvar_ctr  = ref 0
let fresh_lcid ( ) =
	let str = string_of_int !fvar_ctr in
	fvar_ctr := 1 + !fvar_ctr;
	"fv" ^ str
let tvar_ctr  = ref 0
let fresh_ucid ( ) =
	let str = string_of_int !tvar_ctr in
	tvar_ctr := 1 + !tvar_ctr;
	"TV" ^ str
end 

module StringSpecial = struct
let pr = Format.print_string
let prf ?(filename=None) str = 
	match filename with 
	|None -> pr str; Format.print_flush( ) 
	|Some filename -> 
		let oc = open_out filename in
		output_string oc str;
		flush oc;
		close_out oc
let toCharList str = 
	let charLst = ref [] in
	for i=0 to (String.length str) - 1 do 
		charLst := !charLst @ [String.get str i];
	done;
	!charLst	
let fromCharList lst = 
	List.fold_left 
		(fun str c -> 
			str ^ (String.make 1 c)) "" lst
end

open StringSpecial
module ListSpecial = struct
(*Returns the index of the first element listelt *)
(*in lst such that comp elt listelt is true      *)
let index_of lst comp elt = 
	let ctr = ref 0 in
	let goOn = ref true in
	while (!ctr < List.length lst) && !goOn do
		match comp (List.nth lst !ctr) elt with 
		|false -> incr ctr 	
		|true -> goOn := false
	done;
	match !goOn with 
	|true -> !ctr
	|false -> failwith "not_found"

let shift_right_cycle lst =
	match lst with 
	|[]->lst
	|hd :: tl -> 
		match List.rev tl with 
		|[]->  lst
		| inv_hd :: inv_tl -> inv_hd :: (hd :: (List.rev inv_tl))
	
	
let rec permutations lst =
	match lst with 
	|[] -> [lst]
	| hd :: tl ->
		let perms = ref [] in  
		let cycles = ref lst in
		for i = 0 to ((List.length lst)-1) do
			perms := !perms @ (List.map (fun lst -> (List.hd !cycles)::lst) (permutations (List.tl !cycles)));
			cycles := shift_right_cycle (!cycles) 
		done;
		!perms

let cartesianProduct combineFunc alist blist = 
	let comb a =
		List.map (fun b -> combineFunc a b) blist in 
	List.flatten (List.map (fun a -> comb a) alist)
let keys hstbl = 
		Hashtbl.fold (fun a b accLst -> a::accLst) hstbl []
let rec union comp lst1 lst2 = 
	match lst2 with
	|[] -> lst1 
	|hd :: tail -> 
		let ins elt lst =  
			if List.exists (fun ty -> comp ty elt) lst 
			then lst 
			else elt :: lst 
		in union comp (ins hd lst1) tail  
let removeMultiples ?(comp = (fun a b -> a = b)) lst = 
	let new_lst = ref [] in
	List.iter (fun a -> 
		match List.exists (fun b -> comp a b) !new_lst with
		| false -> new_lst :=  !new_lst @ [a]
		| true -> ()) lst;
	!new_lst 

let pred_union obj pred_map_lst =
	List.flatten
	(List.map (fun (pred,mapFunc)->
		match pred obj with true -> mapFunc obj | _->[]) pred_map_lst)
end
		
	


module RandSelect = struct

(*Chooses a random element in a list*)
let choose_random ?(selectionSize=1) lst =  
	(* An object can only be chosen once *)
	match List.length lst > selectionSize with
	| false -> lst
    |true->
		let resLst = ref [] in
		let arr = Array.of_list (List.map (fun obj -> Some obj) lst) in
		while List.length !resLst < selectionSize do
			let currentIndex = ref (Random.int (List.length lst)) in
			let goOn ind = (match arr.(ind) with None -> true |_->false) in
			while goOn !currentIndex do
				if !currentIndex = (List.length lst) - 1 
					then currentIndex := 0
					else  currentIndex := !currentIndex + 1 
			done;
			resLst := arr.(!currentIndex) :: !resLst;								  
			arr.(!currentIndex) <- None
		done;
		List.map (function Some obj -> obj | None ->raise Not_found) !resLst
	
		
		
let rouletteWheelSelect ?(size=1) objLst magFunction =
	(*An object may be chosen more than once*)
	if size < 1 then failwith "Invalid selection size in rouletteWheelSelect";
	if List.length objLst < 1 then failwith "Invalid selection list in rouletteWheelSelect";
	let sum = List.fold_left (fun acc obj -> acc +. (magFunction obj)) 0. objLst in
	let chooseOne ( ) = 
		let needle = Random.float sum in
		let rec inner acc = function
		| [ ] ->	
			let begIndex = Random.int (List.length objLst) in
			let lst = Array.to_list (
				Array.sub (Array.of_list objLst) begIndex ((List.length objLst) - begIndex)
				) 
			in inner acc lst	
		| head :: tail -> 
			let num = magFunction head in
			if acc <= num then head 
			else inner (acc -. num) tail in
		inner needle [ ] in	
	let resList = ref [chooseOne ( )] in 
	while List.length !resList < size do
		resList := chooseOne( ) :: !resList
	done;
	!resList
let probHappen ?(prob=1.0) success failure = 
	if prob < 0.0 or prob > 1.0 then failwith "Invalid prob";
	if Random.float 1.0 > prob then failure else success
end


module Symbols = struct
	let  s_Pi = "TT"
	let  s_Lambda = "Lam"
	let  s_lambda = "lam"
	let s_TyInt = "Int"
	let s_TyUnit = "Unit"
	let s_TyFloat = "Float"
	let s_TyBool = "Bool"
	let s_TyString = "String"
	let s_TmTrue = "TmTrue"
	let s_TmFalse = "TmFalse"
end

module Statistics = struct

	let mean lst floatMap =  
		let n = List.length lst in
		assert (n > 0);
		(ListLabels.fold_left ~init:0.0 lst
		~f:(fun acc elt -> (floatMap elt) +. acc)) /. (float n)
	
	let variance lst floatMap = 
		let n = List.length lst in
		assert (n > 0);
		let mean = mean lst floatMap in
		((ListLabels.fold_left ~init:0.0 lst
		~f:(fun acc elt -> 
			let value = floatMap elt in
			(value *. value) +. acc)) /. (float n)) -. (mean *. mean)
	
	let median (lst:'a list) (floatMap:'a->float) = 
		let n = List.length lst in
		assert (n > 0);
		floatMap(List.nth 
		(List.sort (fun e1 e2 -> Pervasives.compare (floatMap e1) (floatMap e2)) lst)
		(n / 2))
end






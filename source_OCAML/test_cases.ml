open Utils.Pervasive
open Utils.StringSpecial

open Str
open Syntax
open Binding
open Core
open PrintExpr
open GPUtils

type test_case = term list * term 
let empty_test_case = [TmUnit], TmUnit

let rtc str test_cases ctx = 
	let to_ter str = ter (parse_string str ctx) in
	let reg = regexp "\\(.+\\)[,]\\(.+\\)" in
	
	let rec get_args str = 
		if string_match reg str 0
		then get_args(matched_group 1 str) @ [to_ter (matched_group 2 str)]
		else [to_ter str]
	
	in if string_match reg str 0 
			then test_cases @
						[get_args(matched_group 1 str) , 
						to_ter (matched_group 2 str)] 
			else failwith "rtc::Invalid string"
			
let get_type ctx  test_cases =
	let ty_of te = typeof ctx te in
	let ty_eq ty1 ty2 = ty_eqv ctx ty1 ty2 in
	
	let rec make_ty ter_lst ty = 
		match List.rev ter_lst with
		| [] -> ty
		| hd :: tail -> make_ty 
			(List.rev tail) (TyArr ((ty_of hd), ty)) in
	
	let rec match_type ty (te_args_lst, res) =
		let ste te = string_of_term ctx te Compact in
		let sty ty = string_of_type ctx ty Compact in
		match te_args_lst with
		| [] -> if ty_eq (ty_of res) ty then ( )  
					else failwith (ste res ^ " should be of type " ^sty ty ^ " but has type " ^ sty (ty_of res))
		| hd::tail -> 
			match ty with
			| TyArr(ty_left, ty_right) -> 
				if ty_eq ty_left (ty_of hd)
				then match_type ty_right (tail, res)
				else failwith (ste hd ^ " should be of type " ^sty ty_left ^ " but has type " ^ sty (ty_of hd))
			| _-> failwith "match_type::Type error" in
		
	let rec check_type ty = function
	|[ ] -> ( )
	|hd :: tail -> match_type ty hd; check_type ty tail in
	
	match test_cases with 
	|[] -> failwith "empty test case lists"
	|(lst , te) :: tail -> 
		let ty = make_ty lst (ty_of te) in
		check_type ty 	test_cases; ty 

let read_test_cases ps_file ctx = 
			let pi = open_in ps_file in 
			rtc (input_line pi); (*First line contains headings, skip*)
			let rec inner lst = 
				try  inner(rtc (input_line pi) lst ctx) 
				with _-> lst in 
			let test_cases = inner [ ] in 
			close_in pi; 
			test_cases, get_type ctx test_cases
			
			
let match_case ?(debug=false) te ctx (args, res) =
	let ste te = string_of_term ctx te Compact in
	let resLeft = 
		normalize ctx 
		(List.fold_left 
		(fun te_acc arg ->  TmApp(te_acc, arg))
		te args) in
	let comp = te_eqv ctx resLeft res in		
	if debug then 
		(prf ("Compared " ^ ste resLeft ^ " against " ^ ste res);
		prf ("\nReturned " ^ string_of_bool comp));
	comp
				
			
			
			
			
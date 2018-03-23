open Utils.Pervasive
open Utils.Error
open Utils.StringSpecial
open Format
open Syntax
open Binding
open PrintExpr
open Core
open Utils.ListSpecial


 
let rec process_command ?(verbose=false) ctx cmd =  

	let prbindingty ctx b = match b with
	| BoundedTyVarBind -> ()
	| TyVarBind -> ()
	| BoundedTeVarBind ty -> prf (":" ^ string_of_type ctx ty Alpha)
	| TeVarBind ty -> prf (":" ^ string_of_type ctx ty Alpha)
	| TyAbbBind ty  -> prf (" = " ^ string_of_type ctx ty Alpha)
	| TmAbbBind(t, tyT_opt) -> prf (": " ^ 
			(match tyT_opt with
				| None -> string_of_type ctx (typeof ctx t) Alpha
				| Some(tyT) -> string_of_type ctx tyT Alpha))

	in match cmd with
	(* Evaluator *)
	| Eval t -> begin
		match t with 
		| Te (te, _) ->
			let tyT = typeof ctx te in 
			let t' = normalize ctx te in
			if(verbose) then (
			prf (string_of_term ctx t' Alpha); 
			print_break 1 2;
			prf (": " ^ string_of_type ctx tyT Alpha))
		| Ty (ty, _) -> if verbose then (prf (string_of_type ctx ty Alpha))
		end;
		force_newline();
		ctx
  | Bind (x, bind) -> 
      let bind = check_binding ctx bind in 
      let bind' = eval_binding ctx bind in 
      if verbose then(
      pr x; pr " "; prbindingty ctx bind'; force_newline());
      add_binding ctx x bind'
  | _ -> failwith "process_command ctx cmd, bad binder"
  

let parse_file ctx inFile =
  let pi = open_in inFile in 
  let lexbuf = Lexer.create inFile pi
  in 
  let cmds,_ =
	begin
    try 
			Parser.toplevel 
			Lexer.main lexbuf 
			ctx
	with 
			Parsing.Parse_error -> 
			error (Lexer.info lexbuf) "Parse error"
   end
  in
  Parsing.clear_parser(); 
  close_in pi;  
  
  let g ctx c =  
    open_hvbox 0;
    let results = process_command ctx c in
    print_flush();
    results
  in
    List.fold_left g  ctx cmds




let parse_string str ctx =
		let string_of_expr  = str ^ ";" in 
		let temp_file_name =  "temp.txt" in 
		let temp_file_channel = open_out temp_file_name in
		output_string temp_file_channel string_of_expr;
		flush temp_file_channel;
		close_out temp_file_channel;
		let pi = open_in temp_file_name in 
		let lexbuf = Lexer.create temp_file_name pi in
		let cmds , _ =
			begin
			try 
			Parser.toplevel 
			Lexer.main lexbuf 
			ctx
			with 
			| Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
			end
		in
		Parsing.clear_parser(); 
		close_in pi;
		match cmds with
		| (Eval expr) :: [] -> expr
		|_->failwith "Illegal operation at GPUtils::parseString" 

    

let abstractOut ctx (x ,n) ter = 
	let rec replace_by_in x1 x2 ter = 
	match ter with 
	 | TmVar index when index = x1 -> TmVar x2 
	 | TmVar index -> TmVar index
	 (*| TmAbs (i,str,ty,ter) -> TmAbs (i, str, ty, replace_by_in (x1+1) (x2+1) ter)
     | TmApp (i, te1, te2)-> 
			 TmApp (i, replace_by_in x1 x2 te1, replace_by_in x1 x2 te2)
     | TmTAbs (i, str, ter) -> TmTAbs (i, str, replace_by_in (x1+1) (x2+1) ter)
     | TmTApp (i, ter, ty) -> TmTApp (i, replace_by_in x1 x2 ter, ty)*)
     | _ -> failwith( 
			 "Unhandled case in replace_by: " ^
			 string_of_term ctx  ter Alpha) 
	in

	let varList = get_TmVars_in_term ter in
	if List.exists (fun elt-> elt =	x) varList then 
		begin
			let var = TmVar x in 
			TmApp(
				TmAbs(typeof ctx var, replace_by_in (x+1) 0 (termShift 1 ter)),
				var)
		end
	else ter  
	
let make_constructors ctx ty =   
	let make_abs (name,ty) =  "lam " ^ name ^ " : " ^ (string_of_type ctx ty Alpha) ^ "." in
	match ty with 
	|TyArr (ty1,ty2) when is_data_type ctx ty1 ->
		(* inner_term (below) corresponds to the term	 *)
		(* (x [ty2]) in which x is a variable of type	 *)
		(* ty1                                           *)  
		let inner_term = TmTApp((TmVar 0), (typeShift 1 ty2)) in
		let inner_ctx, varName1 =  fresh_fvar ctx ty1 in
		let cons_types = get_constructor_arguments ctx ty2 (typeShift (-1) (typeof inner_ctx inner_term)) in  
		
		(* (typeof (x [ty2])) is a constructor for [ty2] *)
		(* each of the constructor arguments is          *)
		(* associated with a new function name           *)
		(* name variable                                 *)
		let names_types = (List.map (fun ty -> fresh_lcid( ),ty) cons_types) in 
		
		(* which allows the string form construction of  *)
		(* the common inner-bodies for all the           *)
		(* constructors                                  *)
		let final_inner_term = ref (string_of_term inner_ctx inner_term Alpha) in 
		List.iter (fun (name, _) -> final_inner_term := !final_inner_term ^ " " ^ name) names_types;
		final_inner_term := make_abs(varName1,ty1)^ !final_inner_term;
		
		(* All the permutations of constructors can now *)
		(* be constructed  in their string form         *)		
		let constructors = ref [] in
		List.iter (fun name_type_lst -> 
			constructors := 		
				(String.concat "" ((List.map make_abs name_type_lst) @ [!final_inner_term]))::
				!constructors)  (permutations names_types);
		
		(* and returned in their term representation    *)		
		List.map (fun str -> ter(parse_string str ctx))	 !constructors
	
	|_->failwith "non constructible type"



open Format

open Utils.Pervasive
open Utils.Error
open Utils.Symbols

open Syntax
open Binding
open Core

type str_rep_mode = Alpha | Num | Compact

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
  | TmVar _ -> true
  | _ -> false


let rec string_of_type ctx ty mode = 
	match mode, ty with
    | Compact, TyAll sub_ty -> 
		begin
			try name_of_expr ctx (Ty (ty, None)) 
			with | Not_found -> 
				let (ctx1, tyX) = fresh_tvar ctx  in
				s_Pi ^ " " ^ tyX ^ ". " ^ (string_of_type ctx1 sub_ty mode)
		end
		
	| Alpha, TyAll sub_ty ->
		let (ctx1, tyX) = fresh_tvar ctx  in
		s_Pi ^ " " ^ tyX ^ ". " ^ (string_of_type ctx1 sub_ty mode)	
		
	| Num, TyAll ty ->
		let (ctx1, tyX) = fresh_tvar ctx  in
		s_Pi ^ " " ^  ". " ^ (string_of_type ctx1 ty mode)
	
	| _, _ -> string_of_ArrowType ctx ty mode

and string_of_ArrowType ctx  ty mode =  
	match mode, ty with 
	| Compact,  TyArr(ty1, ty2) -> 
		begin try name_of_expr ctx (Ty (ty, None)) 
			with | Not_found -> 
			string_of_AType ctx ty1 mode ^
			"-> " ^ string_of_ArrowType ctx ty2 mode end
	|_, TyArr(ty1, ty2) -> 
		string_of_AType ctx ty1 mode ^
		"-> " ^
		string_of_ArrowType ctx ty2 mode
	| _ -> string_of_AType ctx ty mode

and string_of_AType ctx ty mode = 
	match ty with
	| TyVar x -> begin match mode with 
		| Alpha | Compact -> 
			let str = name_of_index ctx x in
			begin match get_binding ctx x with
			| BoundedTyVarBind ->  str 
			| TyVarBind -> str 
			| TyAbbBind _  -> str
			| _ -> failwith "String_of_AType -> TyVar match" end
		| Num -> string_of_int x end
    | TyString -> "String"
	| TyBool -> "Bool"
	| TyFloat -> s_TyFloat
	| TyNat -> "Nat"
	| TyInt -> s_TyInt
	| TyUnit -> s_TyUnit
	| _ -> "(" ^ string_of_type ctx ty mode ^ ")"


let rec string_of_term ctx ter mode = 
	let inner ~(alpha:bool) = 
	match ter with
	| TmAbs (ty, te) ->
		let (ctx', x') = fresh_fvar ctx ty in
		let abBody  = string_of_term ctx' te mode in
		let tyStr = string_of_type ctx ty mode in 
		s_lambda ^ " " ^ (if alpha then x' else "#") ^ ":" ^ tyStr ^ "." ^ abBody
	| TmTAbs ter ->
		let (ctx1, x) = fresh_tvar ctx in
            s_Lambda ^" " ^ 
            (if alpha then x else "") ^ 
            "." ^ string_of_term ctx1 ter mode
	| _ -> string_of_appterm ctx ter mode in
	
	match mode with
	| Compact  -> 
		begin try name_of_expr ctx (Te (ter, None))  
		with Not_found ->
		inner ~alpha:true end
	|Alpha ->	inner ~alpha:true
	|Num -> inner ~alpha:false
		
    
and string_of_appterm ctx ter mode= 
	let inner ( ) = 
		match ter with
		| TmApp (t1, t2) -> 
		string_of_appterm ctx t1 mode ^ " " ^
		string_of_aterm ctx t2 mode
		| TmTimesfloat (t1, t2) ->
		string_of_aterm ctx t1 mode ^ " *. " ^ string_of_aterm ctx t2 mode
		|TmPred (t1) ->
		failwith "Unsupported Let term string_of_term"
		| TmIsZero (t1) ->
		failwith "Unsupported Let term string_of_term"
		| TmTApp (ter, ty) ->
		string_of_appterm ctx ter mode ^ " " ^
		"[" ^ string_of_type ctx ty mode ^ "]"
		| _ -> string_of_aterm ctx ter mode 
	
	in match mode with
	| Compact  -> 
		begin try  name_of_expr ctx (Te (ter, None))  
		with Not_found ->
		let str = inner( ) in str end
	| _ ->	inner ()


and string_of_aterm ctx ter mode = 
	match ter with
	| TmVar x ->
	(match mode with 
		| Num -> string_of_int x
		|Compact | Alpha -> name_of_index ctx x)
	| TmString s -> "\"" ^ s ^ "\""
	| TmTrue -> s_TmTrue
	| TmFalse -> s_TmFalse
	| TmFloat s -> string_of_float s
	| TmInt n -> string_of_int n
	| TmZero  -> "TmZero"
	| TmSucc TmZero -> "TmSucc TmZero"
	| TmSucc t1 -> "TmSucc (" ^ string_of_term ctx t1 mode ^")" 
	| _ -> "(" ^ string_of_term ctx ter mode ^  ")"
 
let string_of_expr ctx expr mode = 
	match expr with 
	| Te (ter, _) -> string_of_term ctx ter mode
	| Ty (ty, _) -> string_of_type ctx ty mode
  
(*Context info***********************************************)
let ctx_synopsis ctx = 
	 let header i =
		let bind = get_binding ctx i in
		match bind with
		| BoundedTyVarBind -> [" a type variable"]
		| TyVarBind ->  [" an atomic type"]
		| BoundedTeVarBind ty -> [(" a term variable of type " ^ string_of_type ctx ty Alpha)]
		| TeVarBind ty -> [(" an atomic term of type " ^ string_of_type ctx ty Alpha)]
		| TyAbbBind ty -> [(" a name for the type " ^ string_of_type ctx ty Alpha)]
        | TmAbbBind( te , defTy )-> [(" a name for the term \n\t" ^ string_of_term ctx te Alpha) ;
			(" of deduced type from context: " ^ (string_of_type ctx ( typeof ctx te) Alpha));
				(match defTy with
				| None -> "of no defined type"
				|Some ty -> ("of defined type: " ^ string_of_type ctx ty Alpha))] in 
	  
	  let lst = ref [] in				
      let len = context_length ctx in
      for i=0 to len-1 do 
		lst := !lst @ [(name_of_index ctx i , header i)]
	  done;
	  !lst

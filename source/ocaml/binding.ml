open Utils.Pervasive
open Utils.Error
open Utils.ListSpecial
open Syntax
open Pointers

type binding =
  | BoundedTyVarBind 
  | TyVarBind
  | BoundedTeVarBind of ty
  | TeVarBind of ty
  | TyAbbBind of ty
  | TmAbbBind of term * (ty option)

type context = (string * binding) list

type command =
  | Eval of  t_Expr
  | Bind of string * binding
  | SomeBind of string * string * term

(* ---------------------------------------------------------------------- *)
(* Context management *)

let empty_context = []
let context_length ctx = List.length ctx

let is_name_bound ctx name =
	List.exists 
		(fun (y, _) -> if y = name then true else false)
		ctx
		

			
let add_binding ctx name bind = 
	(*if is_name_bound ctx name then
		failwith ("Syntax::add_binding, name: " ^ name ^ " already exists")
	else*) 
	(name, bind) :: ctx



let add_fvar_name ctx name ty = 
	add_binding ctx name (BoundedTeVarBind ty)

let add_tvar_name ctx x = 
	add_binding ctx x BoundedTyVarBind

let fresh_fvar ctx ty =
	let lc = String.make 1 (char_of_int ((Random.int 25) + 97)) in
	let rec help ctx ty lc = 
		if is_name_bound ctx lc 
		then help ctx ty (lc ^ "'")
		else  (add_fvar_name ctx lc ty),  lc in
	help ctx ty lc
		

let fresh_tvar ctx = 
	let uc = String.make 1 (char_of_int ((Random.int 25) + 65)) in
	let rec help ctx name = 
		if is_name_bound ctx name 
		then help ctx (name ^ "'")
		else (add_tvar_name ctx name),  name in
	help ctx uc

(*
let rec pick_fresh_name ctx x =
  if is_name_bound ctx x then pick_fresh_name ctx (x^"'")
  else add_binding ctx x BoundedTyVarBind, x
*)



let name_of_index ctx x = fst (List.nth ctx x)

let rec index_of_name ctx name =
  match ctx with
    |  [] -> failwith ("Identifier " ^ name ^ " is unbound")
    | (bind_name, _) :: rest ->
        if bind_name = name then 0
        else 1 + (index_of_name rest name)
        

let binding_shift d bind =
  match bind with
  | BoundedTyVarBind -> BoundedTyVarBind
  | TyVarBind -> TyVarBind
  | TyAbbBind ty -> TyAbbBind (typeShift d ty)
  
  | BoundedTeVarBind ty -> BoundedTeVarBind (typeShift d ty)
  | TeVarBind ty -> TeVarBind (typeShift d ty)
  | TmAbbBind (t, tyT_opt) ->
     let tyT_opt' = match tyT_opt with
                      None->None
                    | Some(tyT) -> Some(typeShift d tyT) in
     TmAbbBind(termShift d t, tyT_opt')
     
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec get_binding ctx i =
 let (_, bind) = List.nth ctx i in
    binding_shift (i+1) bind 
	
(*
let is_bounded_type ctx x = 
	match get_binding ctx x with
			| BoundedTyVarBind -> true
			| _ -> false
*)
			
let expr_of_name ctx name =  
	let ind = index_of_name ctx name in
	match get_binding ctx ind with
	| TyVarBind -> Ty (TyVar ind, Some name)
    | TeVarBind _ ->  Te (TmVar ind, Some name)
    | BoundedTeVarBind _ -> Te (TmVar ind, Some name)
	| TyAbbBind ty -> Ty (ty, Some name)
	| TmAbbBind (te , _) ->Te (te, Some name)
	| _ -> failwith ("\nInvalid name " ^ name ^ " at  Binding::expr_of_name")
    
let get_atomic_types ctx =
	let tys = ref [] in
	for i=0 to (context_length ctx) - 1 do
	match get_binding ctx i with
		| TyVarBind -> tys := !tys @ [TyVar i]
		| TyAbbBind _->  tys := !tys @ [TyVar i]
		|_->( )	
	done;
	!tys

let name_of_expr ctx expr = 
	
	let is_same_type ty1 ty2 = 
		Pointers.hash (Ty (ty1, None)) 3000 = Pointers.hash (Ty (ty2, None)) 3000 in
	let is_same_term te1 te2 = 
		Pointers.hash (Te (te1, None)) 3000 = Pointers.hash (Te (te2, None)) 3000 in


	let rec inner i ctx = 
	match ctx with 
	| [] -> raise Not_found
	| (str, bind) :: tail -> 
		match binding_shift (i+1) bind, expr with  
		| TyAbbBind ty1, Ty (ty2, _)  
		when is_same_type ty1 ty2 -> str   
		| TmAbbBind (te1, _), Te (te2,_) 
		when is_same_term te1 te2 -> str
		|_ , _ -> inner (i+1) tail
	in inner 0 ctx
	
	

let get_type ctx i =
   match get_binding ctx i with
     | BoundedTeVarBind ty -> ty 
     | TeVarBind ty -> ty
     | TmAbbBind (_, Some(tyT)) -> tyT
     | TmAbbBind (_, None) -> 
		 failwith ("No type recorded for variable " ^ (name_of_index ctx i))
     | _ -> 
        failwith 
        ("getTypeFromContext: Wrong kind of binding for variable " ^ (name_of_index ctx i))
         
       

let get_TmVars_in_term t =
  let mymerge = union (fun x y->x=y) in
  
  let rec inner t' top_level_begin = 
	match t' with
	| TmVar x when x >= top_level_begin ->
      [x - top_level_begin]
	
	| TmAbs (ty, t'') -> 
		inner t'' (top_level_begin + 1)
  
	| TmApp (t1, t2)->
        mymerge
          (inner t1 top_level_begin)
          (inner t2 top_level_begin)
  
	| TmTAbs t'' ->
		inner t'' (top_level_begin + 1)
  
	| TmTApp (t'', _) ->
		inner t'' top_level_begin
  
	| _ ->[]
  
	in inner t 0
 


       
let get_atomic_types_in_expr ctx expr =  
   let merge = union (fun ty1 ty2 -> ty1=ty2) in
	
	let rec get_atomic_types_in_ty ctx_len ty beg_top_ind =  
	match ty with  
	| TyVar x  when x >= beg_top_ind ->  
		let x = x - beg_top_ind in 
		(*let at1 = TyVar(x , n) in 
		let at2 = unpack_ty ctx at1 in 
154                                 merge  *)
		[TyVar  x]  
(*             begin 
157             if (at1 = at2) then [] 
158             else get_atomic_types_in_ty ctx_len at2 beg_top_ind 
159             end *)
	| TyArr (ty1 , ty2) ->  
		merge 
			(get_atomic_types_in_ty ctx_len ty1 beg_top_ind) 
			(get_atomic_types_in_ty ctx_len ty2 beg_top_ind) 
	| TyAll ty -> 
		get_atomic_types_in_ty (ctx_len + 1) ty (beg_top_ind + 1) 
	| _->[] 
	in 

	match expr with  
	|Ty (ty , _) ->  
		get_atomic_types_in_ty (context_length ctx) ty 0 
	|_->[]  
	



         

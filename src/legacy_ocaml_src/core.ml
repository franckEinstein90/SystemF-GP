open Utils.Error
open Utils.Pervasive
open Syntax
open Binding

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec is_nat ctx t = match t with
  |	TmZero  -> true
  |	TmSucc t1 -> is_nat ctx t1
  |	TmPred t1  -> is_nat ctx t1
  |	_ -> false


let rec is_numeric_val ctx t = match t with
  | TmFloat _  -> true
  | TmInt _ -> true
  |	TmZero  -> true
  |	TmSucc t1 -> is_numeric_val ctx t1
  |	TmPred t1  -> is_numeric_val ctx t1
  |	_ -> false


let rec is_val ctx t = match t with
  | t when is_numeric_val ctx t  -> true 
  | TmString _  -> true
  | TmVar _ ->true
  | TmTrue   -> true
  | TmFalse -> true
  | TmUnit -> true 
  | TmAbs (ty, te) -> 
		let ctx, _ = fresh_fvar ctx ty in
		is_val ctx te
  | TmTAbs te -> 
		let ctx, _ = fresh_tvar ctx in
		is_val ctx te
  | _ -> false


let varHandler x ctx = 
	match name_of_index ctx x with 
	| "zero" -> TmInt 0
	| "one" -> TmInt 1
	|_ -> 
		match get_binding ctx x with
		| TmAbbBind (t, _) -> t 
		| _ -> raise NoRuleApplies
    
  
let rec eval1 ctx t = match t with
	|  TmAbs (ty, te) -> 
		 let ctx , _ = fresh_fvar ctx ty  in
		 TmAbs (ty,  (eval1 ctx te)) 
	| TmTAbs te -> 
		let ctx, _ = fresh_tvar ctx in
		TmTAbs (eval1 ctx te)

	
   | TmApp(TmApp (TmVar x, TmString str1), TmString str2)   
		when name_of_index ctx x = "concat" -> 
				TmString (str1 ^ str2)
 
   | TmApp(TmApp (TmVar x, TmInt n1), TmInt n2)   
		when name_of_index ctx x = "plus" -> 
				TmInt (n1 + n2)
				
  | TmApp(TmApp (TmVar x, TmInt n1), TmInt n2)   
		when name_of_index ctx x = "mult" -> 
				TmInt (n1 * n2)

  |  TmApp (TmAbs (ty, te), v2) ->
		termSubstTop v2 te
  | TmApp (t1, t2) ->
      begin 
      try TmApp (eval1 ctx t1, t2) 
      with NoRuleApplies ->
			TmApp (t1, eval1 ctx t2) 
      end
  
  | TmIf (TmTrue, t2, t3) -> t2
  | TmIf (TmFalse, t2, t3) -> t3
  | TmIf (t1, t2, t3) -> TmIf (eval1 ctx t1, t2, t3)
      
  | TmTimesfloat (TmFloat f1, TmFloat f2) ->
      TmFloat (f1 *. f2)
  | TmTimesfloat (TmFloat f1 as t1, t2) ->
      TmTimesfloat (t1, eval1 ctx t2) 
  | TmTimesfloat (t1, t2) ->
      TmTimesfloat(eval1 ctx t1, t2) 
    
    (* Natural numbers (Peano arithmetic) *)
	| TmSucc t1 -> TmSucc (eval1 ctx t1)
	| TmPred TmZero  -> TmZero
	| TmPred (TmSucc nv1) when (is_nat ctx nv1) -> nv1
	| TmPred t1 -> TmPred (eval1 ctx t1)
		 

	| TmIsZero TmZero  -> TmTrue 
	| TmIsZero (TmSucc nv1) when (is_numeric_val ctx nv1) ->
      TmFalse
	| TmIsZero t1 -> TmIsZero (eval1 ctx t1)
	 
	 			
	| TmVar x -> varHandler x ctx 
  
	| TmTApp (TmTAbs te, ty) -> tytermSubstTop ty te
  
	| TmTApp (t1, tyT2) -> TmTApp (eval1 ctx t1, tyT2)
  
	| _ -> raise NoRuleApplies

and normalize ctx t =
  try let t' = eval1 ctx t
      in normalize ctx t'
  with NoRuleApplies -> t

let istyabb ctx index = 
  match get_binding ctx index with
  |  TyAbbBind (tyT) -> true
  | _ -> false

let gettyabb ctx index = 
  match get_binding ctx index with
  | TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec compute_ty ctx tyT = 
  match tyT with
  | TyVar index when istyabb ctx index -> gettyabb ctx index
  | TyArr(ty1, ty2) -> 
		let ty1' = (try compute_ty ctx ty1 with _->ty1) in
		let ty2' = (try compute_ty ctx ty2 with _->ty2) in
		if TyArr(ty1,ty2) = TyArr(ty1',ty2') 
		then raise NoRuleApplies
		else 	TyArr(ty1',ty2') 
  | _ -> raise NoRuleApplies

let rec simplify_ty ctx tyT =
  try
    let tyT' = compute_ty ctx tyT in
    simplify_ty ctx tyT' 
  with NoRuleApplies -> tyT

let simplify_expr ctx expr = 
	match expr with 
	| Ty (ty, name) -> Ty (simplify_ty ctx ty, name)
	| Te (te, name) -> Te (normalize ctx te, name)


let rec ty_eqv ctx tyS tyT =
  let tyS = simplify_ty ctx tyS in
  let tyT = simplify_ty ctx tyT in
  if (tyS = tyT) then true else
  match (tyS, tyT) with
  | (TyVar index, _) when istyabb ctx index ->
      ty_eqv ctx (gettyabb ctx index) tyT
  | (_, TyVar index) when istyabb ctx index ->
      ty_eqv ctx tyS (gettyabb ctx index)
  | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) ->
       (ty_eqv ctx tyS1 tyT1) && (ty_eqv ctx tyS2 tyT2)
  | (TyAll  ty1, TyAll ty2) ->
       let ctx1, _ = fresh_tvar ctx in
       ty_eqv ctx1 ty1 ty2
  | _ -> false
let rec type_to ctx ty_dest ty_env =
	match ty_env with
		| _ when ty_eqv ctx ty_dest ty_env -> true  
		| TyArr (_, ty2) -> type_to ctx ty_dest ty2
		|_ -> false
let rec te_eqv ctx teS teT =  
	normalize ctx teS =  normalize ctx teT  
let rec ex_eqv ctx ex1 ex2 =  
	match (ex1, ex2) with 
	| Ty (ty1,_) , Ty (ty2,_) ->  ty_eqv ctx ty1 ty2 
	| Te (te1,_) , Te (te2,_) ->  te_eqv ctx te1 te2 
	| _ -> false 


(* ------------------------   TYPING  ------------------------ *)
let rec typeof ctx t =
	match t with
	| TmVar index ->  simplify_ty ctx (get_type ctx index)
	
	| TmAbs (ty_left, te) ->
		let ctx', _ = fresh_fvar ctx ty_left in 
		let ty_right = typeof ctx' te in
		TyArr(ty_left, typeShift (-1) ty_right)
		
	
	| TmApp (t1, t2) ->
	  let tyT1 = simplify_ty ctx (typeof ctx t1) in
      let tyT2 = simplify_ty ctx (typeof ctx t2) in
      begin match tyT1 with
         | TyArr (tyT11, tyT12) ->
				if ty_eqv ctx tyT2 tyT11 then tyT12
				else failwith "parameter type mismatch"
         | _ -> failwith "arrow type expected"
      end 
  
	| TmString _ -> TyString
	| TmUnit -> TyUnit
	
	| TmTrue -> TyBool
	| TmFalse  -> TyBool
	| TmIf (t1, t2, t3) ->
		if ty_eqv ctx (typeof ctx t1) TyBool then
       let tyT2 = typeof ctx t2 in
       if ty_eqv ctx tyT2 (typeof ctx t3) then tyT2
		else failwith "arms of conditional have different types"
		else failwith "guard of conditional not a boolean"
  
	| TmFloat _ -> TyFloat
	| TmInt _ -> TyInt  
	| TmZero  -> TyNat
  
  | TmTimesfloat (t1, t2) ->
      if ty_eqv ctx (typeof ctx t1) TyFloat
      && ty_eqv ctx (typeof ctx t2) TyFloat then TyFloat
      else failwith "argument of timesfloat is not a number"
 
  | TmSucc (t1) ->
      if ty_eqv ctx (typeof ctx t1) TyNat then TyNat
      else failwith "argument of succ is not a number"
  | TmPred (t1) ->
      if ty_eqv ctx (typeof ctx t1) TyNat then TyNat
      else failwith "argument of pred is not a number"
  | TmIsZero (t1) ->
      if ty_eqv ctx (typeof ctx t1) TyNat then TyBool
      else failwith "argument of iszero is not a number"
 | TmTAbs te ->
      let ctx = add_binding ctx "X" TyVarBind in
      let ty = typeof ctx te in
      TyAll ty
  | TmTApp (te, ty_right) ->
      let ty_left = typeof ctx te in
      begin 
		match simplify_ty ctx ty_left with
         |TyAll ty -> typeSubstTop ty_right ty
         | _ -> failwith "Core::typeof universal type expected"
		end

let eval_binding ctx bind = 
	match bind with
	|TmAbbBind(t, tyT) -> 
		let t' = normalize ctx t in 
		TmAbbBind(t', tyT)
	| _ -> bind
  
let check_binding ctx bind = 
	match bind with
	| TmAbbBind(t,None) -> TmAbbBind(t, Some(typeof ctx t))
	| TmAbbBind(t,Some(tyT)) ->
		let tyT' = typeof ctx t in
			if ty_eqv ctx tyT' tyT then TmAbbBind(t,Some(tyT))
		else failwith "Type of binding does not match declared type"
	|_ -> bind     

(*Type analysis**********************************************)
let rec is_constructor_for ctx constructed_type function_type =
	match function_type with
		| _ when ty_eqv ctx constructed_type function_type -> true  
		| TyArr (_, ty2) -> is_constructor_for ctx constructed_type ty2
		|_ -> false		
		
let rec get_constructor_arguments ctx constructed_type function_type =
	match function_type with
		| _ when ty_eqv ctx constructed_type function_type -> []  
		| TyArr (tyLeft, ty2) when is_constructor_for ctx constructed_type ty2 ->
			[tyLeft] @ (get_constructor_arguments ctx constructed_type ty2)			
		|_ -> failwith "Invalid Arguments"				
		
let rec is_data_type ctx ty = 
	let rec inner ctx tyVarName tyLeft tyRight = 
		let tyVar = TyVar (index_of_name ctx tyVarName) in
		match (is_constructor_for ctx tyVar tyLeft, tyRight) with
		|false, _ -> false
		|true, _ when ty_eqv ctx tyVar tyRight -> true
		|true, (TyArr (tyLeft, tyRight))->
			inner ctx tyVarName tyLeft tyRight
		|_->false
	
	in	match ty with 
	|TyVar index when istyabb ctx index ->
		is_data_type ctx (gettyabb ctx index)
	|TyAll (TyArr (tyLeft, tyRight)) -> 
		let (ctx, tyVarName) = fresh_tvar ctx	in
		inner ctx tyVarName tyLeft tyRight
	|_->false

let rec is_constructible_type ctx ty =
	match ty with 
	|TyArr (ty1,_) when is_data_type ctx ty1 -> true
	|_->false

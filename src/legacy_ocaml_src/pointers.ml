open Syntax

let rec complexity_of_expr expr = 
	let cTy ty = complexity_of_expr (Ty (ty, None)) in
	let cTe te = complexity_of_expr (Te (te, None)) in
	 
	match expr with 
	|Ty (ty, _) -> begin match ty with  
		| TyVar  _ -> 1 
		| TyArr (ty1 , ty2) -> 1 + (cTy ty1) + (cTy ty2)
		| TyAll ty -> 1 + cTy (ty) 
		| TyNat -> 1 
		| TyFloat -> 1
		| TyString -> 1
		| TyBool -> 1
		| TyInt -> 1
		| TyUnit -> 1 end
		
	|Te (ter, _) ->  begin match ter with 
		| TmVar _ -> 1
		| TmAbs (ty, ter) -> 1 + cTy ty + cTe ter  
		| TmApp (ter1, ter2) -> 1 + cTe ter1 + cTe ter2
		| TmTAbs ter -> 1 + cTe ter
		| TmTApp (ter,  ty) -> 1 + cTe ter + cTy ty 
		| TmString _ -> 1
		| TmTrue -> 1
		| TmFalse -> 1
		| TmIf (te1, te2, te3) -> 1 + cTe te1 + cTe te2 + cTe te3
		
		(* Numerical values *)
		| TmFloat _ -> 1
		| TmInt _ -> 1 
		| TmZero -> 1
		| TmTimesfloat (te1, te2) -> 1 + cTe te1 + cTe te2
		| TmSucc te -> 1 + cTe te
		| TmPred te -> 1 + cTe te
		| TmIsZero te -> 1 + cTe te
		| TmUnit -> 1 end
		
		
let hash expr precision = 
	Hashtbl.hash_param precision precision
	(match expr with 
	|Ty (ty,_) ->  Ty(ty, None)
	|Te(te,_) ->  Te(te, None))
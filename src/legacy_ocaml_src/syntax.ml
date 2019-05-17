open Format
open Utils.Error
open Utils.Pervasive
open Utils.Symbols

  
  
  
(* ---------------------------------------------------------------------- *)
(* Datatypes *)

(* Data type definitions *)
type ty =
  | TyVar of int 
  | TyArr of ty * ty
  | TyAll of ty
  
  | TyString
  | TyBool
  
  (* Numerical types *)
  | TyFloat
  | TyNat
  | TyInt
  | TyUnit 

type term =
  | TmVar of int 
  | TmAbs of ty * term
  | TmApp of term * term  
  | TmTAbs of  term
  | TmTApp of  term * ty
  
  | TmString of  string
  | TmTrue 
  | TmFalse 
  | TmIf of term * term * term
  
  (* Numerical values *)
  | TmFloat of  float
  | TmInt of int 
  | TmZero 
   
  | TmTimesfloat of  term * term
 
  | TmSucc of term
  | TmPred of  term
  | TmIsZero of  term

  | TmUnit

(**************************
 
 About expressions 
 
 **************************)
type t_Expr =   
	| Ty of ty * string option 
	| Te of term * string option 
	
let small t = 
  match t with
  | TmVar _ -> true
  | _ -> false


let rec string_of_type ty = 
	match ty with
    | TyAll ty -> s_Pi ^ ". " ^ string_of_type ty 
	| _ -> string_of_ArrowType ty 

and string_of_ArrowType ty = 
	match ty with 
    | TyArr(ty1, ty2) ->
		string_of_AType ty1 ^
		"-> " ^ string_of_ArrowType ty2 
   | _ -> string_of_AType ty

and string_of_AType ty = 
	match ty with
	| TyVar x -> string_of_int x
    | TyString -> "String"
	| TyBool -> "Bool"
	| TyFloat ->"Float"
	| TyNat -> "Nat"
	| TyInt -> s_TyInt
	| TyUnit -> s_TyUnit
	| _ -> "(" ^ string_of_type ty ^ ")"

let rec string_of_term ter = 
	match ter with
    | TmAbs (ty, te) -> s_lambda ^  ":" ^
         string_of_type ty ^ "." ^
         string_of_term te 
	| TmTAbs ter ->
		s_Lambda ^"." ^
            string_of_term ter
	| _ -> string_of_appterm ter

and string_of_appterm ter = 
	match ter with
	| TmApp (t1, t2) ->
		string_of_appterm t1 ^ " " ^
		string_of_aterm t2 
	| TmTimesfloat (t1, t2) ->
	  string_of_aterm t1 ^ " *. " ^ string_of_aterm t2 
	| TmPred (t1) ->
       failwith "Unsupported Let term string_of_term"
	| TmIsZero (t1) ->
        failwith "Unsupported Let term string_of_term"
	| TmTApp (ter, ty) ->
      string_of_appterm ter ^ " " ^
      "[" ^ string_of_type ty ^ "]"
	| _ -> string_of_aterm ter 


and string_of_aterm ter = 
	match ter with
	| TmVar x -> string_of_int x
	| TmString s -> "\"" ^ s ^ "\""
	| TmTrue -> s_TmTrue
	| TmFalse -> s_TmFalse
	| TmFloat s -> string_of_float s
	| TmInt n -> string_of_int n
	| TmZero  -> "TmZero"
	| TmSucc TmZero -> "TmSucc TmZero"
	| TmSucc t1 -> "TmSucc (" ^ string_of_term t1 ^")" 
	| _ -> "(" ^ string_of_term ter ^  ")"
 

let deb_of_expr expr = match expr with 
	| Te (ter, _) -> string_of_term ter 
	| Ty (ty, _) -> string_of_type ty 
  


let rec is_equal expr1 expr2 = 
	match expr1, expr2 with
	| Ty (ty1, _), Ty (ty2, _) -> ty1 = ty2
	| Te(te1, _), Te(te2, _) -> te1 = te2
	|_ -> false


(*utils*)  
let ter expr = match expr with   
	| Te (te,_) -> te  
	| _ -> failwith "Unable to produce term from expression"   
  
let typ expr = match expr with  
	| Ty (ty,_) -> ty  
	| _ ->failwith "Unable to produce type from expression"  



(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tymap onvar c tyT = 
  let rec walk c tyT = match tyT with
  | TyVar x -> onvar c x 
  | TyString -> TyString
  
  | TyFloat -> TyFloat  
  | TyNat -> TyNat
  | TyInt -> TyInt
  
  | TyBool -> TyBool

  | TyUnit -> TyUnit
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyAll ty -> TyAll (walk (c+1) ty)
  in walk c tyT

let tmmap onvar ontype c t = 
  let rec walk c t = match t with
	| TmVar x -> onvar c x 
	| TmAbs (tyT1, t2) -> TmAbs (ontype c tyT1, walk (c+1) t2)
	| TmApp(t1,t2) -> TmApp(walk c t1, walk c t2)
	| TmString _ as t -> t
  
	| TmIf (t1,t2,t3) -> TmIf (walk c t1,walk c t2,walk c t3)
	| TmTrue  -> TmTrue 
	| TmFalse -> TmFalse
	
	| TmUnit -> TmUnit
	 
	| TmFloat _ as t -> t
	| TmInt _ as t -> t
	| TmZero -> TmZero 
	
	
	| TmTimesfloat (t1 , t2) -> TmTimesfloat (walk c t1, walk c t2)
	| TmSucc (t1)   -> TmSucc (walk c t1)
	| TmPred (t1)   -> TmPred (walk c t1)
	| TmIsZero (t1) -> TmIsZero (walk c t1)
	| TmTAbs t2 -> TmTAbs (walk (c+1) t2)
	| TmTApp (t1,tyT2) -> TmTApp (walk c t1,ontype c tyT2)
  
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x  -> if x >=c then TyVar (x+d) else TyVar x)
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun c x  -> if x >=c then TmVar (x+d) else TmVar x)
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT


(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun j x  -> if x = j then termShift j s else TmVar x )
    (fun j tyT -> tyT)
    j t


let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x -> if x=j then (typeShift j tyS) else (TyVar x))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap 
	(fun c x -> TmVar x)
    (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
  
  
let rec term_replace src dest env =  
	if env = src then dest
	else match env with
	| TmAbs (ty, te) -> TmAbs (ty, term_replace (termShift 1 src) (termShift 1 dest) te)
	| TmApp (te1, te2)->TmApp (term_replace src dest te1, term_replace src dest te2)
	| TmTAbs te -> TmTAbs (term_replace (termShift 1 src) (termShift 1 dest) te)
	| TmTApp (te, ty)->TmTApp (term_replace (termShift 1 src) (termShift 1 dest) te, ty)
	| TmIf (te1, te2, te3)-> TmIf
			(term_replace (termShift 1 src) (termShift 1 dest) te1,
			term_replace (termShift 1 src) (termShift 1 dest) te2,
			term_replace (termShift 1 src) (termShift 1 dest) te3)
	| TmTimesfloat (te1, te2)-> TmTimesfloat 
			(term_replace (termShift 1 src) (termShift 1 dest) te1,
			term_replace (termShift 1 src) (termShift 1 dest) te2)
	|_->env  
	

  
  


(* module Syntax: syntax trees and associated support functions *)

open Utils.Pervasive
open Utils.Error

	 
  


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
  
type t_Expr =   
	| Ty of ty * string option
	| Te of term * string option
  

val deb_of_expr : t_Expr->string

(* Shifting and substitution *)
val typeShiftAbove: int->int->ty->ty
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term



(*utils*)  
val ter  : t_Expr->term  
val typ  : t_Expr->ty  


val term_replace : term->term->term->term




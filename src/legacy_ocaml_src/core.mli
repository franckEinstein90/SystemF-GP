(* module Core

   Core typechecking and evaluation functions
*)

open Utils.Error
open Syntax
open Binding

exception NoRuleApplies
val normalize : context -> term -> term 
val typeof : context -> term -> ty

val ty_eqv : context -> ty -> ty -> bool
val type_to : context->ty->ty->bool
val te_eqv : context -> term -> term -> bool
val ex_eqv : context -> t_Expr -> t_Expr -> bool 


val simplify_ty : context -> ty -> ty
val simplify_expr : context -> t_Expr -> t_Expr

val eval_binding : context -> binding -> binding 
val check_binding : context -> binding -> binding

(*Type analysis **************************************)
val is_constructor_for : context->ty->ty->bool
val get_constructor_arguments : context->ty->ty->ty list
val is_data_type : context->ty->bool
val is_constructible_type : context->ty->bool
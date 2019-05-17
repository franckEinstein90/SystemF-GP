open Utils.Error 
open Syntax 
  
type binding =
  | BoundedTyVarBind 
  | TyVarBind
  | BoundedTeVarBind of ty
  | TeVarBind of ty
  | TyAbbBind of ty
  | TmAbbBind of term * (ty option)

type command =
  | Eval of t_Expr
  | Bind of string * binding
  | SomeBind of string * string * term


(****************************
Context and context operations 
*****************************) 
type context
val context_length : context->int
val empty_context : context 
val is_name_bound : context->string->bool


val add_fvar_name : context->string->ty->context
val add_tvar_name : context->string->context 

val fresh_fvar : context->ty->(context * string)
val fresh_tvar : context->(context * string)

val add_binding : context -> string -> binding -> context
(*val add_name: context -> string -> context*)

val name_of_index : context -> int -> string
val index_of_name : context -> string -> int

val get_atomic_types : context -> ty list
val get_binding : context -> int -> binding
val get_type : context -> int -> ty
val get_TmVars_in_term : term-> int list
val get_atomic_types_in_expr : context->t_Expr->ty list 


val expr_of_name : context->string->t_Expr
val name_of_expr : context->t_Expr->string

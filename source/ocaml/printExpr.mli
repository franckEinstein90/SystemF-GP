open Syntax
open Binding

type str_rep_mode = Alpha | Num | Compact


val string_of_type : context->ty->str_rep_mode->string
val string_of_term : context->term->str_rep_mode->string
val string_of_expr : context->t_Expr->str_rep_mode->string

(*Context synopsis************************************)
val ctx_synopsis : context -> (string * string list) list

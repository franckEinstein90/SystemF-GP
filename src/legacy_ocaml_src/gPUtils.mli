open Syntax
open Binding





val parse_file : context->string->context 
val parse_string : string->context->t_Expr 

(* operations on expressions *)
val abstractOut : context->(int * int)->term->term
val make_constructors : context->ty->term list

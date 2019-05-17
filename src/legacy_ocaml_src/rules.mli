open Syntax
open Binding
open BlockPopulation
open Array


type formation_rule = context->gene_pool->t_Expr->t_Expr list
type constraint_function = gene_pool->t_Expr->bool	
type formation_pred = context->gene_pool->t_Expr->bool

val new_subset	: ?debug:bool -> ?max_trials:int 
					-> ?constraintFunc:constraint_function
					->context ->gene_pool->t_Expr
					-> formation_rule  list
					->t_Expr list 
					

type rule_app = {
	rule : formation_rule;
	accept : formation_pred;
	mutable app:bool}
	
val rules : rule_app array
val get_applicable_rules : context -> gene_pool -> t_Expr -> formation_rule list

type grow_unit = Block | Complexity of int	
val growToComplexity : 
	?debug:bool -> ?growUnit:grow_unit ->
	gene_pool->constraint_function->
	?onSuccess:(gene_pool -> t_Expr list -> unit)->
	?onFailure:(string->unit) -> unit
	

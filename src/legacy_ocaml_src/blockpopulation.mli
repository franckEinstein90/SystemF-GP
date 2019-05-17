open Format
open Utils.Error
open Utils.Pervasive
open Utils.RandSelect
open Syntax
open Pointers
open Binding
open Core
open PrintExpr
open GPUtils

exception BlockConstructionFailure

module Gene : sig
	type alleCategory = 
		| Kernel 
		| NoSpec	
		| GoalTypeConstructor 
		| LeftCombinator
		| RightCombinator
		
	type allele 
	
	(*************************************)
	(* Utils *****************************)
	(*************************************)
	val  alleListFold : ?f:('a -> allele ->'a) ->'a -> allele list -> 'a
	
	
	(*************************************)
	(* Attribute values ******************)
	(*************************************)
	val alleProtected : allele -> bool
	val alleComplexity : allele -> int
	val	alleFitness : allele -> float 
	val alleExpr : allele -> term	 
	
	val  alleId : allele -> int
	val  alleHasId :  int->allele-> bool
	
	val	 alleSetFitness : allele -> float -> unit
	
	val  alleNormalizeFitness : allele -> unit
	
	val	 alleSelectionProb : allele -> float
	val	 alleSetSelectionProb : allele -> float -> unit
	
	
	val  alleSeedSelectionCount : allele -> int	
	val  alleUseInPopulation : allele -> int
	val	 alleIncUseInPop : allele -> unit
	val  alleIsCarried : allele -> bool
	val  alleScoreOfBestCarrier : allele -> float
	val	 alleSetScoreBestCarr : allele -> float -> unit
end
		
open Gene
type categoryInfo = {
	mutable species_use : int;
	mutable totalFitness : float;
	mutable next_id : int
	}
type gene_pool

(*Essentials **************************)
val make : context ref -> ty -> term list -> term list -> gene_pool 
val insert_expression : gene_pool -> term -> (ty * int) option -> alleCategory -> unit	
val gpContext : gene_pool -> context

(*Utils      **************************)
val gpIter : ?f:(ty -> categoryInfo -> allele list->unit)->gene_pool->unit
val gpFold : ?f:(ty -> (categoryInfo * allele list) ->'a -> 'a) -> gene_pool -> 'a -> 'a 
val seedSelect : gene_pool -> allele	
val reset_values : gene_pool->unit

(*Removal    **************************)
val gpRemoveGene : gene_pool->ty->unit
val gpRemoveUnusableGenes : gene_pool  -> unit
val gpRemoveUnusedAlleles : gene_pool  -> unit

(*********************************************)
(*Information on genes/alleles ***************)
(*********************************************)
val gpAlleleIdExists : gene_pool -> ty -> int -> bool
val gpAlleleAvgFitness : gene_pool -> float 

val replace_type : gene_pool->ty->categoryInfo option->allele list option->unit

val geneExists : gene_pool->ty->bool
val gpAllelesofGenes : gene_pool->ty->(categoryInfo * allele list)


val alleleExists : gene_pool-> term -> bool


val teFind : gene_pool-> term -> allele


val get_alleles : gene_pool -> allele list 
val get_types : ?pred:(ty->bool) -> gene_pool -> ty list 
val get_terms : ?predTe:(term->bool)-> ?predTy:(ty->bool)->gene_pool->term list
val get_term : gene_pool->ty->int->term

 
val gpCount : ?pred:(ty->bool) -> gene_pool->int
val comp : gene_pool->int


val gpAssignSelectionProbs : gene_pool -> unit

val rand_ty : gene_pool->ty
val rand_te : ?ty_opt:(ty option)->gene_pool->term
val rand_func_from :  gene_pool -> ty -> term
val rand_func_to : gene_pool -> ty -> term
val rand_expr : gene_pool -> t_Expr


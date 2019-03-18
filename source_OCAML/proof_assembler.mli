open Syntax
open Hashtbl
open BlockPopulation
open Binding


type species =  
	| Partial of ty * species option
	| ProgNode of ty 
	| ForAllElim of species * ty
	| ImplyElim of species * species


type genotype =  
	| In of int 
	| TypeFunc of genotype * ty 
	| ObjFunc of genotype * genotype

val proofEquiv : context->species->species->bool
val check_tree : context -> species -> ty
val exists : context -> species list -> species -> bool 
val is_complete : species->bool
val width : context->ty->ty->int

val string_of_proof : ?output:string -> ?separator:string -> ?line_break:string 
	-> context -> species -> string
val dotGraphs : context -> species list -> string
	
val gpGenotypesOfSpecies : gene_pool->species->genotype list	

val term_of_program : ?debug:bool->gene_pool->species->genotype->term
val prog_num : gene_pool->species list->int


val string_of_program : gene_pool -> (species * genotype) -> string 

val gpeGpeStr : ?separator:string -> gene_pool -> genotype -> string
val gpePotentialNum : gene_pool -> species -> int

val gtypeEquiv : context -> genotype -> genotype -> bool
val gtypeGenotypesWithinDistance : gene_pool -> species -> genotype -> int -> genotype list
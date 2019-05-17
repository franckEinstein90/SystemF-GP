open Hashtbl
open Syntax
open BlockPopulation
open ProofAssembler
open TestCases
open Rules

				
(**********************************************)
(*Status Specific Info ************************)
(**********************************************)
type generation_num = int
type genoStatus

type speciesInfo = {
	speciesId : int;
	ancesterID : int option
	}

	
type population = {
	mutable info : speciesInfo;
	mutable contains_solution : bool;
	mutable popFitness : float;
	genotypes : (genotype, genoStatus) t 
	}

(**********************************************)
(*Utils ***************************************)
(**********************************************)
val popCount : ?pred:(genotype->genoStatus->bool) -> population -> int 
val popIter : ?pred:(genotype->genoStatus->bool)-> ?f:(genotype->unit)->population->unit
val popFold : population -> (genotype -> genoStatus -> 'a -> 'a) -> 'a -> 'a

(**********************************************)
(*Predicates **********************************)
(**********************************************)
val popExists : population -> genotype -> bool
val popIsAlive : population -> genotype -> (float * generation_num) option
val popAvgAliveFitness : population -> float


val popKill : population -> genotype -> unit

val popGenotypesStatus : population->(genotype -> genoStatus -> bool) -> genotype list

val popNew : int->int option->population


val popSetFitness : population->float->unit
val popFitness : population->float

val popEmptyInfo : int -> int option -> speciesInfo


val popIdInfo : population -> (int * int option)

val popRemove : ?pred:(genotype->genoStatus->bool) -> population -> unit 
val popInsertGenotype : population -> genotype -> genoStatus -> unit

val popSetInfo : population -> speciesInfo -> unit
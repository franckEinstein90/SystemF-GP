open Syntax
open Binding
open BlockPopulation
open ProofAssembler
open Population 
open TestCases
open Rules

type ecoSetting = {
	maxProofWidth : int;
	aprNumberOfLiveGenotypes : int;
	aprGenePoolTotComp : int;
	selectSize : int;
	selectReap : int
	}
type ecosystem


val ecoContext : ecosystem -> context
val ecoSpecies : ecosystem -> species list
val ecoHasSolution : ecosystem -> bool
val ecoPopulation : ecosystem -> species ->	population

(********************************************************)
(* Fitness analisys *************************************)
(********************************************************)
val ecoMaxPossibleScore : ecosystem -> float
val ecoAvgGtyeScore : ecosystem -> float

val ecoGenes : ecosystem -> gene_pool
val ecoGenerationNumber : ecosystem -> int
val ecoGoalType : ecosystem -> ty 
val ecoStats : ecosystem -> string
val ecoSpeciesId : ecosystem->species->(int * int option) 
val ecoSpeciesFromId : ecosystem->int->species



val ecoCount : ?pred:(genotype->genoStatus->bool) -> ecosystem -> int
val empty_eco : gene_pool ref -> ecosystem
val ecoIsPossibleGenotype : ecosystem -> species -> genotype -> bool

	
val make_eco : ?debug:bool -> gene_pool ref->ty->test_case list->ecoSetting -> ecosystem
val ecoFirstGeneration : ecosystem -> constraint_function->int ref->term option
val ecoNextGeneration : ecosystem -> constraint_function->int ref->term option		  


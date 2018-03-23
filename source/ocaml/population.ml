open Utils.Pervasive
open Utils.ListSpecial
open Utils.RandSelect
open Utils.Statistics
open Utils.StringSpecial

open Hashtbl
open Syntax
open Binding
open Core
open PrintExpr

open BlockPopulation.Gene 
open BlockPopulation 

open ProofAssembler
open Rules
open TestCases
open List


type popSize = Max | MaxProportion of float | Fixed of int



type speciesInfo = {
	speciesId : int;
	ancesterID : int option;
	}
let popEmptyInfo id ancesterID = {
	speciesId = id;
	ancesterID = ancesterID;
	}

(**********************************************)
(*Status Specific Info ************************)
(**********************************************)
type generation_num = int
type genoStatus = 
	|Dead of (float * generation_num)
	|Alive of (float * generation_num) 


type population = {
	mutable info : speciesInfo;
	mutable contains_solution : bool;
	mutable popFitness : float;
	genotypes : (genotype, genoStatus) t 
	}
let popNew id ancesterID = {
	info = popEmptyInfo id ancesterID;
	popFitness = 0.;
	contains_solution =false;
	genotypes = create 200
	}
	
let popCount ?(pred=(fun _ _ -> true))  population = 
	Hashtbl.fold 
	(fun genotype status acc -> if pred genotype status then acc + 1 else acc) 
	population.genotypes 0
	
let popIter  ?(pred=(fun _ _-> true)) ?(f=(fun _ -> ( ))) population  = 
	Hashtbl.iter
	(fun genotype status -> if pred genotype status then f genotype)
	population.genotypes

let popFold population foldf base = 
	Hashtbl.fold foldf population.genotypes base

let popGenotypesStatus population func = 
	popFold population 
		(fun genotype status lst -> 
			match func genotype status with 
			|true -> genotype::lst
			|_-> lst) []
		
let popRemove ?(pred=(fun _ _-> true)) population =
	popIter ~pred  population 
	~f:(fun gtype -> Hashtbl.remove (population.genotypes) gtype)
	


(**********************************************)
(*Predicates **********************************)
(**********************************************)				
let popExists population genotype = Hashtbl.mem population.genotypes genotype

let popIsAlive population gtype = 
	match Hashtbl.mem population.genotypes gtype with
	|false -> None
	|true -> 
	let status = Hashtbl.find population.genotypes gtype in
	match status with
	|Alive info -> Some info
	|_->None
 
 let popAvgAliveFitness population = 
	let num = ref 0.0 in
	let fit = ref 0.0 in
	popIter population
	~f:(fun gtype -> 
		match popIsAlive population gtype with 
		|Some (ev , _)  -> 
				num := !num +. 1.0;
				fit := !fit +. ev
		|_->( ));
	!fit /. !num
	
 let popKill population gtype = 
	match popIsAlive population gtype with 
	|Some (ev , genCount) -> 
		Hashtbl.replace population.genotypes gtype (Dead (ev , genCount))
	|_->( )

let popIdInfo pop = (pop.info.speciesId,pop.info.ancesterID)
let popInsertGenotype popn gype gtus = Hashtbl.add popn.genotypes gype gtus
let popSetInfo population info = population.info <- info
let popSetFitness pop fit = pop.popFitness <- fit
let popFitness pop =  pop.popFitness

	

	 



	

 
		
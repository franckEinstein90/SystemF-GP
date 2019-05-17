open Utils.StringSpecial
open Utils.Pervasive
open Utils.ListSpecial
open Utils.RandSelect
open Utils.Error
open Syntax
open Core
open TestCases
open BlockPopulation
open PrintExpr
open ProofAssembler
open BlockPopulation.Gene
open Rules
open Population



type ecoSetting = {
	maxProofWidth : int;
	aprNumberOfLiveGenotypes : int;
	aprGenePoolTotComp : int;
	selectSize : int;
	selectReap : int
	}
type ecosystem  = { 
	maxGenotypeScore : float;
	mutable solutions : term list;
	mutable speciesCtr : int;
	mutable generationCounter : int;
	mutable stats : string;
	settings : ecoSetting;
	goalType : ty; 
	mutable environment : test_case list;
	genes : gene_pool ref; 
	kingdom : (species,  population) Hashtbl.t  
	} 

let make_eco ?(debug=false) genes_ref goal test_cases settings  = 
	{maxGenotypeScore = float (List.length test_cases);
	solutions = [ ];
	stats = "";
	speciesCtr = 0;
	generationCounter = 0;
	settings = settings;
	goalType = goal;
	environment = test_cases;
	genes = genes_ref;
	kingdom = Hashtbl.create 300}

let empty_eco genes = {
	maxGenotypeScore = 0.0;
	settings = {
		maxProofWidth = 10;
		aprNumberOfLiveGenotypes = 0;
		aprGenePoolTotComp = 0;
		selectSize = 0;
		selectReap = 0 
		};
	stats ="";
	solutions = [ ];
	speciesCtr = 0;
	generationCounter = 0 ;
	goalType = TyArr(TyUnit, TyUnit);
	environment = [empty_test_case];
	genes = genes;
	kingdom = Hashtbl.create 0;
	}
let ecoContext eco = gpContext !(eco.genes) 
let ecoHasSolution eco = (List.length eco.solutions) > 0
let ecoPopulation eco species = 
	let res = ref None in
	Hashtbl.iter
		(fun spec pop -> 
			match proofEquiv (ecoContext eco) spec species with
			|true -> res := Some pop
			|_-> ( )) eco.kingdom;
	match !res with 
	|Some pop -> pop
	|None -> failwith "Unable to get population for species"
	

let ecoGenes eco = !(eco.genes)
let ecoGenerationNumber eco = eco.generationCounter
let ecoGoalType eco = eco.goalType
let ecoStats eco = eco.stats
let push eco stat = eco.stats <-   eco.stats ^ ("\n" ^ stat) 

(****************************************************************************)
(* Utils ********************************************************************)
(****************************************************************************)
let mprf str = prf str; read_line( )
let ecoFold eco base foldFunc  = Hashtbl.fold foldFunc eco.kingdom base
let ecoCount ?(pred=(fun _ _->true)) eco = 
	ecoFold eco 0 (fun _ population acc -> acc + (popCount ~pred population))  
let ecoIter eco iterFunc = 
	Hashtbl.iter iterFunc eco.kingdom

(********************************************************)
(* Fitness analisys *************************************)
(********************************************************)
let ecoMaxPossibleScore eco = eco.maxGenotypeScore
let ecoAvgGtyeScore eco = 
	let sum = ref 0. in
	let count = ref 0. in
	ecoIter eco 
	(fun species population ->
		popIter population 
		~f: (fun gtype -> 
				match popIsAlive population gtype with
				|Some (ev,_) -> sum := !sum +. ev; count := !count +. 1. 
				|_->( )));
	!sum /. !count	

(****************************************************************************)	
(*Updates *******************************************************************)
(****************************************************************************)
let ecoUpdateSpeciesGeneUse eco  = 
	reset_values (ecoGenes eco);
	let rec update species = 
		match species  with 
		| ProgNode ty  -> 
			let catInfo, _ = gpAllelesofGenes (ecoGenes eco) ty in
			catInfo.species_use <- catInfo.species_use + 1
		| ForAllElim (proof, ty1)  -> update proof
		| ImplyElim (proof1, proof2) -> update proof1; update proof2
		| _->failwith "Invalid pair proof/program" in
	ecoIter eco (fun a b -> update a) 

(****************************************************************************)
(*Genotypes *****************************************************************)
(****************************************************************************)
let ecoKill eco species gtype = 
	popKill (ecoPopulation eco species) gtype


let rec ecoIsPossibleGenotype eco species gtype = 
	match species , gtype with 
	|ProgNode ty , In id when  gpAlleleIdExists (ecoGenes eco) ty id -> true 
	|(ForAllElim(species,ty1)) , (TypeFunc(gtype,ty2)) ->
		ecoIsPossibleGenotype eco species gtype
	| (ImplyElim (speciesLeft,speciesRight)), (ObjFunc(gtypeLeft,gtypeRight)) ->
		(ecoIsPossibleGenotype eco speciesLeft gtypeLeft) && (ecoIsPossibleGenotype eco speciesRight gtypeRight)		
	|_ -> false
	
let ecoRemoveImpossibleGenotypes eco = 
	prf "\nRemoving impossible genotypes";
	Hashtbl.iter 
		(fun species population -> 
			let pred gtype _ = not (ecoIsPossibleGenotype eco species gtype) in
			popRemove ~pred population)
		(eco.kingdom);
	prf "\nDone removing impossible genotypes"
	
let ecoMaxPossibleNewGenotypes eco species	 = 
	(gpePotentialNum (ecoGenes eco) species) -
	(popCount (ecoPopulation eco species)) 
			
(****************************************************************************)
(*Gene Specific *************************************************************)
(****************************************************************************)
let ecoFillGenes eco constraintFunc  =
		let compGenes = comp (ecoGenes eco) in
		match eco.settings.aprGenePoolTotComp - compGenes with
		| x when x > 0 ->
				growToComplexity 
				~growUnit:(Complexity x)
				(ecoGenes eco) constraintFunc
				~onSuccess:(fun bpset _ -> prf "\nsuccess")
				~onFailure:(fun str -> prf "\nfailure")
		|_ -> prf "\n Done building gene pool"	
				
(****************************************************************************)
(* Species ******************************************************************)
(****************************************************************************)
let ecoSpecies eco = keys eco.kingdom
let ecoSpeciesExists eco species = 
	ProofAssembler.exists (ecoContext eco) (ecoSpecies eco) species 
let ecoIsValidEcoSpecies eco proof = 
	ty_eqv (ecoContext eco) (check_tree (ecoContext eco) proof) eco.goalType
let ecoIsValidCompSpecies eco proof =
	match ecoIsValidEcoSpecies eco proof with
	|false -> false
	|true -> is_complete proof
let ecoSpeciesId eco species = 
	let idOpt = ecoFold eco None
		(fun spec pop acc -> 
			if (proofEquiv (ecoContext eco) spec species) 
			then Some (popIdInfo pop)
			else acc)
	in match idOpt with 
	|None -> raise Not_found
	|Some id -> id	
let ecoFreshSpeciesId eco = 
	let id = eco.speciesCtr in
	eco.speciesCtr <- eco.speciesCtr + 1;
	id		
let ecoSpeciesFromId eco id = 
	let speciesOpt = ecoFold eco None
	(fun spec pop acc -> 
			match popIdInfo pop with 
			|sid,_ when sid = id -> Some spec 
			|_-> acc)
	in match speciesOpt with 
		| None -> raise Not_found
		| Some species -> species
				
let rec ecoSpeciesUpdate eco species  =
	let population = ecoPopulation eco species in
	let (id,aid) = ecoSpeciesId eco species in 
	let new_info = {
				speciesId = id;
				ancesterID = aid} in 
			popSetInfo population new_info
						
let ecoAddSpecies eco proof ancestorID = 
	match ecoIsValidEcoSpecies eco proof with
	|true -> 
		Hashtbl.add eco.kingdom proof (popNew (ecoFreshSpeciesId eco) ancestorID);
		ecoSpeciesUpdate eco proof;
		ecoSpeciesId eco proof
	|false -> failwith "Unable to add species"	
					
(***********************************************************)
(*Evaluation************************************************)
(***********************************************************)
let ecoSpeciesEvaluateOne eco species = 
	prf ("\nEvaluating Species " ^ (string_of_int (ecoPopulation eco species).info.speciesId));
	let avgFitness = popAvgAliveFitness	(ecoPopulation eco species) in
	popSetFitness (ecoPopulation eco species) avgFitness  
	
let ecoSpeciesEvaluate eco = 
	prf "\nEvaluating All Species";
	ecoIter eco (fun species _ -> ecoSpeciesEvaluateOne eco species)
		

let ecoGenesEvaluate eco =
	prf "\nEvaluating Genotypes";
	let genes = !(eco.genes) in
	reset_values genes;
		
	let rec updateGenes species  = 
		match species with
		| ProgNode ty -> 
			let catInfo, _ = gpAllelesofGenes genes ty in
			catInfo.species_use <- catInfo.species_use + 1
		| ForAllElim (proof, ty1) -> updateGenes proof
		| (ImplyElim (proof1, proof2)) -> updateGenes proof1; updateGenes proof2 
		| _->failwith "Invalid proof" in
		
	let rec updateAlleles species prog ev = 
		match (species, prog) with 
		| ProgNode ty, In id -> 
			let info,lst  = gpAllelesofGenes genes ty in
			info.totalFitness <- info.totalFitness +. ev;
			let alle = List.find (fun g -> alleId g = id) lst in
				alleIncUseInPop alle;
				alleSetFitness alle ((alleFitness alle) +. ev);
				if (alleScoreOfBestCarrier alle) < ev then alleSetScoreBestCarr alle ev
		| ForAllElim (proof, ty1), TypeFunc (prog, ty2) -> 
			updateAlleles proof prog ev
		| (ImplyElim (proof1, proof2)) , (ObjFunc (prog1, prog2)) ->
			updateAlleles proof1 prog1 ev; updateAlleles proof2 prog2 ev
		| _->failwith "Invalid pair proof/program" in
	
	ecoIter eco 
	(fun species population ->
		updateGenes species; 
		popIter	population
		~f:(fun gtype ->
			match popIsAlive population gtype with 
			|Some (ev,_) -> updateAlleles species gtype ev;
			|_->( )));
	
	gpIter	 genes
	~f:(fun _ info alleLst -> 
			info.totalFitness <- 
				info.totalFitness /. 
				(List.fold_left (fun acc alle -> (float (alleUseInPopulation alle)) +. acc) 0. alleLst);
			List.iter alleNormalizeFitness alleLst) 
				   

	 
(***********************************************************)
(*Negative Selection****************************************)
(***********************************************************)		
let ecoRemoveIncompleteSpecies eco = 	
	ListLabels.iter (ecoSpecies eco)
	~f: (fun species -> 
		if not (ecoIsValidCompSpecies eco species) 
		then Hashtbl.remove eco.kingdom species)


(***********************************************************)
(* This function kills n genotypes randomly across the *****)
(* population. It is a catastrophy event, the sort that ****)
(* would occur when a volcano erupts or a meteorite ********)
(* crashes down. It doesn't distinguish between	fitness ****)
(* levels **************************************************)


(***********************************************************)
(* This function kills all genotypes with fitness below a **)
(* certain threshold. **************************************)
(***********************************************************)
let ecoDeterministicGenotypeFilter eco threshold =
	ecoIter eco
	(fun _ pop -> 
		popIter pop ~pred:(popIsAlive pop gtype) 
	~f:(fun gtype _ -> 
			match status with  
			| x when x <= threshold -> Hashtbl.replace pop.genotypes prog (Dead x)
			|_ ->( ))) 
	
let ecoDeterministicSpeciesFilter eco threshold = 
	ecoIter eco 
	(fun species population -> 
		match popFitness population with 
		| x when x <= threshold ->   Hashtbl.remove eco.kingdom species
		| _-> ( ))

		
let rec ecoReapGenotypes eco = 
	let pred = fun _ -> function Alive _ -> true | _->false	in
	let get_live pop = 
		let foldf genotype status lst =
			match status with
			|Alive ev -> (ev , genotype) :: lst
			|_->lst	in
		popFold pop foldf [] in
			
	let assignSurplus totSurplus species =
		  (species ,
			int_of_float ((float (totSurplus) *. 
			float (popCount ~pred (ecoPopulation eco species)) /.
			float (ecoCount ~pred eco)))) in
	
	let avgGenotypeFitness = ecoAvgGtyeScore eco in 
	let gtypeProb ev = ev /. avgGenotypeFitness in  	 		  
	let elim (species, numToKill) = 
		let pop = ecoPopulation eco species in
		match popCount ~pred pop with
		| x when x < numToKill ->  Hashtbl.remove eco.kingdom species
		| x -> 
			let killedGenotypes = ref [] in
			let lives =	 get_live pop in 
			List.iter 
			(fun (ev,gtype) ->
				match ev with 
				| x when x < avgGenotypeFitness ->  
					killedGenotypes := [gtype] @ !killedGenotypes;
					ecoKill eco species gtype
				| _->( )) lives;
			let selFunc (ev,_) = if ev > 0. then avgGenotypeFitness /. ev else 10.  in
			while (List.length !killedGenotypes) <  numToKill do
				let _,selDead = List.hd(rouletteWheelSelect (get_live pop)  selFunc) in
				killedGenotypes := [selDead] @ !killedGenotypes;
				ecoKill eco species selDead;
			done;
			ecoSpeciesEvaluateOne eco species in
				
	let totSurplus = (ecoCount ~pred eco) - (eco.settings.aprNumberOfLiveGenotypes - eco.settings.selectReap) in
	(match totSurplus with 
	|x when x > 0 ->
		let progrom = List.map (assignSurplus totSurplus) (ecoSpecies eco) in
		List.iter elim progrom
	|_->( ))
	
	
		
								

		

let ecoUpdateAll eco =
	ecoUpdateSpeciesGeneUse eco;
	ecoIter eco (fun species _ -> ecoSpeciesUpdate eco species)
	
let candidateTypes genes gt = 
	get_types genes ~pred:(fun ty ->type_to (gpContext genes) gt ty)

let implyTypes goalType genes = 
	ListLabels.map (candidateTypes genes goalType)
	~f:(function 
		|ty when ty_eqv (gpContext genes) ty goalType -> ProgNode ty
		|ty -> Partial (goalType, Some (ProgNode ty))
		(*ImplyElim (ProgNode ty, Partial (ty1, None))*)
		|_->  assert (1=0);failwith "Not supported")
						
						
let rec mp proof eco depth goalType = 
	assert (depth >=1);
	let genes = !(eco.genes) in
	let ctx = gpContext genes in
	let proofType = check_tree ctx proof in
	match ty_eqv ctx proofType goalType with
	|true->
		(match depth with 
		|_ when depth = 1 -> 
			(match proof with 
			| Partial (tyGoal, Some proof) -> 
				ListLabels.map (mp proof eco 1 tyGoal)
				~f:(fun proof -> 
					if ty_eqv ctx tyGoal (check_tree ctx proof) 	
					then proof 
					else Partial(tyGoal, Some proof))
			| Partial (ty, None) -> implyTypes ty genes 
			| _->[proof])	
		|_->
			match proof with
			| ProgNode ty -> [ProgNode ty] 
			| Partial (tyGoal, _) -> mp proof eco 1 tyGoal
			| ImplyElim(proof1, proof2)->
				cartesianProduct 
				(fun proof1 proof2 -> ImplyElim(proof1,proof2))
				(mp proof1 eco (depth - 1) (check_tree ctx proof1)) 
				(mp proof2 eco (depth - 1) (check_tree ctx proof2)) 
			|_->  [ ])
	|false ->
		match proofType with 
		|TyArr(ty1, ty2) -> [ImplyElim (proof, (Partial (ty1,None)))]
		| _ -> assert (1=0);failwith "???"		

let modifyProofs ?(proofs=[]) eco depth = 
	assert (depth >= 0);
	let genes = !(eco.genes) in
	match depth, proofs  with
		|0, _  -> List.map (fun proof -> (None,proof)) (implyTypes eco.goalType genes)
		|_ -> 
		let proofs = match proofs with
			|[ ] -> ecoSpecies eco 
			|_-> proofs in
		List.flatten (
			ListLabels.map proofs
			~f:(fun proof -> 
				let aid = fst (ecoSpeciesId eco proof) in
				List.map (fun p -> (Some aid, p)) (mp proof eco depth eco.goalType)))
																	
let speciesAccept ?(max=20) eco new_proofs =
	let genes  = !(eco.genes) in 
	let addProofs = 
		List.iter
		(fun (ancesterID, proof) -> 
			match ecoIsValidEcoSpecies eco proof, ecoSpeciesExists eco proof with
			|true, false -> un(ecoAddSpecies eco proof ancesterID)
			|_ -> ( )) in

	let (new_complete_proofs, new_incomplete_proofs) = 
		ListLabels.partition new_proofs 
		~f:(fun (_,pr) -> ecoIsValidCompSpecies eco pr) in
	
	prf ("\nAdding "^string_of_int (List.length new_complete_proofs) ^ " complete proofs");
	addProofs new_complete_proofs;
	
	let lst =  
		if List.length new_incomplete_proofs < max 
		then new_incomplete_proofs
		else choose_random ~selectionSize:max new_incomplete_proofs in
		
	addProofs lst		 		

let makeProofs goOn eco  = 
	let depth = ref 0 in
	while goOn eco !depth do
		speciesAccept eco (modifyProofs eco !depth);
		depth := !depth + 1
	done	
	
						
let evaluate eco species prog foundSolution =  
	let tests = eco.environment in
	let genes = !(eco.genes) in
	let ctx = gpContext genes in
	let te = term_of_program genes species prog in 
	let score = ListLabels.fold_left ~init:0.0 tests
	~f:(fun acc test -> if match_case te ctx test then acc +. 1.0 else acc) in
	match score with 
	|x when x = eco.maxGenotypeScore ->
		foundSolution := true;
		prf ("\nFound solution :" ^ string_of_term ctx te Compact ^ "\n");
		prf (string_of_program genes (species, prog));
		(Hashtbl.find eco.kingdom  species).contains_solution <- true;   
		if not (List.exists (fun lstTe ->  te_eqv ctx lstTe te) eco.solutions) then  
		eco.solutions <- te :: eco.solutions;
		(Some te, score)
	|_ -> (None, score)


			
(************************************************************************************)
(*Next Generation Construction ******************************************************)
(************************************************************************************)	
let ecoInsertAndEvaluate eco species_gtypeList genotypeEvaluationNum = 
		let foundSolution = ref false in
		let sol = ref None in
		List.iter 
		(fun (species , genotype) ->
			let (solOption, eval) = evaluate eco species genotype foundSolution	 in
				(match solOption with
				|None ->( )
				|Some te -> 
					foundSolution := true;
					sol := Some te);
				let genoStatus = Alive eval in
				genotypeEvaluationNum := !genotypeEvaluationNum + 1;
				prf ("\nEvaluated genotype number " ^ (string_of_int !genotypeEvaluationNum));
				prf (" inserting into species " ^ (string_of_int (fst (ecoSpeciesId eco species))));
				popInsertGenotype (ecoPopulation eco species) genotype genoStatus)
		species_gtypeList;
		!sol	
		
			 

(********************************************************)
(* ecoNewGenotypes eco species selectionSize evGtypeLst *)
(*   - doesn't insert the new genotypes, just creates   *)
(*      and returns a list of new genotypes             *)
(*   - evGtypeLst has type (float * genotype) list ******)
(*      and each genotype it contains is alive in the ***)
(*      population of species ***************************)
(********************************************************) 
let ecoNewGenotypes eco species selectionSize evGtypeLst = 
	let	 maxNew = ecoMaxPossibleNewGenotypes eco species in
	let removeExists species lst = 
	List.filter (fun g -> not (popExists (ecoPopulation eco species) g))	lst in
	
	let newGtypes = 
	(match evGtypeLst, (maxNew > selectionSize) with
	| _ , false -> 
		List.map (fun gtype -> (species , gtype))
		(removeExists species (gpGenotypesOfSpecies (ecoGenes eco) species))
	
	|[ ] , _ -> 
		let unbornGenotypes = gpGenotypesOfSpecies (ecoGenes eco) species in	
		List.map (fun gtype -> (species , gtype))
		(choose_random ~selectionSize unbornGenotypes)
	
	|_ ->  
		let selectFunc (ev,gen) = (ev *. ev) in
		let parentGenotype =  List.hd(rouletteWheelSelect evGtypeLst selectFunc) in
		prf ("\nSelected genotype " ^ (gpeGpeStr (ecoGenes eco) (snd parentGenotype)));
		prf (" from species " ^ (string_of_int  (ecoPopulation eco species).info.speciesId) ^ " as parent");
		prf (" - Fitness : (" ^ (string_of_float (fst parentGenotype)) ^ ")");
		let wgtypes = gtypeGenotypesWithinDistance (ecoGenes eco) species (snd parentGenotype) 1 in
		let resTypes = List.map (fun gtype -> (species , gtype)) (removeExists species wgtypes)	in
		match List.length resTypes with 
		| x when x > selectionSize -> choose_random ~selectionSize resTypes
		|_ -> let res = ref resTypes in !res) in
	
	prf ("\nCreated " ^ (string_of_int (List.length newGtypes)) ^ " new genotypes");
	newGtypes
		
let ecoRebuildPopulation eco genotypeEvaluationNum = 
	let sum = ref (0.,0.) in
	let bestSpeciesScore = ref 0. in
	Hashtbl.iter 
		(fun species population ->
			match popFitness population with 
			|0. -> ( )
			|x -> 
				if x > !bestSpeciesScore then bestSpeciesScore := x;
				sum := (((fst !sum) +. x), ((snd !sum) +. 1. )))
		eco.kingdom;
	let avgFitnessSpecies = (fst !sum) /. (snd !sum) in
	Hashtbl.iter 
		(fun species population ->
			match popFitness population with 
			|0. -> popSetFitness population avgFitnessSpecies
			|_ -> ( ))
		eco.kingdom;
	
	let predLive = (fun _ -> function Alive _->true | _->false)	in   
	let predDead = (fun _ -> function Dead _->true | _->false)  in 
	while (ecoCount ~pred:predLive eco) < eco.settings.aprNumberOfLiveGenotypes do  
		let species = List.hd (rouletteWheelSelect 
			(ecoSpecies eco)
			(fun species -> popFitness (ecoPopulation eco species))) in   
		
		let population = ecoPopulation eco species	in
		
		let maxPossible = 
			(gpePotentialNum (ecoGenes eco) species) -
			(popCount population
			~pred:(fun a b -> (predLive a b) || (predDead a b))) in
		
		let  evGtypeLst = 
			popFold population 
			(fun gtype status lst -> 
				match status with 
				|Alive ev -> lst @ [(ev , gtype)]
				|_-> lst) [] in
	
		let selectionSize = int_of_float (15. *. ((popFitness (ecoPopulation eco species))	/.	!bestSpeciesScore))	in
		let newSpeciesGenotypes = ecoNewGenotypes eco species selectionSize evGtypeLst in
		ecoInsertAndEvaluate eco newSpeciesGenotypes genotypeEvaluationNum   
	done;	
	None

let ecoNextGeneration eco constraintFunc genotypeEvaluationNum = 
	prf "\n-----------------------------------------";
	prf "\n-----------------------------------------";
	prf ("\nConstructing generation: " ^ (string_of_int eco.generationCounter));
	let non_determinisic_selection_threshold = 0. in
	ecoDeterministicGenotypeFilter eco non_determinisic_selection_threshold;
	ecoReapGenotypes eco;
	ecoSpeciesEvaluate eco;
	ecoDeterministicSpeciesFilter eco non_determinisic_selection_threshold;
	ecoGenesEvaluate eco;
	gpRemoveUnusedAlleles (ecoGenes eco);
	ecoRemoveImpossibleGenotypes eco;	
	
	(************************************)
	(* Assign selection probabilities ***)							    
	(* to the genes *********************)
	(************************************)
	gpAssignSelectionProbs (ecoGenes eco);
	let ctr = ref 6 in
	let cond1( ) = !ctr > 0	 in
	while cond1()  do	  
			ecoFillGenes eco constraintFunc;			
			let goOn = (fun eco depth -> if depth >= 4 then false else true) in
			makeProofs goOn eco;
			ecoRemoveIncompleteSpecies eco;
			ecoUpdateAll eco;
			gpRemoveUnusableGenes (ecoGenes eco);
			ctr := !ctr - 1
		done;  	
	let sol = ecoRebuildPopulation eco genotypeEvaluationNum in
	ecoSpeciesEvaluate eco;
	ecoGenesEvaluate eco;  
	eco.generationCounter <- eco.generationCounter + 1;
	sol	
				

(************************************************************************************)
(*Initial Generation Creation *******************************************************)
(************************************************************************************)
let ecoRandomSpawnNewGenotypes eco genotypeEvaluationNum = 
	let pred = fun _ -> function Alive _->true | _->false  in
	assert(ecoCount ~pred eco= 0);
	let numPotential = ProofAssembler.prog_num (ecoGenes eco) (ecoSpecies eco) in
	assert (numPotential >= eco.settings.aprNumberOfLiveGenotypes);
	
	let numToCreate species  = 
		int_of_float 
		((float (eco.settings.aprNumberOfLiveGenotypes)) 
		*. (float (ecoMaxPossibleNewGenotypes eco species)) 
		/. (float numPotential)) in
	
	let foundSolution = ref false in
	let sol = ref None in
	List.iter 
		(fun species  ->
			let newGenotypes = ecoNewGenotypes eco species (numToCreate species) [] in
			sol :=  ecoInsertAndEvaluate eco newGenotypes genotypeEvaluationNum)
	(ecoSpecies eco);
	!sol
			
let rec ecoFirstGeneration eco constraintFunc genotypeEvaluationNum = 
	push eco ("----\nGeneration Construction:" ^ (string_of_int eco.generationCounter));
	let prog_num ( ) = ProofAssembler.prog_num (ecoGenes eco) (ecoSpecies eco) in
	let ctr = ref 20 in
	let cond1( ) = !ctr > 0	 in
	let cond2( ) =	(prog_num ( )) <=  eco.settings.aprNumberOfLiveGenotypes in
	let cond3( ) = (Hashtbl.length eco.kingdom) < 15 in
		while ((cond1()) && (cond2())) && (cond3())  do	  
			ecoFillGenes eco constraintFunc;			
			let goOn = (fun eco depth -> if depth >= 6 then false else true) in
			makeProofs goOn eco;
			ecoRemoveIncompleteSpecies eco;
			ecoUpdateAll eco;
			gpRemoveUnusableGenes (ecoGenes eco);
			ctr := !ctr - 1
		done;  
		gpRemoveUnusableGenes (ecoGenes eco);
		push eco ("\t"^ (string_of_int (List.length (keys eco.kingdom))) ^ " species");
		push eco ("\t"^ string_of_int (ecoCount eco) ^ " possible genotypes");
		prf "\nBeginning evaluation of first generation";
		let sol = ecoRandomSpawnNewGenotypes eco genotypeEvaluationNum in
		ecoSpeciesEvaluate eco;
		ecoGenesEvaluate eco;
		gpAssignSelectionProbs (ecoGenes eco);
		eco.generationCounter <- eco.generationCounter + 1;
		sol
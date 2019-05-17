open Format
open Utils.Error
open Utils.Pervasive
open Utils.RandSelect
open Utils.StringSpecial
open Syntax
open Pointers
open Binding
open Core
open PrintExpr
open GPUtils


exception BlockConstructionFailure

module Gene = struct	
	type alleCategory = 
		| Kernel 
		| NoSpec	
		| GoalTypeConstructor 
		| LeftCombinator
		| RightCombinator
	
	type allele = {
		seed : (ty * int) option; 
		expression : term;
		alleId : int;
		alleProtectFlag : alleCategory; 
		
		mutable alleFitness : float;	
		mutable alleSelectionProb : float;
		mutable scoreOfBestCarrier : float;
		mutable useInPopulation : int;
		mutable seedSelectionCount : int
	}
	let alleProtected alle = 
		match alle.alleProtectFlag with 
		|NoSpec -> false
		|_-> true
		
	let alleExpr g = g.expression
	let alleNew te id seed protectFlag = {
		seed = seed;
		expression = te;
		alleId = id;		
		alleProtectFlag = protectFlag;
		alleSelectionProb = 0.0;
		alleFitness = 0.0;
		seedSelectionCount = 0;
		scoreOfBestCarrier = 0.0;
		useInPopulation = 0
	}
	(**********************************************************)
	(* Utils **************************************************)
	(**********************************************************)
	let alleListFold ?(f =(fun a _ ->a)) base alleLst = List.fold_left f base alleLst
			
	let alleComplexity alle = complexity_of_expr (Te (alleExpr alle,None))
	let alleId a = a.alleId
	let alleHasId  id alle = alle.alleId = id
	
	(*Selection probability handlers***************************)
	let alleSelectionProb a = a.alleSelectionProb
	let alleSetSelectionProb alle value = alle.alleSelectionProb <- value
	
	
	let alleScoreOfBestCarrier alle = alle.scoreOfBestCarrier	
	let alleSetScoreBestCarr alle score = alle.scoreOfBestCarrier <- score
	
	let alleUseInPopulation alle = alle.useInPopulation
	let alleIsCarried alle = alle.useInPopulation > 0
	let alleIncUseInPop alle = alle.useInPopulation <- alle.useInPopulation + 1
	
	let alleSeedSelectionCount alle = alle.seedSelectionCount
	
	
	(*Fitness value handlers***********************************)
	let alleFitness alle = alle.alleFitness
	let alleSetFitness alle fit = alle.alleFitness <- fit
	
	let alleNormalizeFitness alle =
		alleSetFitness alle 
		(match alleUseInPopulation alle with
		| x when x > 0 ->  	(alleFitness alle) /. (float x)
		| _ -> - 1.0)
end 
open Gene

type categoryInfo = {
	mutable species_use : int;
	mutable totalFitness : float;
	mutable next_id : int
	}
type gene_pool = {
	g : (ty, (categoryInfo * allele list)) Hashtbl.t;
	mutable ctx : context ref
	}

let gpIter ?(f=(fun a b c -> ( ))) genes = 
	Hashtbl.iter (fun a (b,c) -> f a b c) genes.g

let gpFold ?(f=(fun _ _ a -> a)) genes base = 
	Hashtbl.fold f genes.g base  


let gpContext genes = !(genes.ctx) 


	

let get_alleles genes = 
	Hashtbl.fold (fun _ (_, lst) accLst -> lst @ accLst)
	genes.g [ ]	

let reset_values genes = 
	Hashtbl.iter (fun _ (cinfo, lst) -> 
		cinfo.species_use <- 0;
		cinfo.totalFitness <- 0.0;
	ListLabels.iter lst
	~f:(fun g -> 
			g.alleFitness <- 0.0;
			g.alleSelectionProb <- 0.0;
			g.useInPopulation <- 0;
			g.scoreOfBestCarrier <- 0.0;
			g.seedSelectionCount <- 0)) genes.g

let gpCount ?(pred = (fun _ -> true)) genes = 
	Hashtbl.fold 
	(fun ty (_, q) acc -> acc + List.length q) 
	genes.g 0			

let gpAlleleAvgFitness genes = 
	(gpFold genes 0.
	~f:(fun _ (_ , lst) acc -> 
		acc +. 
		(alleListFold  0. lst
		~f:(fun acc alle -> acc +. (alleFitness alle))))) /.
	 (float (gpCount genes))


(*******************************************)
(*Assignation of alleles selection probs ***)
(*******************************************)
 let gpAssignSelectionProbs genes = 
	prf "\nAssigning allele probabilities";
	let numAlleles = ref 0 in
	let totalUse = ref 0 in
	let sumFit = ref 0. in
	gpIter genes
	~f:(fun ty info alleLst ->
			List.iter 
			(fun alle ->  
				numAlleles := !numAlleles + 1;
				totalUse := !totalUse + (alleUseInPopulation alle);
				sumFit := !sumFit +. (alleFitness alle)) alleLst);
	
	let avgFitness = !sumFit /. (float !totalUse) in
	gpIter genes
	~f:(fun ty info alleLst ->
		List.iter 
			(fun alle ->
				let selProb =   (float (alleUseInPopulation alle)) *. (alleFitness alle) in
				alleSetSelectionProb alle selProb) alleLst)
				


	
let comp genes = 
	Hashtbl.fold 
	(fun _ (_ , lst) acc -> 
		ListLabels.fold_left ~init:acc lst
		~f:(fun acc g -> (alleComplexity g) + acc))
	genes.g 0  

let gpCompareGenes genes ty1 ty2 = 
	ty_eqv !(genes.ctx) 
		(simplify_ty !(genes.ctx) ty1)
		(simplify_ty !(genes.ctx) ty2)

let geneExists genes ty = 
	let res = ref false in
	gpIter	genes
	~f:(fun gene _ _ 	 ->	
			match gpCompareGenes genes gene ty with
			|true -> res:=true
			|_->( )) ;
	!res
	
let gpAllelesofGenes genes ty =
	assert(geneExists genes ty);
	let returnVal = ref None in
	gpIter genes
	~f:(fun gene info alleles -> 
		match gpCompareGenes genes ty gene with
		|true -> returnVal := Some (info, alleles)
		|false -> ( )) ;
	match !returnVal with 
	|None -> failwith ("The gene "^( string_of_type (gpContext genes) ty Alpha)^ " is not in the gene pool")
	|Some alles -> alles

let alleleExists genes te = 
	let ty = simplify_ty (gpContext genes) (typeof !(genes.ctx) te) in
	match geneExists genes ty with
	|true ->  
		List.exists 
			(fun alle -> te_eqv !(genes.ctx) (alleExpr alle) te) 
			(snd (gpAllelesofGenes genes ty))
	|_ -> false

let teFind genes te = 
	ListLabels.find (snd (gpAllelesofGenes genes (typeof !(genes.ctx) te)))
	~f:(fun t -> te_eqv !(genes.ctx) (alleExpr t) te)
				
let gpRemoveGene genes ty = 
	let hasProtectedAllele lst = List.exists alleProtected lst in		
	let sTy = simplify_ty !(genes.ctx) ty in
	match geneExists genes sTy  with 
	|false ->( )
	|true -> 
		let _,lst = gpAllelesofGenes genes sTy in
		match hasProtectedAllele lst with
		|true -> ( )
		|false -> Hashtbl.remove genes.g sTy

let gpRemoveUnusableGenes genes  =
	gpIter genes
	~f:(fun gene info _ ->
		match info.species_use with
		| 0 -> gpRemoveGene genes gene
		|_->( )) 

(* Remove all alleles for which  ***************)
(* useInPopulation <= 0          ***************)
let  gpRemoveUnusedAlleles genes = 	 
	 let count = ref 0 in
	 gpIter genes
	 ~f:(fun ty ci alleLst -> 
		let usedAlleles = 
			List.filter 
			(fun alle -> (alleIsCarried alle) ||(alleProtected alle)) 
			alleLst  in
		count := !count + ((List.length alleLst) - (List.length usedAlleles));
		match usedAlleles with
		| [] -> gpRemoveGene genes ty
		| lst -> Hashtbl.replace genes.g ty (ci,lst)) 
		;
		prf ("\nRemoved " ^ (string_of_int !count) ^ " alleles")
							
let replace_type genes ty catInfo geneLst  = 
	let sTy = simplify_ty !(genes.ctx) ty in
	if geneExists genes sTy  then begin 
		let cInfo, lst = gpAllelesofGenes genes sTy in
		let catInfo  = match catInfo with 
			| Some cInfo -> cInfo 
			| None -> cInfo in
		let geneLst = match geneLst with 
			| Some newlst -> newlst
			| None -> lst in
		assert (catInfo.next_id = List.length geneLst);
		Hashtbl.replace genes.g sTy (catInfo,geneLst)
	end
	else begin	
		let geneLst = match geneLst with 
			| Some newlst -> newlst
			| None -> [ ] in	
		let catInfo  = match catInfo with 
			| Some cInfo -> cInfo 
			| None -> {species_use=0; totalFitness = 0.0; next_id = List.length geneLst } in
		assert (catInfo.next_id = List.length geneLst);
		Hashtbl.add genes.g sTy (catInfo,geneLst)
	end
		
let rec make ctx_ref goal_type leftCombinators rightCombinators = 
	let genes = {
		ctx = ref !ctx_ref;
		g = Hashtbl.create 200} in
	let len = context_length !ctx_ref in
      for i=0 to len-1 do 
		match expr_of_name !ctx_ref (name_of_index !ctx_ref i) with
		|Te (te,_) -> insert_expression genes te None Kernel
		|Ty (ty,_) -> ( )
	   done;
	List.iter 
		(fun te -> insert_expression genes te None GoalTypeConstructor) 
		(make_constructors !ctx_ref goal_type);
	
	List.iter 
		(fun te -> insert_expression genes te None RightCombinator) 
		rightCombinators;	   
	
	let tmttp left ty = normalize ! ctx_ref (TmTApp (left,ty)) in
	List.iter 
		(fun left -> 
			List.iter
			(fun te -> insert_expression genes te None LeftCombinator)
			(List.map (tmttp left) ([TyInt ; goal_type] @(get_atomic_types !ctx_ref))))
			leftCombinators;	
	
	let prob = 1.0 /. float (gpCount genes) in
	List.iter 
		(fun alle -> alle.alleSelectionProb <- prob)
		(get_alleles genes);	
	genes
     
(*insertion operations*)
and insert_expression genes te seed protectFlag = 
	let ctx = gpContext genes  in
	let gene = simplify_ty ctx (typeof ctx te) in 
	match (alleleExists genes te), (geneExists genes gene) with
	|true,_ ->  ( )
	|false, false -> 
		Hashtbl.add genes.g gene
		({species_use=0; totalFitness = 0.0; next_id = 1}, 
		[alleNew te 0 seed protectFlag])

	|_ ->
		replace_type genes gene None None;		
		let cinfo, geneLst = gpAllelesofGenes genes gene in
		let id = cinfo.next_id in 
		cinfo.next_id <- cinfo.next_id + 1;	
		let allele = alleNew te id seed protectFlag in
		replace_type genes gene (Some cinfo) (Some (allele::geneLst))
					 
						
let set ~(fitness:float) genes te = 
	alleSetFitness (teFind genes te) fitness



	
let seedSelect genes = 
		let alle = List.hd (rouletteWheelSelect (get_alleles genes) alleSelectionProb) in	
		alle.seedSelectionCount <- alle.seedSelectionCount + 1;
		alle

let gpAlleleIdExists genes ty id = 
	let _, lst = gpAllelesofGenes genes ty in
	List.exists (fun g-> g.alleId = id) lst
		
let get_term genes ty id = 
		let _, lst = gpAllelesofGenes genes ty in
		(alleExpr (List.find (fun g-> g.alleId = id) lst))

let get_types ?(pred =(fun _->true)) genes = 
	Hashtbl.fold 
	(fun ty _ acc -> if pred ty then ty::acc else acc) 
	genes.g []
	
let get_terms ?(predTe =(fun _->true)) ?(predTy =(fun _->true)) genes  = 
	Hashtbl.fold 
		(fun ty (_, lst) acc -> 
			(if not (predTy ty) then [ ] else 
			List.filter 
				(fun te -> predTe te)
				(List.map alleExpr lst)) @ acc) 
			genes.g [ ]
	
	
let rand_expr genes = 
	let expr_list = 
		(List.map (fun ty -> Ty(ty,None)) (get_types genes)) @
		(List.map (fun te -> Te(te,None)) (get_terms genes)) 
	in List.hd(choose_random expr_list)

let rand_ty genes =
		let type_list = get_types genes
		in List.hd(choose_random type_list)

let rand_te ?(ty_opt=None) genes  =
	List.hd (
	choose_random (match ty_opt with 
	|None -> get_terms genes
	|Some ty -> List.map alleExpr (snd (gpAllelesofGenes genes ty))))	

let rand_func_from genes ty = 
  let is_from tyArg = 
	match tyArg with
    |TyArr (ty1 , _) -> ty_eqv !(genes.ctx) ty1 ty 
    |_->false in 
    
  List.hd(
	choose_random  
	(List.map alleExpr
	(Hashtbl.fold (fun ty (_,lst) acc -> 
	 if (is_from ty) 
	 then lst @ acc
	 else acc) genes.g [ ])))   
	   
let rand_func_to genes ty = 
	let is_to tyArg = 
		type_to !(genes.ctx) ty tyArg in
	
	List.hd(
	choose_random  
	(List.map alleExpr
	(Hashtbl.fold (fun ty (_ , lst) acc -> 
	 if (is_to ty) 
	 then lst @ acc
	 else acc) genes.g [ ])))   	

		
	

	
	

open Utils.Pervasive
open Utils.Error
open Utils.RandSelect
open Utils.ListSpecial
open Utils.StringSpecial

open Syntax
open Core
open PrintExpr

open BlockPopulation.Gene
open BlockPopulation

(*********************************************************
Intuitionistic type theory, or constructive type theory, or Martin-Löf
type theory or just Type Theory is a logical system and a set theory
based on the principles of mathematical constructivism. Type Theory
was introduced by Per Martin-Löf, in 1972.


Intuitionistic type theory is based on a certain analogy or
isomorphism between propositions and types: a proposition is
identified with the type of its proofs. This identification is usually
called the Curry–Howard isomorphism, which was originally formulated
for propositional logic and simply typed lambda calculus. Type Theory
extends this identification to predicate logic by introducing
dependent types, that is types which contain values. Type Theory
internalizes the interpretation of intuitionistic logic proposed by
Brouwer, Heyting and Kolmogorov, the so called BHK interpretation. The
types of Type Theory play a similar role as sets in set theory but
functions definable in Type Theory are always computable.
*********************************************************)
type species = 
	| Partial of ty * species option
	| ProgNode of ty 
	| ForAllElim of species * ty
	| ImplyElim of species * species

let rec check_tree ctx species =
	let rec check = function
	| Partial (ty,_) -> ty
	| ProgNode ty -> ty
	| ForAllElim (species, ty) ->
		(match check species with
		|TyAll tyAll ->typeSubstTop ty tyAll
		| _-> assert (1=0); TyUnit)
	| ImplyElim (proof1, proof2) ->
		let ty_right = check proof2 in
			(match check proof1  with
			|TyArr (ty1, ty2) -> 
				if ty_eqv ctx ty1 ty_right then  ty2
				else (
					List.iter (fun ty -> prf ("\n"^(string_of_type ctx ty Compact))) [ty1; ty_right]; 
					showGraphs ctx [proof1; proof2]; assert(1=0);TyUnit)
			| _->  assert (1=0); TyUnit) in		
	check species
	
and dotGraph ?(nodeTag="n") ctx species = 
	let sty ty = string_of_type ctx ty Compact in
	let ctr = ref 0 in
	let newNode ( )= 
			let node = nodeTag ^ string_of_int !ctr in
			ctr := !ctr + 1;
			node in
	let rec make_dot_graph species parent graph =
		(match species with
		| Partial (ty, None) -> 
			let node = newNode( ) in
			let label = "Partial species\\n" ^ (sty ty) in
			graph ^ "\n"^node^" [label = \"" ^  label ^"\"];"^
			"\n"^parent^"->"^node^";" 
		| Partial (ty, Some species) -> 
			let node = newNode( ) in
			let label = "Partial species\\n" ^ (sty ty) in
			let g1 = graph ^ "\n"^node^" [label = \"" ^  label ^"\"];"^
			"\n"^parent^"->"^node^";\n" in 
			make_dot_graph species node g1
		| ProgNode ty -> 
			let node = newNode( ) in
			let label = "Gene\\n" ^ (sty ty) in
			graph ^ "\n"^node^" [label = \"" ^  label ^"\"];"^
			"\n"^parent^"->"^node^";"
		| ForAllElim (species, ty) -> assert (1=0); failwith "Unsupported"
		| ImplyElim (proof1, proof2) -> 
			let ty = check_tree ctx species in
			let label = "->Elim\\n" ^ sty ty ^ "\\n" in
			let node = newNode( ) in
			let g1 = graph ^ "\n"^node^" [label = \"" ^ label ^"\"];\n" ^
			parent ^ "->" ^ node in
			let g2 = make_dot_graph proof1 node g1 in
			make_dot_graph proof2 node g2) in
	let root = newNode( ) in
	make_dot_graph species root ("\n"^ root ^" [label=\"GoalType\" color=red];") 
and dotGraphs ctx proofs = 	
	"digraph G {\nval=\"0.4\";\ncenter=\"1\";" ^
	"\nrankdir=\"TB\";\nspline = \"true\";" ^
	let ctr = ref 0 in
	let newTag( ) = 
		let tag = "n" ^ (string_of_int !ctr) in
		ctr := !ctr + 1;
		tag in 
 	(String.concat "\n" (
 		List.map (fun species -> dotGraph ~nodeTag:(newTag( )) ctx species) proofs)
 		) ^"\n}"		
and showGraphs ctx proofs =  		 
	prf ~filename:(Some "tree267.dot") (dotGraphs ctx proofs);
	command "dot -Tgif tree267.dot -o out7876.gif | out7876.gif"
	
	
			
type genotype =  In of int | TypeFunc of genotype * ty | ObjFunc of genotype * genotype


	

let rec gtypeDistance ctx gtype1 gtype2 = 
	match gtype1 , gtype2 with 
	| (TypeFunc (gtype1,ty1)), (TypeFunc (gtype2,ty2)) when ty_eqv ctx ty1 ty2 ->
		(gtypeDistance ctx gtype1 gtype2) 
	| (ObjFunc (gtype1a , gtype2a)), (ObjFunc (gtype1b , gtype2b))  ->
		(gtypeDistance ctx gtype1a gtype1b) + (gtypeDistance ctx gtype2a gtype2b)		 
	| In id1 , In id2 when id1 = id2 -> 0
	| In id1 , In id2 -> 1
	| _ -> failwith "None matching genotypes" 
let rec gtypeEquiv ctx gtype1 gtype2  = 
	try (gtypeDistance ctx gtype1 gtype2) = 0
	with _ -> false


let rec gpeGpeStr ?(separator="\n") genes =
	function 
		| In id -> 
			string_of_int id
		| TypeFunc (prog, ty)-> 
			(gpeGpeStr ~separator genes prog) ^ "::" ^ (string_of_type (gpContext genes) ty Compact)
		| ObjFunc (prog1, prog2) -> 
			(gpeGpeStr ~separator genes prog1) ^  " ][ " ^ (gpeGpeStr ~separator genes prog2)


let rec string_of_program genes  =	
		let ctx = gpContext genes in function
			| ProgNode ty, In id -> 
				"/* Gene = " 
				^ (string_of_type ctx ty Alpha)
				^ "-- ID = " 
				^ (string_of_int id) 
				^ "*/" 
				^ string_of_term ctx (get_term genes ty id) Alpha
			| ForAllElim (species, ty1), TypeFunc (prog, ty2) -> 
					assert (ty_eqv ctx ty1 ty2); 
					"("
					^(string_of_program genes (species, prog))
					^ ") ["
					^ (string_of_type ctx ty1 Alpha)
					^ "]"
			| (ImplyElim (proof1, proof2)) , (ObjFunc (prog1, prog2)) ->
				"(" ^ (string_of_program genes (proof1, prog1)) ^ ")("
				^ (string_of_program genes (proof2, prog2)) ^ ")" 
			| _ -> failwith "InvalidProgProofPair"	
			
let rec proofEquiv ctx proof1 proof2 =
 match (proof1, proof2) with
 | Partial (ty1, None), Partial (ty2, None) -> ty_eqv ctx ty1 ty2
 | Partial (ty1, Some proof1), Partial (ty2, Some proof2) ->
	(ty_eqv ctx ty1 ty2) && (proofEquiv ctx proof1 proof2) 
 | ProgNode ty1,ProgNode ty2	-> ty_eqv ctx ty1 ty2
 | ForAllElim (proof1, ty1), ForAllElim (proof2, ty2) ->
     (proofEquiv ctx proof1 proof2) && (ty_eqv ctx ty1 ty2)
 | ImplyElim (proof1a, proof1b), ImplyElim (proof2a, proof2b)->
     (proofEquiv ctx proof1a proof2a) && (proofEquiv ctx proof1b proof2b)
 |_ -> false
 
let rec is_complete = function 
	| Partial _ -> false
	| ProgNode _ -> true
	| ForAllElim (proof1, ty1) -> is_complete proof1
	| ImplyElim (proof1, proof2) ->
		(is_complete proof1) && (is_complete proof2)
let rec width ctx ty goaltype = 
	if (type_to ctx goaltype ty) 
	then (match ty with
		| _ when ty_eqv ctx ty goaltype -> 0  
		| TyArr (_, ty2) -> 1 + (width ctx ty2 goaltype)
		|_-> raise Not_found)
	else failwith "Infinite"	
		(*| TyAll ty -> spec_abs_type ctx ty_dest (typeSubstTop ty_dest ty)*)

let is_axiom genes ty = 
	(geneExists genes ty) && (List.length (snd (gpAllelesofGenes genes ty)) > 0)

let exists ctx proofs species = List.exists (fun proElt -> proofEquiv ctx species proElt) proofs
 	
let rec string_of_proof ?(output="txt") ?(separator="::") ?(line_break="") ctx species =
	let sty ty = string_of_type ctx ty Compact in
	let sto species = string_of_proof ~output ~separator ~line_break ctx species in
	match output with 
	|"txt" ->(match species with
		| Partial (ty,_) -> "Partial " ^ sty ty
		| ProgNode ty -> sty ty
		| ForAllElim (species, ty) -> line_break ^ sto species ^ separator ^ sty ty
		| ImplyElim (proof1, proof2) -> line_break ^ sto proof1 ^ separator ^ sto proof2)
	|"dot" -> 
		"digraph G {\nval=\"0.4\";\ncenter=\"1\";"^"\nrankdir=\"TB\";\nspline = \"true\";" ^ 
 		 (dotGraph ctx species) ^ "\n}"
	|_->failwith "Unsupported format"	


(****************************************************)
(* Returns all possible genotypes of species        *)
(* with alleles in genes as building blocks         *)
(****************************************************)				
let rec gpGenotypesOfSpecies genes species = 
	match species with 
	| Partial _ ->failwith "Incomplete species" 
	| ProgNode ty -> 
		let tyGenes = snd (gpAllelesofGenes genes ty) in
		List.map (fun g -> In (alleId g)) tyGenes
	| ForAllElim (species, ty) -> 
			List.map (fun prog -> TypeFunc (prog, ty)) (gpGenotypesOfSpecies genes species) 
	| ImplyElim (proof1, proof2) ->
			let progs1 = gpGenotypesOfSpecies genes proof1 in
			assert (List.length progs1 > 0);
			let progs2=  gpGenotypesOfSpecies genes proof2 in
			assert (List.length progs2 > 0);
			ListLabels.fold_left progs1 ~init:[ ]
			~f:(fun acc progFunc -> 
				(List.map (fun progArg -> ObjFunc (progFunc, progArg)) progs2) @ acc ) 
		
let rec gtypeGenotypesWithinDistance genes species gtype dist = 
	let allGtypes = 	
		(match species , gtype with 
		| ProgNode ty , In id -> 
			let tyGenes = snd (gpAllelesofGenes genes ty) in
			List.map (fun g -> In (alleId g)) tyGenes
		| (ImplyElim (proof1, proof2)) , (ObjFunc (gtype1 , gtype2)) ->
			let progs1  = gtypeGenotypesWithinDistance genes proof1 gtype1 dist in
			let progs2  = gtypeGenotypesWithinDistance genes proof2 gtype2 dist in
			ListLabels.fold_left progs1 ~init:[ ]
			~f:(fun acc progFunc -> 
				(List.map (fun progArg -> ObjFunc (progFunc, progArg)) progs2) @ acc )) in
	
	List.filter (fun g -> (gtypeDistance (gpContext genes) g gtype) <= dist) allGtypes 
	
	
let term_of_program ?(debug=false) genes species prog = 
		let ctx = gpContext genes in
		let ste te = string_of_term ctx te Compact in
		let norm te = normalize ctx te in
		
	let findTerm id ty = 
		let _, lst = gpAllelesofGenes genes ty in
		alleExpr (List.find (fun g-> (alleId g) = id) lst) in 
		
	let rec inner = function
	| ProgNode ty, In id -> findTerm id ty
	| ForAllElim (species, ty1), TypeFunc (prog, ty2) -> 
		assert (ty_eqv ctx ty1 ty2); 
		normalize ctx (TmTApp(inner (species, prog), ty1))
	| (ImplyElim (proof1, proof2)) , (ObjFunc (prog1, prog2)) ->
		normalize ctx (TmApp((inner(proof1, prog1)),(inner(proof2, prog2)))) 	
	| _->failwith "Invalid pair species/program" in
	
	inner (species,prog)



let rec gpePotentialNum genes species = 
	let tnum ty = if (geneExists genes ty) 
		then (List.length (snd (gpAllelesofGenes genes ty)))
		else 0 in
	 match species with
	 | ProgNode ty -> tnum ty
	 | ForAllElim (species, ty) -> gpePotentialNum genes species
	 | ImplyElim (proof1, proof2) -> (gpePotentialNum genes proof1) * (gpePotentialNum genes proof2)
	 | Partial _ -> 0

	
let prog_num genes proofs = 
	List.fold_left  (fun acc species -> (gpePotentialNum genes species) + acc) 0 proofs			


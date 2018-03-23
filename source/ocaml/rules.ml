open Array

open Utils.Error
open Utils.Pervasive
open Utils.RandSelect
open Utils.StringSpecial

open Syntax
open Binding
open PrintExpr
open Core
open GPUtils
open Pointers
open BlockPopulation.Gene
open BlockPopulation

type formation_rule = context->gene_pool->t_Expr->t_Expr list
type formation_pred = context->gene_pool->t_Expr->bool
type constraint_function = gene_pool->t_Expr->bool		
type rule_app = {
	rule : formation_rule;
	accept : formation_pred;
	mutable app:bool}
type grow_unit = Block | Complexity of int	
	
let rec rules = [| 
	{rule = rule00; accept = rule00Accept; app = true} ;
	{rule = rule01; accept = rule01Accept; app = true} ;
	{rule = rule02; accept = rule02Accept; app = true};
	{rule = rule03; accept = rule03Accept; app = true} ;
	{rule = rule04; accept = rule04Accept; app = true} ;
	{rule = rule05; accept = rule05Accept; app = true} (*;
	{rule = rule06; app = true} ;
	{rule = rule07; app = true} ;
	{rule = rule08; app = true} ;
	{rule = rule09; app = true} ;*)|]
	
	

and get_applicable_rules ctx genes expr  =
	ListLabels.map
		(ListLabels.filter
			(List.filter (fun ra -> ra.app) (Array.to_list rules))
			~f:(fun ra -> ra.accept ctx genes expr))
	~f:(fun ra -> ra.rule)
	

and growToComplexity ?(debug=false) ?(growUnit=Block)
	genes constraintFunc
	?(onSuccess = fun _ _ ->( ))
	?(onFailure = fun _ -> ( )) = 
	
	let final_comp = (comp genes) + 
		(match growUnit with 
		|Block ->  1
		|Complexity x -> x) in
	
	let ctx = gpContext genes in
	let ste expr = string_of_expr ctx expr Compact in
	let go_on = ref true in
	let previous_val = ref (comp genes, 0) in
	let max_try = 160 in
	
	while !go_on do 
	try 
					
		let seed = seedSelect genes in
		
		chooseNapplySomeRule ~debug ~growUnit
			genes constraintFunc seed 
			onSuccess onFailure;
			
		prf ("\nCurrent total complexity of BlockPopulation is " ^ string_of_int (comp genes));  
		previous_val := (comp genes), (
			if fst !previous_val = comp genes 
				then (snd !previous_val) + 1
				else 0);
		if (comp genes > final_comp || 
			(snd !previous_val) >= max_try) then go_on := false;
		prf ("\n\t" ^ string_of_int (snd !previous_val))
	with _->( )
    done
    
and chooseNapplySomeRule  ?(debug=false) ?(growUnit=Block)
		genes constraintFunc seed  
		(onSuccessCallBack:gene_pool->t_Expr list->unit) 
		onFailureCallBack = 
		
	let ctx = gpContext genes in
	let actual_rule_list = get_applicable_rules ctx genes (Te (alleExpr seed,None)) in
		
	if List.length actual_rule_list = 0 
      then begin 
        let mess = "No rules to handle expression" in 
        onFailureCallBack mess; failwith mess end
      else begin
      try
	   (	
		  
	   
		let new_exprs = 
				List.map (function Te(te,_) -> te | _->failwith "Invalid expression")
				(new_subset ~debug  ~constraintFunc ctx genes (Te (alleExpr seed,None)) actual_rule_list) in
			match List.length new_exprs with
			| 0 ->	 
				let mess = "Failed, not sure why" in 
				failwith mess
			| card ->
				let seedTy = typeof ctx (alleExpr seed) in 
				let ancestorTag = (Some (seedTy, alleId seed)) in
				let ins selectionProb expr = 
					insert_expression genes expr ancestorTag NoSpec;
					let new_gene = teFind genes expr in 
					alleSetSelectionProb new_gene selectionProb in 
				let toExpr te = Te(te,None) in
				match growUnit with 
				|Block ->
					let new_expr = List.hd new_exprs in   
					let selectionProb = (alleSelectionProb seed) /. 2.0 in					
					alleSetSelectionProb seed selectionProb;
					ins selectionProb new_expr;
					onSuccessCallBack genes [toExpr new_expr]
				|Complexity comp ->
					let selectionProb = (alleSelectionProb seed) /. (float card) in
					alleSetSelectionProb seed selectionProb;
					List.iter (ins selectionProb) new_exprs;		    
					onSuccessCallBack genes (List.map toExpr new_exprs))      
	  
	  with Failure x -> begin onFailureCallBack x ; failwith x end
      end   
	  
(* Each descendant gene included in a new_subset *)
(* is new and meets constraints                  *)    
and  new_subset 
		?(debug=true) ?(max_trials=10) 
		?(constraintFunc=(fun _ _ -> true))
		ctx genes (seed : t_Expr) 
		(rule_list:formation_rule list) =
	
	let results = ref [ ] in
	let ctr = ref max_trials in
	let success = ref false in
	let constraint_filter lst = List.filter (constraintFunc genes) lst in
	while !ctr > 0 && (not !success) do
		begin try 
			let rule = List.hd (choose_random rule_list) in   
			let rawResults = constraint_filter (apply_rule ctx genes rule seed) in
			let filter expr = 
				match diff ctx genes with 
				|x when x > 0 -> true
				|_ ->  match expr with
					|Ty _ -> false
					|Te (te,_) -> not (alleleExists genes te) in
						
			results := List.filter filter rawResults;
			if (List.length !results > 0) then success := true
		with _ -> ( ) end;
		ctr := !ctr-1
	done;
	!results
	
and apply_rule ctx genes rule (exp:t_Expr) = 
	let simplify_expr expr = 
		match expr with 
		| Ty (ty, n) -> Ty (simplify_ty ctx ty, n)
		| Te (te, n) -> Te (normalize ctx te, n)
	in List.map simplify_expr (rule ctx genes exp)


and  diff ctx genes = (context_length ctx) - (context_length (gpContext genes))

and get_bounded_fvars ctx genes = 
	let rec gv ind = 
	if ind <= 0 then [] else 
	match get_binding ctx (ind - 1) with
		| BoundedTeVarBind _  
		| TeVarBind _ 
		| TmAbbBind _ -> TmVar (ind -1) :: gv (ind - 1)
		| _-> gv (ind-1) in
	gv (diff ctx genes)
and get_terms_from_pool ctx genes ty = 
	let gene_ty = typeShift (-1 * (diff ctx genes)) ty in
	ListLabels.map (get_terms genes
				~predTy:(fun ty -> ty_eqv (gpContext genes) gene_ty ty)) 
				~f:(termShift (diff ctx genes)) 
		
			
and rule00Accept ctx _ expr = 
	match expr with 
	| Te (te, _) -> 
		(match simplify_ty ctx (typeof ctx te) with 
		| TyAll _ -> true
		| _ -> false)
	| _-> false
and rule00 ?(debug=false) ctx genes expr =
	match expr with
	| Te (te, _) -> 
		(match simplify_ty ctx (typeof ctx te) with
			| TyAll _ -> 
				ListLabels.map 
				(List.map (fun ty -> typeShift (diff ctx genes) ty) (get_types genes))
				~f:(fun ty -> Te(TmTApp(te, ty), None))
			|  _ -> raise BlockConstructionFailure)
	| Ty _ -> raise BlockConstructionFailure


and rule01Accept ctx _ expr = 
	match expr with 
	| Te (TmAbs(_,_), _)->true
	| _-> false
and rule01 ?(debug=false) ctx genes expr =
	match expr with
	| Te (TmAbs(ty, te), _) -> 
		let ctx', _ =  fresh_fvar ctx ty in
		let seed_expr = (Te(te,None)) in
		let rule_list = get_applicable_rules ctx' genes seed_expr in
		ListLabels.map
		(ListLabels.fold_left
			(new_subset ctx' genes seed_expr rule_list)
			~init:[ ]
			~f:(fun lst -> function Te (te,_) -> te::lst |_->lst))	
		~f:(fun te -> Te(TmAbs(ty, te), None)) 	
	| _ -> raise BlockConstructionFailure
		
and rule02Accept ctx _ expr = 
	match expr with 
	| Te (te,_)->
		(match simplify_ty ctx (typeof ctx te) with
		| TyArr _ ->true
		|_->false)
	| _-> false
and rule02 ?(debug=false) ctx genes expr =
	match simplify_ty ctx (typeof ctx (ter expr)) with
	| TyArr (ty1, ty2) ->
			let manageTe str = normalize ctx (
				TmApp ((TmTApp (TmTApp (
				ter (parse_string ("Lam X . Lam Y. lam s : X->Y . lam x : X ." ^ str) ctx),
				ty1), ty2)) , (ter expr))) in 
				
				List.map (fun str ->Te (manageTe str, None))
				["s x"; "Lam Z . lam y : Y->Z . y (s x)"]
			
	| _ -> raise BlockConstructionFailure




and rule03Accept ctx genes expr = 
	match expr with 
	| Te (TmApp (_,_) ,_) ->true
	| _-> false
and rule03 ?(debug=false) ctx genes expr =
	match expr with
	| Te (TmApp(teL, teR),_) ->
		 let tyL = simplify_ty ctx (typeof ctx teL) in
		 let gene_tyL = typeShift (-1 * (diff ctx genes)) tyL in
			let rightTerms = 
			ListLabels.map	
				(get_terms genes
				~predTy:(fun ty -> ty_eqv (gpContext genes) gene_tyL ty)) 
				~f:(termShift (diff ctx genes)) in
			ListLabels.map rightTerms 
			~f:(fun lTe -> Te(TmApp(lTe, teR), None))
	| _ -> raise BlockConstructionFailure


and rule04Accept ctx genes expr = 
	match expr with 
	| Te (te,_)->
		(match simplify_ty ctx (typeof ctx te) with
		| TyArr _ ->true
		|_->false)
	| _-> false
and rule04 ?(debug=false) ctx genes expr =
	match simplify_ty ctx (typeof ctx (ter expr)) with
	|TyArr(ty1, ty2) -> 
			(ListLabels.map (get_terms_from_pool ctx genes ty1)
			~f:(fun rTe -> Te(TmApp((ter expr), rTe), None))) 
	| tyArg ->	
	let gene_ty = typeShift (-1 * (diff ctx genes)) tyArg in
	let termLst =
		ListLabels.map	
			(get_terms genes
				~predTy:(function 
					|TyArr (ty1, _) when 
						ty_eqv (gpContext genes) ty1 gene_ty -> true
					|_->false))
			~f:(termShift (diff ctx genes)) in
		ListLabels.map termLst 
			~f:(fun rTe -> Te(TmApp(rTe, (ter expr)), None))
	
and rule05Accept ctx genes expr = 
	if diff ctx genes > 0 then false
	else match expr with 
	| Te (TmAbs(_,_) , _) ->true
	| _-> false	
and rule05 ctx genes expr =
	let back_ctx  = (gpContext genes) in
	
	let rec rft_term ctx_local ter = 
		 let termLst ty =
			(ListLabels.filter (get_bounded_fvars ctx_local genes) 
				~f:(fun te -> ty_eqv ctx_local (typeof ctx_local te) ty)) @
			(let ty_back = typeShift (-1 * (diff ctx_local genes)) ty in
			let predTy tyElt = ty_eqv (gpContext genes) tyElt ty_back in
			(ListLabels.map (get_terms ~predTy genes)
			~f:(fun te -> termShift (diff ctx_local genes) te))) in
			
		match ter, typeof ctx_local ter  with 
		|  TmAbs (ty, ter), _ ->
			let (new_ctx, var) = fresh_fvar ctx_local ty in
			ListLabels.map (rft_term new_ctx ter) 
			~f:(fun te -> TmAbs(ty, te))
		
		| TmTAbs _, _ |  _ , TyAll _ ->
			ListLabels.map
			(get_types genes 
			~pred:(fun ty -> complexity_of_expr (Ty (ty, None)) < 50))
			~f:(fun ty -> TmTApp (ter, typeShift (diff ctx_local genes) ty)) 
		| TmApp (t1, t2), _ when Random.float 1.0 > 0.5->
			List.map (fun (t1,t2) -> TmApp(t1,t2))
			(List.combine (rft_term ctx_local t1) (rft_term ctx_local t2))
		| _, ty  when  Random.float 1.0 > 0.75 -> termLst ty		
		|  _, TyArr(ty_left, ty_right) ->
				
					let p1  = (fun ( ) -> 
						ListLabels.map (termLst ty_left) 
						~f:(fun ter_right -> TmApp(ter, ter_right)))  in
					p1( )
					(*
					
					and p3 = (fun ( ) -> 
						let ty = typeShift (-1 * diff) (TyArr(ty_left, ty_right)) in
						let ter_left = rand_func_from g_pop ty in
						TmApp(termShift diff ter_left, ter)) 
						
					and p4 = (fun ( ) -> 
						deb "in p4";
						TmAbs (ty_left, 
							TmApp (termShift 1 ter, TmVar 0)))
					
					in (choose_random [p1 ; p2; p3; p4]) ( )*)
					
			|  TmVar x, _ -> (*In sub_case 4*)
				let var = TmVar x in
				let tyvar = typeof ctx_local var in
				failwith "So far"		
				
			|_-> raise BlockConstructionFailure in
		 
		match expr with
		| Te (ter, _) -> 
			ListLabels.map (rft_term ctx ter)
			~f:(fun te -> Te(normalize ctx te, None))
		| _-> raise BlockConstructionFailure	
		
and rule06Accept ctx genes expr = 
	if diff ctx genes > 0 then false else
	match expr with 
	| Te (te,_) ->
		(match te , (simplify_ty ctx (typeof ctx te)) with
		| TmVar _ , TyArr _ ->true
		| _ -> false)
	| _-> false

	
	
		
		
		
		
		

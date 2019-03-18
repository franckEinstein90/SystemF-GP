type evaluation = float option
type genoStatus = Dead of evaluation | Alive of evaluation | Unborn
type 'a genoTree = 
	GeneLeaf of  'a * genoStatus | 
	GeneTree of  'a * 'a genoTree array

let makeLeaf ?(stat=Unborn) elt = 
	GeneLeaf (elt , stat)
	
let rec card_genoTree ?(pred = fun _ _ ->true) gt = 
	match gt with 
	| GeneLeaf (a, stat) -> 
		if (pred a stat) then 1 else 0
	| GeneTree (_ , gta) ->
		Array.fold_left 
		(fun acc gt -> acc + (card_genoTree ~pred gt))
		0 gta

let paths ?(pred = fun _ _ -> true) gt root = 
	
	let rec inner soFar gt = 
	match gt with 
	|GeneLeaf (node, stat) -> 
		if (pred node stat) 
			then [soFar @ [node], stat]
			else [ ]
	
	|GeneTree (node, gta) -> 
		let soFar = soFar @ [node] in
		Array.fold_left
		(fun accLst gt ->  inner soFar gt @ accLst)
		[ ] gta in

	inner [root] gt

(*	let add soFar refer = 
		List.map 
		(fun soFarElt -> soFarElt @ [refer]) 
		soFar in
	
	
	t*)


let rec tree_iter ?(f = fun _ -> ( )) gt =
	match gt with 
	|GeneLeaf (node, _) -> f node
	|GeneTree (node, gta) -> 
		f node;
		Array.iter (fun gt ->  tree_iter ~f gt) gta 


type evaluation = float option
type genoStatus = Dead of evaluation | Alive of evaluation | Unborn
type 'a genoTree = 
	GeneLeaf of  'a * genoStatus | 
	GeneTree of  'a * 'a genoTree array
	
val makeLeaf : ?stat:genoStatus -> 'a -> 'a genoTree
val card_genoTree : ?pred:('a->genoStatus->bool) -> 'a genoTree->int
val paths : ?pred:('a->genoStatus->bool) -> 'a genoTree -> 'a -> ('a list * genoStatus) list
val tree_iter : ?f:('a->unit) -> 'a genoTree -> unit

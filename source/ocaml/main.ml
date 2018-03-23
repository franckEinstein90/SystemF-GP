open Utils.Pervasive
open Utils.StringSpecial
open Utils.ListSpecial 

open Syntax
open Binding
open Primitive_set_visual
open BlockPopulation
open population_visual

let proofWidthDefault = ref 5
let proofDepthDefault = ref 4
let genePrecisionDefault = ref 12
let blockPopulationComplexityStep = ref 1000
let geneMaxSize = ref 50 
let popSize = ref 10
let autoRun = ref false



let app_dir = Sys.getcwd( ) 
let app_path = Sys.executable_name 
let current_context = ref empty_context



let gene_pool_init precisionLabel 
proofDepthLabel proofWidthLabel genePoolIncrementLabel 
popSizeLabel maxGeneSize ( ) = 

	let read_num label defVal = 
		try int_of_string label#text															   
		with _-> !defVal in
	
	
	let precision = read_num precisionLabel genePrecisionDefault in
	let gpIncrement = read_num genePoolIncrementLabel blockPopulationComplexityStep in
	let pDepth = read_num proofDepthLabel proofDepthDefault in
	let pWidth = read_num proofWidthLabel proofWidthDefault in
	let mgSize = read_num maxGeneSize geneMaxSize in
	let popLiveSize = read_num popSizeLabel popSize in
	
	make_population_visual current_context 
	((if !autoRun then [Auto] else [ ]) @
	[	(*LoadCasesOnOpen  (app_dir ^ "\\TestCases\\Fibonnaci\\case_fib_1.txt");*)
		GenePrecision precision; 
		BlocPopInitComplexity gpIncrement;
		ProofDepth pDepth;
		MaxProofWidth pWidth;
		PopSize popLiveSize;
		MaxGeneSize mgSize;
		OpenFileDlgPathGP (app_dir ^ "\\TestCases\\")]) 
	
let ps_set_call psetLabel ( ) =
	show_ps_window
		current_context 
		psetLabel 
		[OpenFileDlgPath (app_dir ^ "\\Languages\\")]
		
		
let destroy () = GMain.Main.quit ()

let main () = 
	Random.self_init( );
	let window = GWindow.window ~border_width:10 () in
		un (window #connect#destroy ~callback:GMain.Main.quit);
	let hBox = GPack.hbox ~packing:window#add () in
	let leftPanel = GPack.vbox 
	~border_width:8 ~height:350 ~width:500 ~spacing:8 ~packing:hBox#add () in    
    let boxMenu = GPack.vbox ~width:300 ~packing:hBox#add () in
	
	let panel title = 
		let pan = GBin.frame ~label:(title^ "...") ~packing:boxMenu#pack ( ) in
		GPack.vbox ~width:300 ~packing:pan#add ( ) in
	
    let appPanel =  panel "App information..." in 
	let contPanel = panel "Context..." in
	let gpPanel = panel "Genetic program..." in
	
	un(GEdit.entry 
		~editable:false
		~text:app_dir
        ~width:1000  
        ~packing:appPanel#add ( ));
        
        
	let primitive_set_label = GEdit.entry 
      ~editable:false
		~text:"No context loaded"
		~width:500 
		~packing:contPanel#add () in
    
    let gpPanelEdit label str =
    let box = GPack.hbox ~width:300 ~packing:gpPanel#add () in
    GMisc.label ~text:label ~width:200 ~justify:`RIGHT ~packing:box#add( ); 
    GEdit.entry 
		~editable:true
		~text:str  
        ~width:100  
        ~packing:box#add ( ) in  
    
	let precisionLabel, proofDepthLabel, proofWidthLabel, genePoolIncrementLabel, popSizeLabel =
	gpPanelEdit "Gene difference precision = "  (string_of_int !genePrecisionDefault), 
	gpPanelEdit "Proof depth = "   (string_of_int !proofDepthDefault),
	gpPanelEdit "Proof width = "   (string_of_int !proofWidthDefault),
	gpPanelEdit "Gene pool increment = "   (string_of_int !blockPopulationComplexityStep), 
	gpPanelEdit "Population size = " (string_of_int !popSize) in
	
	let maxGeneSize = 
	gpPanelEdit "Gene max size = "  (string_of_int !geneMaxSize) in
	
	let psbutton, gene_pop_btn, auto_btn= 
	GButton.button ~label:"Load definitions" ~packing:contPanel#add (),
	GButton.button ~label:"Start" ~packing:gpPanel#add ( ),
	GButton.check_button  ~label:"Auto" ~packing:gpPanel#add () in
	
	let set_auto ( ) = 
		if auto_btn#active then autoRun := true
		else autoRun := false in
	un(auto_btn#connect#toggled ~callback:set_auto);
	un (psbutton#connect#clicked ~callback:(ps_set_call primitive_set_label));
	un (gene_pop_btn#connect#clicked ~callback:
	(gene_pool_init precisionLabel 
	proofDepthLabel proofWidthLabel genePoolIncrementLabel popSizeLabel maxGeneSize));
 	window#show ( );
	GMain.Main.main ()

let _ =	 main( )
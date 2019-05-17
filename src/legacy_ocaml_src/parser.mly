/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Utils.Error
open Utils.Pervasive
open Syntax
open Binding
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* 

   We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 
 */

/* Keyword tokens */
%token <Utils.Error.info> TYPE
%token <Utils.Error.info> INERT
%token <Utils.Error.info> LAMBDA
%token <Utils.Error.info> FIX
%token <Utils.Error.info> LETREC
%token <Utils.Error.info> USTRING
%token <Utils.Error.info> UNIT
%token <Utils.Error.info> UUNIT
%token <Utils.Error.info> IF
%token <Utils.Error.info> THEN
%token <Utils.Error.info> ELSE
%token <Utils.Error.info> TRUE
%token <Utils.Error.info> FALSE
%token <Utils.Error.info> BOOL
%token <Utils.Error.info> TIMESFLOAT

/*Numeric types */
%token <Utils.Error.info> UFLOAT
%token <Utils.Error.info> UINT
%token <Utils.Error.info> NAT


%token <Utils.Error.info> SUCC
%token <Utils.Error.info> PRED
%token <Utils.Error.info> ISZERO

%token <Utils.Error.info> SOME
%token <Utils.Error.info> LET
%token <Utils.Error.info> IN
%token <Utils.Error.info> AS
%token <Utils.Error.info> ALL

/* Identifier and constant value tokens */
%token <string Utils.Error.withinfo> UCID  /* uppercase-initial */
%token <string Utils.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Utils.Error.withinfo> INTV
%token <float Utils.Error.withinfo> FLOATV
%token <string Utils.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Utils.Error.info> APOSTROPHE
%token <Utils.Error.info> DQUOTE
%token <Utils.Error.info> ARROW
%token <Utils.Error.info> BANG
%token <Utils.Error.info> BARGT
%token <Utils.Error.info> BARRCURLY
%token <Utils.Error.info> BARRSQUARE
%token <Utils.Error.info> COLON
%token <Utils.Error.info> COLONCOLON
%token <Utils.Error.info> COLONEQ
%token <Utils.Error.info> COLONHASH
%token <Utils.Error.info> COMMA
%token <Utils.Error.info> DARROW
%token <Utils.Error.info> DDARROW
%token <Utils.Error.info> DOT
%token <Utils.Error.info> EOF
%token <Utils.Error.info> EQ
%token <Utils.Error.info> EQEQ
%token <Utils.Error.info> EXISTS
%token <Utils.Error.info> GT
%token <Utils.Error.info> HASH
%token <Utils.Error.info> LCURLY
%token <Utils.Error.info> LCURLYBAR
%token <Utils.Error.info> LEFTARROW
%token <Utils.Error.info> LPAREN
%token <Utils.Error.info> LSQUARE
%token <Utils.Error.info> LSQUAREBAR
%token <Utils.Error.info> LT
%token <Utils.Error.info> RCURLY
%token <Utils.Error.info> RPAREN
%token <Utils.Error.info> RSQUARE
%token <Utils.Error.info> SEMI
%token <Utils.Error.info> SLASH
%token <Utils.Error.info> STAR
%token <Utils.Error.info> TRIANGLE
%token <Utils.Error.info> USCORE
%token <Utils.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Binding.context -> (Binding.command list * Binding.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
	| EOF
      { fun ctx -> [],ctx }
	
	| Command SEMI toplevel
      { fun ctx ->
          let cmd, ctx = $1 ctx in
          let cmds, ctx = $3 ctx in
          cmd::cmds, ctx }

/* A top-level command */
Command :
	| Term 
      { fun ctx -> (let t = $1 ctx in Eval (Te (t, None))), ctx }
  
  
	/* Atomic type definition or evaluation */
	| UCID 
	{ fun ctx -> if is_name_bound ctx $1.v 
		then (Eval (expr_of_name ctx $1.v), ctx)
		else  (Bind ($1.v, TyVarBind), add_binding ctx $1.v TyVarBind) }
	
	/* Composite type definition */
	| UCID EQ Type
	{ fun ctx -> if is_name_bound ctx $1.v  
		then failwith ("\nParser error, unable to reassign type name " ^ $1.v)
		else let right_hand_side = TyAbbBind ($3 ctx) in
		(Bind ($1.v, right_hand_side), add_binding ctx $1.v right_hand_side) }
	
	/* Request to evaluate a type */
	| Type 
	  { fun ctx -> (let t = $1 ctx in Eval (Ty (t, None))), ctx }
	
	/* Defining a new free term variable */
	| LCID Binder 
      { fun ctx -> 
			let bind = $2  ctx in
			Bind ($1.v, bind), add_binding ctx $1.v bind }
      

/* Right-hand sides of top-level bindings */
Binder :
 | COLON Type
      { fun ctx -> TeVarBind ($2 ctx)}
  | EQ Term 
      { fun ctx -> TmAbbBind($2 ctx, None) }

/* All type expressions */
Type :
  | ArrowType
                { $1 }
  | ALL UCID DOT Type
      { fun ctx ->
          let ctx1 =  add_tvar_name ctx $2.v in
          TyAll ($4 ctx1) }

/* Atomic types are those that never need extra parentheses */
AType :
	| LPAREN Type RPAREN  
           { $2 } 
	
	| UCID 
      { fun ctx ->
          if is_name_bound ctx $1.v 
          then TyVar (index_of_name ctx $1.v)
          else failwith ("Parsing::AType --> unknown atomic type:" ^ $1.v) }
	
	
	| USTRING
      { fun ctx -> TyString }
      
      
	| BOOL
      { fun ctx -> TyBool }	
	| UFLOAT
    { fun ctx -> TyFloat }
	| NAT
    { fun ctx -> TyNat }
	| UINT
	{ fun ctx -> TyInt }
  
	| UUNIT
      { fun ctx -> TyUnit } 



/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
  | AType ARROW ArrowType
     { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | AType
            { $1 }

Term :
  |  LPAREN Term RPAREN
		{$2}  
 
  |  AppTerm
      { $1 }

  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
		  let ty = $4 ctx in
          let ctx1 = add_fvar_name ctx $2.v ty in
          TmAbs (ty, $6 ctx1) }


  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($2 ctx, $4 ctx, $6 ctx) }
      
      
  | LAMBDA UCID DOT Term 
      { fun ctx -> 
          let ctx1 =  add_tvar_name ctx $2.v in
          TmTAbs ($4 ctx1) }

AppTerm :

  | ATerm
      { $1 }
      
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp (e1, e2) }
  
  | LPAREN Term RPAREN ATerm
      {	 fun ctx ->
          let e1 = $2 ctx in
          let e2 = $4 ctx in
          TmApp (e1, e2) }
  
  | Term LPAREN Term RPAREN
      {	 fun ctx ->
          let e1 = $1 ctx in
          let e2 = $3 ctx in
          TmApp (e1, e2) }
          
  | TIMESFLOAT ATerm ATerm
      { fun ctx -> TmTimesfloat ($2 ctx, $3 ctx) }
  | SUCC ATerm
      { fun ctx -> TmSucc ($2 ctx) }
  | PRED ATerm
     { fun ctx -> TmPred ($2 ctx) }
  | ISZERO ATerm
      { fun ctx -> TmIsZero ($2 ctx) }
  | AppTerm LSQUARE Type RSQUARE
      { fun ctx ->
          let t1 = $1 ctx in
          let t2 = $3 ctx in
          TmTApp (t1, t2) }
  | LPAREN Term RPAREN LSQUARE Type RSQUARE
      {  fun ctx ->
          let t1 = $2 ctx in
          let t2 = $5 ctx in
          TmTApp (t1, t2) }
  

FieldTypes :
    /* empty */
      { fun ctx i -> [] }
  | NEFieldTypes
      { $1 }

NEFieldTypes :
    FieldType
      { fun ctx i -> [$1 ctx i] }
  | FieldType COMMA NEFieldTypes
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

FieldType :
    LCID COLON Type
      { fun ctx i -> ($1.v, $3 ctx) }
  | Type
      { fun ctx i -> (string_of_int i, $1 ctx) }


/* Atomic terms are ones that never require extra parentheses */
ATerm :
  | LCID 
      { fun ctx ->
          TmVar(index_of_name ctx $1.v) }
  | STRINGV
      { fun ctx -> TmString ($1.v) }
  | TRUE
      { fun ctx -> TmTrue }
  | FALSE
      { fun ctx -> TmFalse }
  | FLOATV
      { fun ctx -> TmFloat ($1.v) }
  | INTV
      { fun ctx -> TmInt ($1.v) }
  
Fields :
    /* empty */
      { fun ctx i -> [] }
  | NEFields
      { $1 }

NEFields :
    Field
      { fun ctx i -> [$1 ctx i] }
  | Field COMMA NEFields
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field :
    LCID EQ Term
      { fun ctx i -> ($1.v, $3 ctx) }
  | Term
      { fun ctx i -> (string_of_int i, $1 ctx) }


/*   */

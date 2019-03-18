type token =
  | TYPE of (Utils.Error.info)
  | INERT of (Utils.Error.info)
  | LAMBDA of (Utils.Error.info)
  | FIX of (Utils.Error.info)
  | LETREC of (Utils.Error.info)
  | USTRING of (Utils.Error.info)
  | UNIT of (Utils.Error.info)
  | UUNIT of (Utils.Error.info)
  | IF of (Utils.Error.info)
  | THEN of (Utils.Error.info)
  | ELSE of (Utils.Error.info)
  | TRUE of (Utils.Error.info)
  | FALSE of (Utils.Error.info)
  | BOOL of (Utils.Error.info)
  | TIMESFLOAT of (Utils.Error.info)
  | UFLOAT of (Utils.Error.info)
  | UINT of (Utils.Error.info)
  | NAT of (Utils.Error.info)
  | SUCC of (Utils.Error.info)
  | PRED of (Utils.Error.info)
  | ISZERO of (Utils.Error.info)
  | SOME of (Utils.Error.info)
  | LET of (Utils.Error.info)
  | IN of (Utils.Error.info)
  | AS of (Utils.Error.info)
  | ALL of (Utils.Error.info)
  | UCID of (string Utils.Error.withinfo)
  | LCID of (string Utils.Error.withinfo)
  | INTV of (int Utils.Error.withinfo)
  | FLOATV of (float Utils.Error.withinfo)
  | STRINGV of (string Utils.Error.withinfo)
  | APOSTROPHE of (Utils.Error.info)
  | DQUOTE of (Utils.Error.info)
  | ARROW of (Utils.Error.info)
  | BANG of (Utils.Error.info)
  | BARGT of (Utils.Error.info)
  | BARRCURLY of (Utils.Error.info)
  | BARRSQUARE of (Utils.Error.info)
  | COLON of (Utils.Error.info)
  | COLONCOLON of (Utils.Error.info)
  | COLONEQ of (Utils.Error.info)
  | COLONHASH of (Utils.Error.info)
  | COMMA of (Utils.Error.info)
  | DARROW of (Utils.Error.info)
  | DDARROW of (Utils.Error.info)
  | DOT of (Utils.Error.info)
  | EOF of (Utils.Error.info)
  | EQ of (Utils.Error.info)
  | EQEQ of (Utils.Error.info)
  | EXISTS of (Utils.Error.info)
  | GT of (Utils.Error.info)
  | HASH of (Utils.Error.info)
  | LCURLY of (Utils.Error.info)
  | LCURLYBAR of (Utils.Error.info)
  | LEFTARROW of (Utils.Error.info)
  | LPAREN of (Utils.Error.info)
  | LSQUARE of (Utils.Error.info)
  | LSQUAREBAR of (Utils.Error.info)
  | LT of (Utils.Error.info)
  | RCURLY of (Utils.Error.info)
  | RPAREN of (Utils.Error.info)
  | RSQUARE of (Utils.Error.info)
  | SEMI of (Utils.Error.info)
  | SLASH of (Utils.Error.info)
  | STAR of (Utils.Error.info)
  | TRIANGLE of (Utils.Error.info)
  | USCORE of (Utils.Error.info)
  | VBAR of (Utils.Error.info)

open Parsing;;
# 7 "parser.mly"
open Utils.Error
open Utils.Pervasive
open Syntax
open Binding
# 78 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* TYPE *);
  258 (* INERT *);
  259 (* LAMBDA *);
  260 (* FIX *);
  261 (* LETREC *);
  262 (* USTRING *);
  263 (* UNIT *);
  264 (* UUNIT *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* BOOL *);
  271 (* TIMESFLOAT *);
  272 (* UFLOAT *);
  273 (* UINT *);
  274 (* NAT *);
  275 (* SUCC *);
  276 (* PRED *);
  277 (* ISZERO *);
  278 (* SOME *);
  279 (* LET *);
  280 (* IN *);
  281 (* AS *);
  282 (* ALL *);
  283 (* UCID *);
  284 (* LCID *);
  285 (* INTV *);
  286 (* FLOATV *);
  287 (* STRINGV *);
  288 (* APOSTROPHE *);
  289 (* DQUOTE *);
  290 (* ARROW *);
  291 (* BANG *);
  292 (* BARGT *);
  293 (* BARRCURLY *);
  294 (* BARRSQUARE *);
  295 (* COLON *);
  296 (* COLONCOLON *);
  297 (* COLONEQ *);
  298 (* COLONHASH *);
  299 (* COMMA *);
  300 (* DARROW *);
  301 (* DDARROW *);
  302 (* DOT *);
    0 (* EOF *);
  303 (* EQ *);
  304 (* EQEQ *);
  305 (* EXISTS *);
  306 (* GT *);
  307 (* HASH *);
  308 (* LCURLY *);
  309 (* LCURLYBAR *);
  310 (* LEFTARROW *);
  311 (* LPAREN *);
  312 (* LSQUARE *);
  313 (* LSQUAREBAR *);
  314 (* LT *);
  315 (* RCURLY *);
  316 (* RPAREN *);
  317 (* RSQUARE *);
  318 (* SEMI *);
  319 (* SLASH *);
  320 (* STAR *);
  321 (* TRIANGLE *);
  322 (* USCORE *);
  323 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\005\000\
\005\000\004\000\004\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\006\000\006\000\003\000\003\000\003\000\
\003\000\003\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\010\000\010\000\011\000\011\000\
\012\000\012\000\009\000\009\000\009\000\009\000\009\000\009\000\
\013\000\013\000\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\001\000\003\000\001\000\002\000\002\000\
\002\000\001\000\004\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\001\000\003\000\001\000\006\000\
\006\000\004\000\001\000\002\000\004\000\004\000\003\000\002\000\
\002\000\002\000\004\000\006\000\000\000\001\000\001\000\003\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\000\000\001\000\001\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\014\000\019\000\000\000\045\000\046\000\
\015\000\000\000\016\000\018\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\048\000\047\000\044\000\001\000\000\000\
\055\000\000\000\000\000\006\000\010\000\000\000\000\000\027\000\
\000\000\000\000\043\000\000\000\000\000\000\000\032\000\033\000\
\034\000\000\000\000\000\000\000\000\000\007\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\031\000\000\000\000\000\005\000\008\000\000\000\000\000\
\012\000\002\000\000\000\020\000\000\000\000\000\000\000\000\000\
\011\000\000\000\029\000\030\000\035\000\000\000\000\000\000\000\
\000\000\000\000\036\000"

let yydgoto = "\002\000\
\025\000\026\000\027\000\028\000\046\000\029\000\030\000\031\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yysindex = "\028\000\
\001\000\000\000\245\254\000\000\000\000\079\255\000\000\000\000\
\000\000\159\255\000\000\000\000\000\000\159\255\159\255\159\255\
\007\255\246\254\223\254\000\000\000\000\000\000\000\000\050\255\
\000\000\232\254\241\254\000\000\000\000\008\255\150\255\000\000\
\251\254\011\255\000\000\079\255\248\254\159\255\000\000\000\000\
\000\000\000\255\143\255\143\255\079\255\000\000\000\000\211\254\
\001\255\001\000\079\255\005\255\143\255\000\000\079\255\143\255\
\079\255\000\000\143\255\143\255\000\000\000\000\241\254\154\255\
\000\000\000\000\221\254\000\000\247\254\241\254\040\255\249\254\
\000\000\143\255\000\000\000\000\000\000\079\255\079\255\028\255\
\241\254\241\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\227\254\084\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\255\000\000\000\000\222\254\064\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\255\073\255\
\000\000\000\000\000\000\000\000\000\000\025\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\255\044\255\000\000"

let yygindex = "\000\000\
\040\000\000\000\250\255\094\000\000\000\050\000\000\000\000\000\
\249\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 312
let yytable = "\037\000\
\023\000\057\000\038\000\079\000\013\000\044\000\039\000\040\000\
\041\000\051\000\004\000\021\000\005\000\045\000\064\000\033\000\
\034\000\048\000\009\000\051\000\011\000\012\000\013\000\054\000\
\076\000\021\000\021\000\021\000\001\000\048\000\058\000\047\000\
\004\000\042\000\026\000\026\000\043\000\050\000\063\000\051\000\
\055\000\052\000\024\000\024\000\067\000\059\000\051\000\051\000\
\070\000\056\000\072\000\077\000\003\000\025\000\025\000\004\000\
\075\000\005\000\006\000\060\000\065\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\081\000\
\082\000\023\000\023\000\017\000\047\000\035\000\020\000\021\000\
\022\000\003\000\022\000\022\000\026\000\078\000\026\000\006\000\
\083\000\066\000\007\000\008\000\024\000\010\000\024\000\043\000\
\043\000\014\000\015\000\016\000\003\000\068\000\009\000\025\000\
\024\000\025\000\035\000\020\000\021\000\022\000\000\000\043\000\
\043\000\043\000\043\000\000\000\000\000\049\000\023\000\000\000\
\000\000\000\000\000\000\023\000\000\000\023\000\000\000\022\000\
\000\000\000\000\000\000\000\000\022\000\036\000\022\000\000\000\
\061\000\062\000\043\000\043\000\000\000\000\000\000\000\000\000\
\000\000\043\000\069\000\000\000\004\000\071\000\005\000\000\000\
\073\000\049\000\000\000\000\000\009\000\000\000\011\000\012\000\
\013\000\007\000\008\000\000\000\000\000\007\000\008\000\080\000\
\017\000\047\000\007\000\008\000\000\000\000\000\000\000\000\000\
\000\000\035\000\020\000\021\000\022\000\035\000\020\000\021\000\
\022\000\000\000\035\000\020\000\021\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\060\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\000\000\000\000\
\000\000\074\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\004\000\000\000\
\005\000\006\000\000\000\000\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\000\000\000\000\
\000\000\000\000\017\000\018\000\019\000\020\000\021\000\022\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000"

let yycheck = "\006\000\
\000\000\010\001\010\000\011\001\034\001\039\001\014\000\015\000\
\016\000\055\001\006\001\046\001\008\001\047\001\060\001\027\001\
\028\001\024\000\014\001\055\001\016\001\017\001\018\001\031\000\
\060\001\060\001\061\001\062\001\001\000\036\000\038\000\027\001\
\062\001\027\001\010\001\011\001\047\001\062\001\045\000\055\001\
\046\001\034\001\010\001\011\001\051\000\046\001\055\001\055\001\
\055\000\039\001\057\000\061\001\003\001\010\001\011\001\006\001\
\064\000\008\001\009\001\055\001\060\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\078\000\
\079\000\010\001\011\001\026\001\027\001\028\001\029\001\030\001\
\031\001\003\001\010\001\011\001\060\001\046\001\062\001\009\001\
\061\001\050\000\012\001\013\001\060\001\015\001\062\001\012\001\
\013\001\019\001\020\001\021\001\062\001\052\000\062\001\060\001\
\055\001\062\001\028\001\029\001\030\001\031\001\255\255\028\001\
\029\001\030\001\031\001\255\255\255\255\024\000\055\001\255\255\
\255\255\255\255\255\255\060\001\255\255\062\001\255\255\055\001\
\255\255\255\255\255\255\255\255\060\001\055\001\062\001\255\255\
\043\000\044\000\055\001\056\001\255\255\255\255\255\255\255\255\
\255\255\062\001\053\000\255\255\006\001\056\000\008\001\255\255\
\059\000\060\000\255\255\255\255\014\001\255\255\016\001\017\001\
\018\001\012\001\013\001\255\255\255\255\012\001\013\001\074\000\
\026\001\027\001\012\001\013\001\255\255\255\255\255\255\255\255\
\255\255\028\001\029\001\030\001\031\001\028\001\029\001\030\001\
\031\001\255\255\028\001\029\001\030\001\031\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\055\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\056\001\255\255\255\255\
\255\255\056\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\255\255\006\001\255\255\
\008\001\009\001\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\255\255\255\255\
\255\255\255\255\026\001\027\001\028\001\029\001\030\001\031\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\055\001"

let yynames_const = "\
  "

let yynames_block = "\
  TYPE\000\
  INERT\000\
  LAMBDA\000\
  FIX\000\
  LETREC\000\
  USTRING\000\
  UNIT\000\
  UUNIT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  BOOL\000\
  TIMESFLOAT\000\
  UFLOAT\000\
  UINT\000\
  NAT\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  SOME\000\
  LET\000\
  IN\000\
  AS\000\
  ALL\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 134 "parser.mly"
      ( fun ctx -> [],ctx )
# 382 "parser.ml"
               :  Binding.context -> (Binding.command list * Binding.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Binding.context -> (Binding.command list * Binding.context) ) in
    Obj.repr(
# 137 "parser.mly"
      ( fun ctx ->
          let cmd, ctx = _1 ctx in
          let cmds, ctx = _3 ctx in
          cmd::cmds, ctx )
# 394 "parser.ml"
               :  Binding.context -> (Binding.command list * Binding.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 145 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval (Te (t, None))), ctx )
# 401 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Utils.Error.withinfo) in
    Obj.repr(
# 150 "parser.mly"
 ( fun ctx -> if is_name_bound ctx _1.v 
		then (Eval (expr_of_name ctx _1.v), ctx)
		else  (Bind (_1.v, TyVarBind), add_binding ctx _1.v TyVarBind) )
# 410 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Utils.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 156 "parser.mly"
 ( fun ctx -> if is_name_bound ctx _1.v  
		then failwith ("\nParser error, unable to reassign type name " ^ _1.v)
		else let right_hand_side = TyAbbBind (_3 ctx) in
		(Bind (_1.v, right_hand_side), add_binding ctx _1.v right_hand_side) )
# 422 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 163 "parser.mly"
   ( fun ctx -> (let t = _1 ctx in Eval (Ty (t, None))), ctx )
# 429 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Utils.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 167 "parser.mly"
      ( fun ctx -> 
			let bind = _2  ctx in
			Bind (_1.v, bind), add_binding ctx _1.v bind )
# 439 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 175 "parser.mly"
      ( fun ctx -> TeVarBind (_2 ctx))
# 447 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 177 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 455 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 182 "parser.mly"
                ( _1 )
# 462 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Utils.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 184 "parser.mly"
      ( fun ctx ->
          let ctx1 =  add_tvar_name ctx _2.v in
          TyAll (_4 ctx1) )
# 474 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 191 "parser.mly"
           ( _2 )
# 483 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Utils.Error.withinfo) in
    Obj.repr(
# 194 "parser.mly"
      ( fun ctx ->
          if is_name_bound ctx _1.v 
          then TyVar (index_of_name ctx _1.v)
          else failwith ("Parsing::AType --> unknown atomic type:" ^ _1.v) )
# 493 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 201 "parser.mly"
      ( fun ctx -> TyString )
# 500 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 205 "parser.mly"
      ( fun ctx -> TyBool )
# 507 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 207 "parser.mly"
    ( fun ctx -> TyFloat )
# 514 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 209 "parser.mly"
    ( fun ctx -> TyNat )
# 521 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 211 "parser.mly"
 ( fun ctx -> TyInt )
# 528 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 214 "parser.mly"
      ( fun ctx -> TyUnit )
# 535 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 222 "parser.mly"
     ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 544 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 224 "parser.mly"
            ( _1 )
# 551 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 228 "parser.mly"
  (_2)
# 560 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 231 "parser.mly"
      ( _1 )
# 567 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Utils.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Utils.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 234 "parser.mly"
      ( fun ctx ->
		  let ty = _4 ctx in
          let ctx1 = add_fvar_name ctx _2.v ty in
          TmAbs (ty, _6 ctx1) )
# 582 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Utils.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 241 "parser.mly"
      ( fun ctx -> TmIf(_2 ctx, _4 ctx, _6 ctx) )
# 594 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Utils.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 245 "parser.mly"
      ( fun ctx -> 
          let ctx1 =  add_tvar_name ctx _2.v in
          TmTAbs (_4 ctx1) )
# 606 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 252 "parser.mly"
      ( _1 )
# 613 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 255 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp (e1, e2) )
# 624 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 261 "parser.mly"
      (	 fun ctx ->
          let e1 = _2 ctx in
          let e2 = _4 ctx in
          TmApp (e1, e2) )
# 637 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 267 "parser.mly"
      (	 fun ctx ->
          let e1 = _1 ctx in
          let e2 = _3 ctx in
          TmApp (e1, e2) )
# 650 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ATerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 273 "parser.mly"
      ( fun ctx -> TmTimesfloat (_2 ctx, _3 ctx) )
# 659 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 275 "parser.mly"
      ( fun ctx -> TmSucc (_2 ctx) )
# 667 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 277 "parser.mly"
     ( fun ctx -> TmPred (_2 ctx) )
# 675 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 279 "parser.mly"
      ( fun ctx -> TmIsZero (_2 ctx) )
# 683 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 281 "parser.mly"
      ( fun ctx ->
          let t1 = _1 ctx in
          let t2 = _3 ctx in
          TmTApp (t1, t2) )
# 696 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Utils.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Utils.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Utils.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 286 "parser.mly"
      (  fun ctx ->
          let t1 = _2 ctx in
          let t2 = _5 ctx in
          TmTApp (t1, t2) )
# 711 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 294 "parser.mly"
      ( fun ctx i -> [] )
# 717 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 296 "parser.mly"
      ( _1 )
# 724 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FieldType) in
    Obj.repr(
# 300 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 731 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'FieldType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFieldTypes) in
    Obj.repr(
# 302 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 740 "parser.ml"
               : 'NEFieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Utils.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 306 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 749 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 308 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 756 "parser.ml"
               : 'FieldType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Utils.Error.withinfo) in
    Obj.repr(
# 314 "parser.mly"
      ( fun ctx ->
          TmVar(index_of_name ctx _1.v) )
# 764 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Utils.Error.withinfo) in
    Obj.repr(
# 317 "parser.mly"
      ( fun ctx -> TmString (_1.v) )
# 771 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 319 "parser.mly"
      ( fun ctx -> TmTrue )
# 778 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Utils.Error.info) in
    Obj.repr(
# 321 "parser.mly"
      ( fun ctx -> TmFalse )
# 785 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float Utils.Error.withinfo) in
    Obj.repr(
# 323 "parser.mly"
      ( fun ctx -> TmFloat (_1.v) )
# 792 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Utils.Error.withinfo) in
    Obj.repr(
# 325 "parser.mly"
      ( fun ctx -> TmInt (_1.v) )
# 799 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 329 "parser.mly"
      ( fun ctx i -> [] )
# 805 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 331 "parser.mly"
      ( _1 )
# 812 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Field) in
    Obj.repr(
# 335 "parser.mly"
      ( fun ctx i -> [_1 ctx i] )
# 819 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Field) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'NEFields) in
    Obj.repr(
# 337 "parser.mly"
      ( fun ctx i -> (_1 ctx i) :: (_3 ctx (i+1)) )
# 828 "parser.ml"
               : 'NEFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Utils.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Utils.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 341 "parser.mly"
      ( fun ctx i -> (_1.v, _3 ctx) )
# 837 "parser.ml"
               : 'Field))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 343 "parser.mly"
      ( fun ctx i -> (string_of_int i, _1 ctx) )
# 844 "parser.ml"
               : 'Field))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Binding.context -> (Binding.command list * Binding.context) )

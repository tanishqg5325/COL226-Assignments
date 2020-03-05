type token =
  | FLOAT of (float)
  | INDICE of (int * int)
  | RANGE of (Sheet.index * Sheet.index)
  | LP
  | RP
  | LB
  | RB
  | COMMA
  | COLON
  | COUNT
  | ROWCOUNT
  | COLCOUNT
  | SUM
  | ROWSUM
  | COLSUM
  | AVG
  | ROWAVG
  | COLAVG
  | MIN
  | ROWMIN
  | COLMIN
  | MAX
  | ROWMAX
  | COLMAX
  | ADD
  | SUBT
  | MULT
  | DIV
  | EQ
  | DELIMITER
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open Sheet;;
# 39 "a3.ml"
let yytransl_const = [|
  260 (* LP *);
  261 (* RP *);
  262 (* LB *);
  263 (* RB *);
  264 (* COMMA *);
  265 (* COLON *);
  266 (* COUNT *);
  267 (* ROWCOUNT *);
  268 (* COLCOUNT *);
  269 (* SUM *);
  270 (* ROWSUM *);
  271 (* COLSUM *);
  272 (* AVG *);
  273 (* ROWAVG *);
  274 (* COLAVG *);
  275 (* MIN *);
  276 (* ROWMIN *);
  277 (* COLMIN *);
  278 (* MAX *);
  279 (* ROWMAX *);
  280 (* COLMAX *);
  281 (* ADD *);
  282 (* SUBT *);
  283 (* MULT *);
  284 (* DIV *);
  285 (* EQ *);
  286 (* DELIMITER *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* FLOAT *);
  258 (* INDICE *);
  259 (* RANGE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\004\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\006\000\006\000\006\000\006\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\001\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\004\000\006\000\
\005\000\007\000\003\000"

let yydgoto = "\002\000\
\004\000\005\000\027\000\028\000"

let yysindex = "\024\000\
\024\255\000\000\254\254\000\000\028\000\246\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\255\018\255\000\255\028\255\029\255\021\255\
\000\000\003\255\004\255\005\255\006\255\007\255\000\000\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000"

let yytablesize = 37
let yytable = "\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\030\000\031\000\032\000\036\000\037\000\038\000\
\001\000\003\000\006\000\007\000\029\000\033\000\034\000\035\000\
\039\000\040\000\041\000\042\000\043\000"

let yycheck = "\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\001\001\002\001\003\001\001\001\002\001\003\001\
\001\000\002\001\029\001\000\000\003\001\030\001\003\001\003\001\
\030\001\030\001\030\001\030\001\030\001"

let yynames_const = "\
  LP\000\
  RP\000\
  LB\000\
  RB\000\
  COMMA\000\
  COLON\000\
  COUNT\000\
  ROWCOUNT\000\
  COLCOUNT\000\
  SUM\000\
  ROWSUM\000\
  COLSUM\000\
  AVG\000\
  ROWAVG\000\
  COLAVG\000\
  MIN\000\
  ROWMIN\000\
  COLMIN\000\
  MAX\000\
  ROWMAX\000\
  COLMAX\000\
  ADD\000\
  SUBT\000\
  MULT\000\
  DIV\000\
  EQ\000\
  DELIMITER\000\
  EOF\000\
  "

let yynames_block = "\
  FLOAT\000\
  INDICE\000\
  RANGE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formulas) in
    Obj.repr(
# 14 "a3.mly"
                            (_1)
# 178 "a3.ml"
               : Sheet.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'unary) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Sheet.index * Sheet.index) in
    Obj.repr(
# 18 "a3.mly"
                                              (UNARY(INDICE((fst _1),(snd _1)), _3, RANGE((fst _4),(snd _4))))
# 187 "a3.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'binary) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Sheet.index * Sheet.index) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Sheet.index * Sheet.index) in
    Obj.repr(
# 19 "a3.mly"
                                              (BINARY1(INDICE((fst _1),(snd _1)), _3, RANGE((fst _4),(snd _4)), RANGE((fst _5),(snd _5))))
# 197 "a3.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'binary) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Sheet.index * Sheet.index) in
    Obj.repr(
# 20 "a3.mly"
                                              (BINARY2(INDICE((fst _1),(snd _1)), _3, _4, RANGE((fst _5),(snd _5))))
# 207 "a3.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'binary) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Sheet.index * Sheet.index) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 21 "a3.mly"
                                              (BINARY3(INDICE((fst _1),(snd _1)), _3, RANGE((fst _4),(snd _4)), _5))
# 217 "a3.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'binary) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int * int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Sheet.index * Sheet.index) in
    Obj.repr(
# 22 "a3.mly"
                                              (BINARY4(INDICE((fst _1),(snd _1)), _3, INDICE((fst _4),(snd _4)), RANGE((fst _5),(snd _5))))
# 227 "a3.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int * int) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'binary) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Sheet.index * Sheet.index) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int * int) in
    Obj.repr(
# 23 "a3.mly"
                                              (BINARY5(INDICE((fst _1),(snd _1)), _3, RANGE((fst _4),(snd _4)), INDICE((fst _5),(snd _5))))
# 237 "a3.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "a3.mly"
                  (COUNT)
# 243 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "a3.mly"
                  (ROWCOUNT)
# 249 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "a3.mly"
                  (COLCOUNT)
# 255 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "a3.mly"
                  (SUM)
# 261 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "a3.mly"
                  (ROWSUM)
# 267 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "a3.mly"
                  (COLSUM)
# 273 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "a3.mly"
                  (AVG)
# 279 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "a3.mly"
                  (ROWAVG)
# 285 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "a3.mly"
                  (COLAVG)
# 291 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "a3.mly"
                  (MIN)
# 297 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "a3.mly"
                  (ROWMIN)
# 303 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "a3.mly"
                  (COLMIN)
# 309 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "a3.mly"
                  (MAX)
# 315 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "a3.mly"
                  (ROWMAX)
# 321 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "a3.mly"
                  (COLMAX)
# 327 "a3.ml"
               : 'unary))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "a3.mly"
                  (ADD)
# 333 "a3.ml"
               : 'binary))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "a3.mly"
                  (SUBT)
# 339 "a3.ml"
               : 'binary))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "a3.mly"
                  (MULT)
# 345 "a3.ml"
               : 'binary))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "a3.mly"
                  (DIV)
# 351 "a3.ml"
               : 'binary))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sheet.formula)

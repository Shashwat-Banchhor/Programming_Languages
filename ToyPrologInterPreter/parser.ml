type token =
  | Identifier of (string)
  | Plus
  | EOL
  | Eof
  | AtomicFormuala
  | Constant of (string)
  | Variable of (string)
  | Lis
  | L
  | And
  | Or
  | Sem
  | Com
  | Per
  | Lparen
  | Rparen
  | Lsqbrac
  | Rsqbrac
  | IF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
        open Asix
        
      
# 29 "parser.ml"
let yytransl_const = [|
  258 (* Plus *);
  259 (* EOL *);
  260 (* Eof *);
  261 (* AtomicFormuala *);
  264 (* Lis *);
  265 (* L *);
  266 (* And *);
  267 (* Or *);
  268 (* Sem *);
  269 (* Com *);
  270 (* Per *);
  271 (* Lparen *);
  272 (* Rparen *);
  273 (* Lsqbrac *);
  274 (* Rsqbrac *);
  275 (* IF *);
    0|]

let yytransl_block = [|
  257 (* Identifier *);
  262 (* Constant *);
  263 (* Variable *);
    0|]

let yylhs = "\255\255\
\002\000\001\000\004\000\004\000\005\000\005\000\007\000\007\000\
\003\000\003\000\006\000\008\000\009\000\009\000\010\000\010\000\
\000\000\000\000\000\000"

let yylen = "\002\000\
\004\000\002\000\002\000\001\000\004\000\002\000\003\000\001\000\
\003\000\003\000\004\000\001\000\003\000\001\000\001\000\001\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\012\000\017\000\000\000\000\000\
\000\000\000\000\000\000\018\000\019\000\000\000\002\000\003\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\015\000\000\000\000\000\000\000\010\000\009\000\000\000\005\000\
\011\000\000\000\001\000\007\000\013\000"

let yydgoto = "\004\000\
\006\000\012\000\013\000\007\000\008\000\009\000\023\000\010\000\
\026\000\027\000"

let yysindex = "\008\000\
\003\255\251\254\003\255\000\000\000\000\000\000\002\255\003\255\
\245\254\254\254\013\255\000\000\000\000\005\255\000\000\000\000\
\000\000\003\255\000\255\255\254\001\255\007\255\008\255\000\000\
\000\000\009\255\010\255\012\255\000\000\000\000\003\255\000\000\
\000\000\000\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\255\000\000\000\000\
\000\000\000\000\011\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\251\255\016\000\000\000\253\255\255\255\000\000\
\254\255\000\000"

let yytablesize = 32
let yytable = "\014\000\
\024\000\005\000\017\000\005\000\029\000\015\000\025\000\018\000\
\001\000\002\000\003\000\011\000\019\000\020\000\022\000\030\000\
\028\000\014\000\021\000\031\000\004\000\032\000\034\000\016\000\
\033\000\035\000\014\000\022\000\008\000\036\000\000\000\037\000"

let yycheck = "\003\000\
\001\001\001\001\014\001\001\001\004\001\004\001\007\001\019\001\
\001\000\002\000\003\000\017\001\015\001\001\001\018\000\021\000\
\018\001\021\000\014\001\013\001\004\001\014\001\013\001\008\000\
\016\001\014\001\016\001\031\000\014\001\031\000\255\255\034\000"

let yynames_const = "\
  Plus\000\
  EOL\000\
  Eof\000\
  AtomicFormuala\000\
  Lis\000\
  L\000\
  And\000\
  Or\000\
  Sem\000\
  Com\000\
  Per\000\
  Lparen\000\
  Rparen\000\
  Lsqbrac\000\
  Rsqbrac\000\
  IF\000\
  "

let yynames_block = "\
  Identifier\000\
  Constant\000\
  Variable\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 52 "parser.mly"
                                                ( _2 ^ ".pl")
# 139 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_clauses) in
    Obj.repr(
# 55 "parser.mly"
                                       ( List (_1) )
# 146 "parser.ml"
               : Asix.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_clauses) in
    Obj.repr(
# 60 "parser.mly"
                                        ( [_1] @ _2 )
# 154 "parser.ml"
               : 'list_of_clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 61 "parser.mly"
                             ( [_1])
# 161 "parser.ml"
               : 'list_of_clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atomic_for) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'bod) in
    Obj.repr(
# 65 "parser.mly"
                                      ( (_1,And(L(_3))) )
# 169 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_for) in
    Obj.repr(
# 66 "parser.mly"
                               ( (_1,And(L([]))) )
# 176 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_for) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bod) in
    Obj.repr(
# 74 "parser.mly"
                                      ( [_1] @ _3   )
# 184 "parser.ml"
               : 'bod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_for) in
    Obj.repr(
# 75 "parser.mly"
                                       ( [_1] )
# 191 "parser.ml"
               : 'bod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_for) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Asix.atomic_formula list ) in
    Obj.repr(
# 79 "parser.mly"
                                             ( [_1] @ _3)
# 199 "parser.ml"
               :  Asix.atomic_formula list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_for) in
    Obj.repr(
# 80 "parser.mly"
                                         ( [_1] )
# 206 "parser.ml"
               :  Asix.atomic_formula list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'prdct) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 84 "parser.mly"
                                                                    (AtomicFormula(_1,_3) )
# 214 "parser.ml"
               : 'atomic_for))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                                      (Pred(_1))
# 221 "parser.ml"
               : 'prdct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 92 "parser.mly"
                                       ([_1] @ _3)
# 229 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 93 "parser.mly"
                                       ( [_1] )
# 236 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                                  (Var(_1) )
# 243 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
                                  (V(_1) )
# 250 "parser.ml"
               : 'term))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry input_file_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry quest *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Asix.program)
let input_file_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : string)
let quest (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf :  Asix.atomic_formula list )

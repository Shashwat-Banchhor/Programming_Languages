type token =
  | Identifier of (string)
  | Plus
  | EOL
  | Eof
  | AtomicFormuala
  | Constant of (string)
  | Variable of (string)
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
# 2 "print_parser.mly"
        open Asix
        
      
# 28 "print_parser.ml"
let yytransl_const = [|
  258 (* Plus *);
  259 (* EOL *);
  260 (* Eof *);
  261 (* AtomicFormuala *);
  264 (* L *);
  265 (* And *);
  266 (* Or *);
  267 (* Sem *);
  268 (* Com *);
  269 (* Per *);
  270 (* Lparen *);
  271 (* Rparen *);
  272 (* Lsqbrac *);
  273 (* Rsqbrac *);
  274 (* IF *);
    0|]

let yytransl_block = [|
  257 (* Identifier *);
  262 (* Constant *);
  263 (* Variable *);
    0|]

let yylhs = "\255\255\
\002\000\001\000\003\000\003\000\004\000\004\000\006\000\006\000\
\005\000\007\000\008\000\008\000\009\000\009\000\000\000\000\000"

let yylen = "\002\000\
\004\000\002\000\002\000\001\000\004\000\002\000\003\000\001\000\
\004\000\001\000\003\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\010\000\015\000\000\000\000\000\000\000\
\000\000\000\000\016\000\002\000\003\000\006\000\000\000\000\000\
\000\000\000\000\000\000\014\000\013\000\000\000\000\000\000\000\
\000\000\005\000\009\000\000\000\001\000\007\000\011\000"

let yydgoto = "\003\000\
\005\000\011\000\006\000\007\000\008\000\019\000\009\000\022\000\
\023\000"

let yysindex = "\003\000\
\001\255\249\254\000\000\000\000\000\000\002\255\001\255\246\254\
\253\254\011\255\000\000\000\000\000\000\000\000\001\255\000\255\
\252\254\003\255\004\255\000\000\000\000\255\254\006\255\007\255\
\001\255\000\000\000\000\000\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\255\000\000\000\000\000\000\000\000\009\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\012\000\000\000\241\255\253\255\000\000\251\255\
\000\000"

let yytablesize = 24
let yytable = "\018\000\
\020\000\004\000\014\000\001\000\002\000\012\000\021\000\015\000\
\010\000\018\000\016\000\017\000\024\000\027\000\025\000\004\000\
\026\000\028\000\013\000\029\000\008\000\030\000\031\000\012\000"

let yycheck = "\015\000\
\001\001\001\001\013\001\001\000\002\000\004\001\007\001\018\001\
\016\001\025\000\014\001\001\001\017\001\015\001\012\001\004\001\
\013\001\012\001\007\000\013\001\013\001\025\000\028\000\015\001"

let yynames_const = "\
  Plus\000\
  EOL\000\
  Eof\000\
  AtomicFormuala\000\
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
# 49 "print_parser.mly"
                                                ( _2 ^ ".pl")
# 129 "print_parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_clauses) in
    Obj.repr(
# 52 "print_parser.mly"
                                       ( "prog("^ _1 ^ ")" )
# 136 "print_parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_clauses) in
    Obj.repr(
# 57 "print_parser.mly"
                                        (  " Clause "^_1^ _2 )
# 144 "print_parser.ml"
               : 'list_of_clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 58 "print_parser.mly"
                             ( " Clause "^ _1)
# 151 "print_parser.ml"
               : 'list_of_clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atomic_for) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'bod) in
    Obj.repr(
# 62 "print_parser.mly"
                                      ( "atomic_for("^_1^"),bod("^_3^")" )
# 159 "print_parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_for) in
    Obj.repr(
# 63 "print_parser.mly"
                               ( "atomic_for("^_1^"),bod()" )
# 166 "print_parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_for) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bod) in
    Obj.repr(
# 71 "print_parser.mly"
                                      ( "atomic_for("^_1^") ,"^ _3   )
# 174 "print_parser.ml"
               : 'bod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_for) in
    Obj.repr(
# 72 "print_parser.mly"
                                       ( "atomic_for("^_1^") " )
# 181 "print_parser.ml"
               : 'bod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'prdct) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 78 "print_parser.mly"
                                                                    ( "prdct("^_1^"),term_list["^_3^"]" )
# 189 "print_parser.ml"
               : 'atomic_for))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "print_parser.mly"
                                      ( "Identifier("^_1^")" )
# 196 "print_parser.ml"
               : 'prdct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 86 "print_parser.mly"
                                       ("term("^_1^"),"^ _3)
# 204 "print_parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 87 "print_parser.mly"
                                       ( "term("^_1^")")
# 211 "print_parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "print_parser.mly"
                                  ("Variable("^_1^")" )
# 218 "print_parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "print_parser.mly"
                                  ("Identifier("^_1^")" )
# 225 "print_parser.ml"
               : 'term))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry input_file_parser *)
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : string)
let input_file_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : string)

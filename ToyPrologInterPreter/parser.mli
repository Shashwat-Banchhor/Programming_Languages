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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Asix.program
val input_file_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val quest :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Asix.atomic_formula list 

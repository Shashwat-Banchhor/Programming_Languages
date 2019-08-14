   (* File lexer.mll *)
        

        {
               (* The type token is defined in parser.mli *)
         (*type token = 
          | Integer of int
          | Plus
          |EOL*)

        open Parser
        open Asix
        exception Eof
        exception Error
        }

        let lalpha  = ['a'-'z'  ]
        let ualpha =['A'-'Z']
        let alpha = ['a'-'z' 'A'-'Z' '_' ]
        let digits = ['0'-'9']
        let identifier  = lalpha(alpha | digits)*
        let variable = ualpha(alpha)*

        rule token = 
        parse
          |  [' ' '\t' '\n']    { token lexbuf }
          | identifier as lxm  { Identifier(lxm ) }
          |   '+'           { Plus }
          |   ":-"          {IF}
          | variable as lxm   {  Variable(lxm)   }
          | '('                {Lparen}
          | ')'                {Rparen}
          | '['                {Lsqbrac}
          | ']'                {Rsqbrac}
          | ';'                 {Sem}
          | ','                 {Com}
          | '.'                 {Per}
          | '|'                {Lis}
          | eof               {Eof}
          | _                 { raise Error}

        {
            (*let main () = begin 
              try
            let lexbuf = Lexing.from_channel stdin in
            while true do
              let result = token lexbuf in
                Printf.printf " I saw a tooken\n";
            done
            with Eof ->exit 0
          end ;;
          main();;


*)
        }
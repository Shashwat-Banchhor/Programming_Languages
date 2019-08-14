  (* File calc.ml *)
        open Asix
        open Parser
        open Main

        let _ =
          try
            let lexbuf =Lexing.from_channel stdin in 
            
         
              let p = Print_parser.prog Print_main.token lexbuf in
                Printf.printf "%s"(p);
               
             
          
          with Main.Eof ->
            exit 0


  (* File calc.ml *)
        open Asix
        open Print_parser
        open Print_main

        let _ =
          try
            let lexbuf =Lexing.from_channel stdin in 
           
            let file = Print_parser.input_file_parser Print_main.token lexbuf in

            let fileHandle = open_in file in

            let lexbuf = Lexing.from_channel fileHandle in
            
              let p = Print_parser.prog Print_main.token lexbuf in
             
              Printf.printf "\n";
              Printf.printf " \n PARSE TREE \n\n";
              flush stdout;
              Printf.printf "%s"(p);
              flush stdout;
              Printf.printf "\n\n";
               
             
          
          with Print_main.Eof ->
            exit 0


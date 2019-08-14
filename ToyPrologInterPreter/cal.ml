  (* File calc.ml *)
        open Asix
        open Parser
        open Main

        let _ =
          try
            let lexbuf =Lexing.from_channel stdin in 
             (* let file = Parser.quest Main.token lexbuf  in 
            (*  print_atomic_formula (file) ;  *)
            let t = true in  
            while t do 
              let result = token lexbuf in 
              (* if result = EOL then ( *)match result with 
                |Identifier(s) -> Printf.printf "%s"(s)
                |Lparen -> Printf.printf "("
                |Rparen -> Printf.printf ")"
                |Per    -> Printf.printf "."
                |Com   -> Printf.printf ","
                |EOL   -> Printf.printf "line_end" 
                |Eof   -> exit 0;
               (*  |_     -> Printf.printf "***" *)

           (*  done ; *)
            Printf.printf "***" ;
          let file = Parser.quest Main.token lexbuf in
            print_atomic_formula (file) ; 
            exit 0;
          done; *)
            let file = Parser.input_file_parser Main.token lexbuf in
            Printf.printf "\n";
            let fileHandle = open_in file in
            (* Printf.printf "Yup0"; *)
            let lexbuf = Lexing.from_channel fileHandle in
            	(* Printf.printf "Yup1";
 *)              let p = Parser.prog Main.token lexbuf in
             (*    Printf.printf "Yup2"; *)
                let program  = p in
                 (* print_string "| ?- "; *)
                (* Printf.printf "Yup3";
               print_newline();
              
              Printf.printf "Yup";
            let lexbuf = Lexing.from_channel stdin in
             let result = Parser.quest Main.token lexbuf  in 
            print_atomic_formula (result) ;
            p_evaluate3(result,program);
            Printf.printf "\n\nWhooppe"; *)
            while true do
              
           print_string "| ?- "; 
           flush stdout ;
            let lexbuf = Lexing.from_channel stdin in
             let result = Parser.quest Main.token lexbuf  in 
          (*   print_atomic_formula (result) ; *)
           

            p_evaluate3(result,program);
 
                
  
                print_newline();
                flush stdout ;
                (* if (result = 0) then exit 0 *)
            done
          with Main.Eof ->
            exit 0


      %{
        open Asix
        
      %}

        %token <string> Identifier
        %token  Plus
        %token EOL
     	%token Eof
        %token  AtomicFormuala
        
        %token <string> Constant 
        %token <string> Variable 


        %token L
        %token And
        %token Or
        %token Sem
        %token Com
        %token Per
        %token Lparen
        %token Rparen
        %token Lsqbrac
        %token Rsqbrac
        %token IF



        %right Com

        %nonassoc Per


        %start prog
       

        %type<string> prog
       
       /* %type<Asix.body> list_of_clauses*/
        
        




        %%
        input_file_parser:
            Lsqbrac Identifier Rsqbrac  Per     { $2 ^ ".pl"} 

        prog:
              |  list_of_clauses Eof   { "prog("^ $1 ^ ")" } 
        ;

        list_of_clauses:
          
             |clause list_of_clauses 			{  " Clause "^$1^ $2 }
             | clause 							{ " Clause "^ $1}
        ;  

        clause:
        	| atomic_for IF bod Per  			 { "atomic_for("^$1^"),bod("^$3^")" }
        	| atomic_for Per 					{ "atomic_for("^$1^"),bod()" }


        ;

      

        bod:
          | atomic_for Com bod        { "atomic_for("^$1^") ,"^ $3   }
          | atomic_for                 { "atomic_for("^$1^") " } 
        ;

       

        atomic_for:
              | prdct Lparen term_list Rparen                       { "prdct("^$1^"),term_list["^$3^"]" } 
        ;

        prdct:
            | Identifier              { "Identifier("^$1^")" }
        ;
        
        term_list:
            | term Com term_list       {"term("^$1^"),"^ $3}
            | term                     { "term("^$1^")"}
        ;    





        

        term:
            | Variable            {"Variable("^$1^")" }
            | Identifier          {"Identifier(" ^ $1 ^ ")" }
        ;
       
         
        
           
         

     
         
        
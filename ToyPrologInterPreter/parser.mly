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

         %token Lis
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
        %start input_file_parser
        %start quest
       

        %type<Asix.program> prog
        %type<string> input_file_parser
        %type< Asix.atomic_formula list > quest
       /* %type<Asix.body> list_of_clauses*/
        
        




        %%
        input_file_parser:
            Lsqbrac Identifier Rsqbrac  Per     { $2 ^ ".pl"} 

        prog:
              |  list_of_clauses Eof   { List ($1) } 
        ;

        list_of_clauses:
          
             |clause list_of_clauses 			{ [$1] @ $2 }
             | clause 							{ [$1]}
        ;  

        clause:
        	| atomic_for IF bod Per  			 { ($1,And(L($3))) }
        	| atomic_for Per 					{ ($1,And(L([]))) }


        ;

      

        bod:
          | atomic_for Com bod        { [$1] @ $3   }
          | atomic_for                 { [$1] } 
        ;

        quest:
            | atomic_for Per  quest          { [$1] @ $3}
            | atomic_for Per Eof         { [$1] }
        ;

        atomic_for:
              | prdct Lparen term_list Rparen                       {AtomicFormula($1,$3) } 
        ;

        prdct:
            | Identifier              {Pred($1)}
        ;
        
        term_list:
            | term Com term_list       {[$1] @ $3}
            | term                     { [$1] }
        ;    

        term:
            | Variable            {Var($1) }
            | Identifier          {V($1) }
        ;
       
         
        
           
         

     
         
        
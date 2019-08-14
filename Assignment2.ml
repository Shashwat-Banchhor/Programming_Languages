 type exp = Const of int |        
  		Abs of exp |
  		Iden of string|	
  		Plus of exp * exp|
  		Sub of exp*exp|
  		Mult of exp*exp|
  		Div of exp * exp|
  		Mod of exp * exp|
  		Expo of exp * exp |
  		T|F|
  		Not of exp|
  		And of exp*exp|
		Or of exp*exp|
		Imply of exp *exp|
  		Grt of exp*exp|
  		Grte of exp*exp|	
  		Let of exp *exp|
  		Lete of exp*exp|
  		Equ of exp * exp |
  		Tup of exp list | (*n-tuple is represented as n element list*)
  		Proj of int*exp ;;

type answer = Res of int|Tr|Fu| Emp of unit|Rest of answer list |Resto of answer list list;;

exception Typ_exn;;



(*Functions for Operations in type Answer*)
		let plus (a,b) = match a,b with 
			Res w , Res e -> Res (e+w)|
			_ -> raise Typ_exn;;
		let sub (a,b) = match a,b with 
			Res w , Res e -> Res (w-e)|
			_ -> raise Typ_exn;;
		let mult (a,b) = match a,b with 
			Res w , Res e -> Res (e*w)|
			_ -> raise Typ_exn;;
		let div (a,b) = match a,b with 
			Res w , Res e -> Res (w/e)|
			_ -> raise Typ_exn;;

		let rec pow (a,b) e = match a,b with 
			p,0 -> e|
			p,1 -> p|
			p,n -> pow (p,n-1) e*p ;;

		let rec modu (a,b)  = if b>a then a else modu (a-b,b);; 
		let modulus (a,b) = match a,b with 
			Res w , Res e -> Res (modu(w,e)) |
			_ -> raise Typ_exn;;
		let power (a,b) = match a,b with 
			Res w , Res e -> Res ( pow(w,e) 1)|
			_ -> raise Typ_exn;;

		let rec map f  l = match l with 
		        [] -> []|  
		  	x::xs -> (f x)::(map f xs);;

		let rec mapx f t l = match l with 
		        [] -> []|  
		  	x::xs -> (f ([],t,x))::(mapx f t xs);;



		let inv p = match p with 
			Tr -> Fu|
			Fu -> Tr |
			_ -> raise Typ_exn;;

		let conj (p,q) = match p,q with 
			Tr,Tr -> Tr|
			_ -> Fu;;

		let disj (p,q) = match p,q with 
			Fu,Fu -> Fu|
			_ -> Tr;;

		let imp (e1,e2) = match (e1,e2) with 
			Fu,p -> Tr|
			Tr,Tr-> Tr|
			_ -> Fu;;
		let rec absolute q = match q with 
			Res p -> if p>=0 then (Res p)  else (Res (-p))|
			_ -> raise Typ_exn;;
		let is_grt (a,b)=match (a,b) with 
			Res n1 , Res n2 ->  if n1>n2 then Tr else Fu|
			_ -> raise Typ_exn;;
		let is_grte (a,b)=match (a,b) with 
			Res n1 , Res n2 ->  if n1>=n2 then Tr else Fu|
			_ -> raise Typ_exn;;
		let is_lt (a,b)=match (a,b) with 
			Res n1 , Res n2 ->  if n1<n2 then Tr else Fu|
			_ -> raise Typ_exn;;
		let is_lte (a,b)=match (a,b) with 
			Res n1 , Res n2 ->  if n1<=n2 then Tr else Fu|
			_ -> raise Typ_exn;;
		let is_eq (a,b)=match (a,b) with 
			Res n1 , Res n2 ->  if n1=n2 then Tr else Fu|
			_ -> raise Typ_exn;;

		let rec give_n (l,n) = match l,n with 
			[],p -> raise Typ_exn|
			x::xs,0 -> x |
			x::xs , n -> give_n (xs,n-1);; 


(*Evaluate eval -> (string ->answer)->exp->answer *)

let rec eval rho e = match e with
	Const n -> Res n| 
  	T->Tr|
  	F-> Fu |
	Iden x -> rho x|
	Plus (e1,e2) -> plus ( eval rho e1 , eval rho e2)|
	Sub (e1,e2) -> sub ( eval rho e1 , eval rho e2)|
	Mult (e1,e2) -> mult ( eval rho e1 , eval rho e2)|
	Div (e1,e2) -> div ( eval rho e1 , eval rho e2)|
	Mod (e1,e2) -> modulus ( eval rho e1 , eval rho e2)|
	Expo (e1,e2) -> power ( eval rho e1 , eval rho e2)|
	Not e1 ->  inv (eval rho e1) |
	Or (e1,e2) -> disj(eval rho e1 , eval rho e2)  |
	And (e1,e2) ->  conj(eval rho e1 , eval rho e2)  |
	Imply (e1, e2) -> 	imp(eval rho e1 , eval rho e2)|
  	Abs n -> absolute (eval rho n) |
	Grt (e1,e2) ->  is_grt(eval rho e1,eval rho e2)|
	Grte (e1,e2) ->  is_grte(eval rho e1,eval rho e2)|
	Let (e1,e2) ->  is_lt(eval rho e1,eval rho e2)|
	Lete (e1,e2) ->  is_lte(eval rho e1,eval rho e2)|
	Equ (e1,e2) ->  is_eq(eval rho e1,eval rho e2) |
	Tup (t)  ->  Rest (map (eval rho) t )|
	Proj (n,ex) -> match n,ex with
		0,Tup(q::tup) -> eval rho q |
		n,Tup(q::tup) -> eval rho (Proj (n-1,Tup tup))|
		_ -> raise Typ_exn;;


(*rho: string -> answer    Defines all alphanumeric strings*)
let rho1 x = match x with 
	 "x" ->(Res 24)|
	 "1y"-> (Res 25)|
	 "z2"-> (Res 2)|
	 "2a"-> (Res 3)|
	 "1343b"-> (Res 4)|
	_ -> (Tr);;


(*Examples Eval*)
rho1 ( "x");;
let x =eval rho1 (Mult(Plus(Iden "x",Const 5),Iden "1y"));; (*Res 725*)
let x =eval rho1 (Tup [Mult(Plus(Iden "x",Const 5),Iden "1y");Const 2]);;(*[Res 725;Res 2*)
let x =eval rho1 (Proj (1,Tup [Mult(Plus(Iden "x",Const 5),Iden "1y");Const 2]));;(*Res 2*)
let x =eval rho1 (Imply(T,Grte(Plus(Iden "x",Const 5),Iden "1y")));;(*Tr*)
let x =eval rho1 (Mod(Const 5,Const 4));;(*Res 1*)
let x =eval rho1 (Sub(Const 5,Const 4));;(*Res 1*)
eval rho1 (Div(Const 5,Const 4));;(*Res 1*)
eval rho1 (Expo(Const 5,Const 4));;(*Res 625*)
eval rho1 (Grte(Const 4,Const 4));;(*Tr*)
eval rho1 (Grt(Const 5,Const 4));;(*Tr*)
eval rho1 (Let(Const 5,Const 4));;(*Fu*)
eval rho1 (Lete(Const 3,Const 3));;(*Tr*)
eval rho1 (Equ(Const 4,Const 4));;(*Tr*)
eval rho1 (And(T,T));;(*Tr*)
eval rho1 (Or(F,F));;(*Fu*)
eval rho1 (Abs (Const (-7)));;(*Res 7*)
eval rho1 (Not(F));;(*Tr*)

(*let x =eval rho1 (Mod(Const 5,T));;  Typ_exn*)
(**)



 type opcode = CONST of int | 
 		LOOKFOR of string|       
  		ABS  |
  		IDEN|	
  		PLUS |
  		SUB |
  		MULT |
  		DIV |
  		MOD  | 		
  		EXPO |
  		TRUE|FALSE|
  		NOT|
  		CONJ|
		DISJ|
		IMPLY |
  		GRT |
  		GRTE |	
  		LET|
  		LETE |
  		EQU  |
  		TUP |
  		PROJ |
  		LIST of opcode list list;;

(*let rec compile_tup  e t = match t with
	[x] -> compile(x)@[CONST e]| 
	x::xs ->compile(x)@[CONST e]@co*)
let rec length l = match  l with
 [] -> 0| x::xs -> 1+(length xs);;


 (*compile:  exp ->opcode list*)

let rec compile e  = match e with
	Const n -> [CONST n]| 
  	T->[TRUE]|
  	F-> [FALSE] |
	Iden x -> [LOOKFOR x]|
	Plus (e1,e2) -> (compile e1) @compile e2@[PLUS]|
	Sub (e1,e2) -> (compile e1)@(compile e2)@[SUB]|
	Mult (e1,e2) -> (compile e1)@(compile e2)@[MULT]|
	Div (e1,e2) -> (compile e1)@(compile e2)@[DIV] |
	Mod (e1,e2) ->(compile e1)@(compile e2)@[MOD] |
	Expo (e1,e2) -> (compile e1) @ (compile e2) @ [EXPO] |
	Not e1 ->  (compile e1)@[NOT] |
	Or (e1,e2) ->   (compile e1) @ (compile e2) @ [DISJ]|
	And (e1,e2) ->   (compile e1) @ (compile e2) @ [CONJ]|
	Imply (e1, e2) -> 	(compile e1) @ (compile e2) @ [IMPLY]|
  	Abs e1 -> (compile e1) @  [ABS] |
	Grt (e1,e2) ->  (compile e1) @ (compile e2) @ [GRT]|
	Grte (e1,e2) -> (compile e1)@(compile e2) @ [GRTE]|
	Let (e1,e2) -> (compile e1)@(compile e2) @ [LET]|
	Lete (e1,e2) ->  (compile e1)@(compile e2) @ [LETE]|
	Equ (e1,e2) ->  (compile e1)@(compile e2) @ [EQU] |
	Tup (x)     ->   (match x with

						[] -> raise Typ_exn|
						[x] ->   (*[TUP;TUP]@compile(x)@[CONST 0]@[TUP]*)
									 [TUP]@compile(x)@[CONST 0]@[TUP;TUP]|


						x::xs -> (*compile(Tup (xs))@compile(x)@[CONST (length xs)]@[TUP]*)
									[TUP]@compile(x)@[CONST (length xs)]@compile(Tup (xs))



									)|              
	Proj (n,k) ->  (compile k)@[CONST n]@[PROJ];;
	


(*Examples Compile*)
(*
rho1 ( "x");;
let x =compile (Mult(Plus(Iden "x",Const 5),Iden "1y"));; (*[LOOKFOR "x";CONST 5;PLUS;LOOKFOR "1y";MULT]*)
let x =compile (Tup [Mult(Plus(Iden "x",Const 5),Iden "1y");Const 2]);;(*[LIST [[CONST 2];[LOOKFOR "x";CONST 5;PLUS;LOOKFOR "1y";MULT]];TUP]*)
let x =compile (Proj (1,Tup [Mult(Plus(Iden "x",Const 5),Iden "1y");Const 2]));;(*[[[LOOKFOR "x";CONST 5;PLUS;LOOKFOR "1y";MULT];[CONST 2]];TUP]*)
let x =compile (Imply(T,Grte(Plus(Iden "x",Const 5),Iden "1y")));;(*Tr*)
let x =compile (Mod(Const 5,Const 4));;(*Res 1*)
(*let x =eval rho1 (Mod(Const 5,T));;  Typ_exn*)
(**)
*)


(*execute : (answer list)*(string -> answer)*opcode list -> answer*)

let rec execute (stack,table,opL) = match (stack,table,opL) with
    (x::s,table,[]) -> x |
	(s,table,CONST n::c) ->execute(Res n ::s,table,c)| 
	(s,table,TRUE::c) ->execute(Tr ::s,table,c)|
	(s,table,FALSE::c) ->execute(Fu ::s,table,c)|
	(s,table,(LOOKFOR x)::c) ->execute(table x ::s,table,c)|
	(Res n1 :: Res n2 ::s,table,PLUS :: c) -> execute((Res (n1+n2)) ::s,table,c)|
	(Res n1 :: Res n2 ::s,table,SUB :: c) -> execute((Res (n2-n1)) ::s,table,c)|
	(Res n1 :: Res n2 ::s,table,MULT :: c) -> execute((Res (n1*n2)) ::s,table,c)|
	(Res n1 :: Res n2 ::s,table,MOD :: c) -> execute(modulus(Res n1 , Res n2) ::s,table,c)|
	(Res n1 :: Res n2 ::s,table,DIV :: c) -> execute((Res (n2/n1)) ::s,table,c)|
	(Res n1 :: Res n2 ::s,table,EXPO :: c) -> execute(power(Res n2 , Res n1) ::s,table,c)|
	(Tr::s,table,NOT :: c) -> execute(Fu ::s,table,c)|
	(Fu::s,table,NOT :: c) -> execute(Tr ::s,table,c)|
	(Tr :: Fu::s,table,CONJ :: c) -> execute(Fu ::s,table,c)|
	(Fu :: Tr::s,table,CONJ :: c) -> execute(Fu ::s,table,c)|
	(Fu :: Fu::s,table,CONJ :: c) -> execute(Fu ::s,table,c)|
	(Tr :: Tr::s,table,CONJ :: c) -> execute(Tr ::s,table,c)|
	(Tr :: Fu::s,table,DISJ :: c) -> execute(Tr ::s,table,c)|
	(Fu :: Tr::s,table,DISJ :: c) -> execute(Tr ::s,table,c)|
	(Fu :: Fu::s,table,DISJ :: c) -> execute(Fu ::s,table,c)|
	(Tr :: Tr::s,table,DISJ :: c) -> execute(Tr ::s,table,c)|
	(Tr :: Fu::s,table,IMPLY :: c) -> execute(Fu ::s,table,c)|
	(Fu :: Tr::s,table,IMPLY :: c) -> execute(Tr ::s,table,c)|
	(Fu :: Fu::s,table,IMPLY :: c) -> execute(Tr ::s,table,c)|
	(Tr :: Tr::s,table,IMPLY :: c) -> execute(Tr ::s,table,c)|
	(Res n1 ::s,table,ABS :: c) -> execute(absolute(Res n1) ::s,table,c)|
	(Res n1 ::Res n2::s,table,GRT :: c) -> execute(is_grt(Res n2 , Res n1) ::s,table,c)|
	(Res n1 ::Res n2::s,table,GRTE :: c) -> execute(is_grte(Res n2 , Res n1) ::s,table,c)|
	(Res n1 ::Res n2::s,table,LET :: c) -> execute(is_lt(Res n2 , Res n1) ::s,table,c)|
	(Res n1 ::Res n2::s,table,LETE :: c) -> execute(is_lte(Res n2 , Res n1) ::s,table,c)|
	(Res n1 ::Res n2::s,table,EQU:: c) -> execute(is_eq(Res n1 , Res n2) ::s,table,c)|

	(Res 0::m::Rest l::s,table, TUP::TUP::c) -> execute( Rest (l@[m]) ::s,table,c)|
	(Res n::k::(Rest l) ::s,table, TUP::c) -> execute(Rest (l@[k]) ::s,table,c)|
	(s,table,TUP::c) -> execute(Rest ([]) ::s,table,c)|
	
	(Res n::Rest l::s,table,PROJ:: c) -> execute(give_n (l,n) ::s,table,c)|
	_ -> raise Typ_exn;;





(* The HashTable and table    Table only has finite Domain  *)
let lookup = Hashtbl.create 100000;;
Hashtbl.add lookup "x" (Res 24);;
Hashtbl.add lookup "1y" (Res 25);
Hashtbl.add lookup "z2" (Res 2);
Hashtbl.add lookup "2a" (Res 3);
Hashtbl.add lookup "1343b" (Res 4);;


let table x = Hashtbl.find lookup x;; 

type answer = Res of int|Tr|Fu| Emp of unit|Rest of answer list |Resto of answer list list;;


(* Note:
 * Please make suitable changes to the shared test cases so that
 * the constructors match your signature definition.
 *)

(*--==Compile & Execute==--*)



let print_ans ans = match ans with
	Res n  -> Printf.printf"%d\n"(n)|
	Tr     -> Printf.printf"%s\n"("True")|
	Fu     -> Printf.printf"%s\n"("False");;

(* print_ans(execute ([],table ,(compile (Plus(Const(1),Const(2)))))); *)
let t1 = Mult(Const(6),Const(6));;
let t2 = Expo(Const(2),Const(4));;
let t3 = Div(Const(6),Const(3));;
let t4 = Iden "iden1";;
let t5 = Iden("iden2");;

let t6= Abs(Const(-1));;
let t7 = Proj(2,Tup([Const(12);Const(121);Const(33)]));;

let t8 = Sub(Proj(2,Tup[Const(2);Const(5);Const(8)]),Const(1));;
let t9 = Mod(Const(2),Proj(2,Tup[Const(2);Const(5);Const(8)]));;
let t10 = Or(Equ(Const(5),Const(5)),And(Equ(Sub(Const(2),Const(1)),Const(1)),Mod(Const(2),Proj(2,Tup[Const(2);Const(5);Const(8)]))));;
let t11 = And(T,F);;
let t12 = Imply(Not(Imply(Or(T,F), And(T, F))),Imply(And(T, F), Or(T,F)));;
let t13  = Grte(Const(4),Const(2));;
let t14 = Lete(Const(4),Const(2));;

Printf.printf"t1 :  ";;
compile t1;;
execute ([],table,compile t1);;
Printf.printf"\n";;

Printf.printf"t1 :  ";;
compile t2;;
execute ([],table,compile t2);;
Printf.printf"\n";;

Printf.printf"t1 :  ";;
compile t3;;
execute ([],table,compile t3);;
Printf.printf"\n";;

Printf.printf"t1 :  ";;
compile t4;;
execute ([],table,compile t4);;
Printf.printf"\n";;
Printf.printf"t1 :  ";;
compile t5;;
execute ([],table,compile t5);;
Printf.printf"\n";;
Printf.printf"t1 :  ";;
compile t6;;
execute ([],table,compile t6);;
Printf.printf"\n";;
Printf.printf"t1 :  ";;
compile t7;;
execute ([],table,compile t7);;
Printf.printf"\n";;
Printf.printf"t1 :  ";;
compile t8;;
execute ([],table,compile t8);;
Printf.printf"\n";;
Printf.printf"t1 :  ";;
compile t9;;
execute ([],table,compile t9);;
Printf.printf"\n";;

compile t10;;
execute ([],table,compile t10);;
compile t11;;
execute ([],table,compile t11);;
compile t12;;
execute ([],table,compile t12);;
compile t13;;
execute ([],table,compile t13);;
compile t14;;
execute ([],table,compile t14);;
(*Or(
	Eql(Const(5),Const(5)),
	And(Eql(Sub(Const(2),Const(1)),Const(1)),
		Mod(Proj(Tup[Const(2),Const(5),Const(8)],2),Const(2))
	)
);

And(Bool(T), Bool(F));
Implies(NOT(Implies(Or(Bool(T), Bool(F)), And(Bool(T), Bool(F)))),Implies(And(Bool(T), Bool(F)), Or(Bool(T), Bool(F))));

Grte(Const(4),Const(2));
Lete(Const(4),Const(2));*)


(*Execute Examples
execute ([],table ,(compile (Tup [Iden "x";Const 2] )));;
let x = execute ([],table ,(compile (Proj(0,(Tup [Iden "x";Const 2] )))));;
let x = execute ([],table ,(compile (Const 2 )));;(*Res 2*)
let x = execute ([],table ,(compile (Iden "x" )));;(* Res 24*)
let x = execute ([],table ,(compile (T)));;(*Tr*)
let x = execute ([],table ,(compile (Tup [Iden "x";Const 2] )));;(*Rest [Res 24;Res 2]*)

let x = execute ([Res 45],table ,(compile (Tup [Iden "x";Const 2] )));;
let x =execute([],table,compile( (Mult(Plus(Iden "x",Const 5),Iden "1y"))));; (*Res 725*)
let x =execute([],table,compile( (Tup [Mult(Plus(Iden "x",Const 5),Iden "1y");Const 2])));;(*[Res 725;Res 2*)
let x =execute([],table,compile( (Proj (1,Tup [Mult(Plus(Iden "x",Const 5),Iden "1y");Const 2]))));;(*Res 2*)
let x =execute([],table,compile( (Imply(T,Grte(Plus(Iden "x",Const 5),Iden "1y")))));;(*Tr*)
let x =execute([],table,compile( (Mod(Const 5,Const 4))));;(*Res 1*)
let x =execute([],table,compile( (Sub(Const 5,Const 4))));;(*Res 1*)
execute([],table,compile( (Div(Const 5,Const 4))));;(*Res 1*)
execute([],table,compile( (Expo(Const 5,Const 4))));;(*Res 625*)
execute([],table,compile( (Grte(Const 4,Const 4))));;(*Tr*)
execute([],table,compile( (Grt(Const 5,Const 4))));;(*Tr*)
execute([],table,compile( (Let(Const 5,Const 4))));;(*Fu*)
execute([],table,compile( (Lete(Const 3,Const 3))));;(*Tr*)
execute([],table,compile( (Equ(Const 4,Const 4))));;(*Tr*)
execute([],table,compile( (And(T,T))));;(*Tr*)
execute([],table,compile( (Or(F,F))));;(*Fu*)
execute([],table,compile( (Not(F))));;(*Fu*) (*Doesn't depend on the initial state of the stack*)*)





















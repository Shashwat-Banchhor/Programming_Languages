type exp = Var of string |
		   Lambda of exp*exp |
		   Pair of exp*exp |
		   Def of exp*exp|
		   Const of int |
		   Bool of bool|
		   Plus of exp * exp|
  		   Sub of exp*exp|
  		   Mult of exp*exp|
		   ITE of exp*exp*exp|
		   Grt of exp*exp|
  		   Grte of exp*exp|	
  		   Let of exp *exp|
  		   Lete of exp*exp|
  		   Equ of exp * exp |
  		   Tup of exp list|
  		   Ldef of exp * exp|
  		   Sdef of exp * exp|
  		   Pdef of exp * exp|
  		   Letdef of exp * exp;;


(* type closure = Pear of (ans *exp);; *)



type opcode = CONST of int|
			TRUE|
			FALSE|
			LOOKUP of string|
			BIND of string|
			UNBIND of exp list|
			RET|
			PLUS |
  		    SUB |
  		    MULT |
			GRT |
  			GRTE |	
  			LET|
  			LETE |
  			EQU  |
			CLOS of  exp * opcode list|
			APP|
			COND of opcode list * opcode list|
			DEF |
			SDEF|
			PDEF|
			LDEF|
			Letdefine|
			LOCAL|
			TUP of opcode list list;;


type answer = Res of int | Tr|Fu|Unit|Vclos of ((exp*answer) list)*string*opcode list | Rest of answer list;; 

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



exception Unbound_Variable_GIVEN;;
exception Emp_Table;;

let rec map f l = match l with
	[] -> []|
	x::xs -> (f x) :: (map f xs);;

let rec find_in_table (x, table) = match x,table with 
		(x,[]) -> raise Unbound_Variable_GIVEN|
		(x,(Var y,ans)::t) -> if (x=y) then ans else find_in_table(x,t)|
		_ -> raise Unbound_Variable_GIVEN;;

let rec are_Parallel(l1,l2) = match l1, l2 with
		[],l1 -> true|
		l1,[] -> true|
		x::xs,l -> if (List.mem x l) then false else are_Parallel(xs,l);;


let rec find_def_var (d1) = match  d1 with 
			Def(x,e1) -> [x]|
			Sdef(d1,d2) -> find_def_var(d1)@find_def_var(d2)|
			Pdef(d1,d2) -> find_def_var(d1)@find_def_var(d2)|
			Ldef(d1,d2) -> find_def_var(d1)@find_def_var(d2)|
			_ -> [];;


let rec find_def_var_number (d1) = match  d1 with 
			Def(x,e1) -> 1|
			Sdef(d1,d2) -> find_def_var_number(d1)+find_def_var_number(d2)|
			Pdef(d1,d2) -> find_def_var_number(d1)+find_def_var_number(d2)|
			Ldef(d1,d2) -> find_def_var_number(d1)+find_def_var_number(d2)|
			_ -> 0;;

let rec map_exec (execute,table,list_of_opcode_list,dump) = match list_of_opcode_list with
			[] ->[]|
			x::xs -> execute([],table,x,dump)::map_exec(execute,table,xs,dump);;


let rec remove_n(table,n) = match table,n with 
			[],n -> raise Emp_Table|
			table,0  -> table|
			x::xs ,n -> remove_n(xs,n-1);;


let rec del_n1_after_n2_table(table,n1,n2) = match  table,n1,n2 with 
		t ,n1,0 -> remove_n(table,n1)|
		x::xs,n1,n2 -> x::del_n1_after_n2_table(table,n1,n2-1)|
		_ -> [];;

let rec remove_x(table,x) = match table,x with
	[],x -> []|
	(y,ans)::ys,x -> if (x=y) then ys else (y,ans)::remove_x(ys,x);;


let rec remove_list(table,var_list) = match table,var_list with
	table,[]-> table|
	table,x::xs -> remove_list(remove_x(table,x),xs);;


exception Same_Var_Cant_be_Defined_Parallely;;
(*Compile Function*)		

let rec compile e = match e with 
	Const n -> [CONST n]| 
	Var x -> [LOOKUP x]|
	Bool t -> (match t with 
					true -> [TRUE]|
					false -> [FALSE])|
	Plus (e1,e2) -> (compile e1) @compile e2@[PLUS]|
	Sub (e1,e2) -> (compile e1)@(compile e2)@[SUB]|
	Mult (e1,e2) -> (compile e1)@(compile e2)@[MULT]|
	Grt (e1,e2) ->  (compile e1) @ (compile e2) @ [GRT]|
	Grte (e1,e2) -> (compile e1)@(compile e2) @ [GRTE]|
	Let (e1,e2) -> (compile e1)@(compile e2) @ [LET]|
	Lete (e1,e2) ->  (compile e1)@(compile e2) @ [LETE]|
	Equ (e1,e2) ->  (compile e1)@(compile e2) @ [EQU] |
	Tup  (e)  -> [TUP(map compile (e))]|
	ITE(e0,e1,e2) -> (compile e0) @ [COND ((compile e1),(compile e2))]| 
	Lambda(x,e) -> [CLOS(x,compile (e)@[RET])]|
	Pair(e1,e2) -> compile(e1) @ compile(e2) @ [APP]|
	Def(Var x,e1)   -> compile(e1) @ [BIND(x)] |
	Sdef(d1,d2) -> compile(d1) @ compile(d2)|(* @ [SDEF]       | *)
	Pdef(d1,d2) -> if (are_Parallel(find_def_var(d1),find_def_var(d2))) then compile(d1) @ compile(d2) else raise Same_Var_Cant_be_Defined_Parallely |(* @ [PDEF]       | *)
	Ldef(d1,d2) -> compile(d1) @ compile(d2)@ [CONST (find_def_var_number(d1))]@[CONST (find_def_var_number(d2))]@[LDEF]	|   
	Letdef(d,e2)  -> compile(d) @ compile(e2)@ [UNBIND( find_def_var(d))]|
	_ -> [];;




(*Compile Function*)		


	
let table = [(Var "x",Res 3);(Var "z",Res 23);(Var "y",Res 1)] ;;  (* exp*exp list *)



(*Execute Function*)
let rec execute (stack,table,opL,dump) = match (stack,table,opL,dump) with
(*Base Cases*)
	  (x::s,table,[],[]) -> x |
	(s,table,CONST n::c,dump) ->execute(Res n ::s,table,c,dump)| 
	(s,table,TRUE::c,dump) ->execute(Tr ::s,table,c,dump)|
	(s,table,FALSE::c,dump) ->execute(Fu ::s,table,c,dump)|
	(s,table,(LOOKUP x)::c,dump) ->execute(find_in_table (x,table)::s,table,c,dump)|

(*Arithmetic*)
	(Res n1 :: Res n2 ::s,table,PLUS :: c,dump) -> execute((Res (n1+n2)) ::s,table,c,dump)|
	(Res n1 :: Res n2 ::s,table,SUB :: c,dump) -> execute((Res (n2-n1)) ::s,table,c,dump)|
	(Res n1 :: Res n2 ::s,table,MULT :: c,dump) -> execute((Res (n1*n2)) ::s,table,c,dump)|
	(Res n1 ::Res n2::s,table,GRT :: c,dump) -> execute(is_grt(Res n2 , Res n1) ::s,table,c,dump)|
	(Res n1 ::Res n2::s,table,GRTE :: c,dump) -> execute(is_grte(Res n2 , Res n1) ::s,table,c,dump)|
	(Res n1 ::Res n2::s,table,LET :: c,dump) -> execute(is_lt(Res n2 , Res n1) ::s,table,c,dump)|
	(Res n1 ::Res n2::s,table,LETE :: c,dump) -> execute(is_lte(Res n2 , Res n1) ::s,table,c,dump)|
	(Res n1 ::Res n2::s,table,EQU:: c,dump) -> execute(is_eq(Res n1 , Res n2) ::s,table,c,dump)|
	(s,table,TUP(c')::c,dump)               -> execute(Rest(map_exec(execute,table,c',dump))::s,table,c,dump)|
	(Tr::s,table,(COND(c1,c2))::c,dump) ->execute(s,table,c1@c,dump)|
	(Fu::s,table,(COND(c1,c2))::c,dump) ->execute(s,table,c2@c,dump)|

	(s,table,CLOS(Var x,c1)::c,dump) ->execute((Vclos(table,x,c1))::s,table,c,dump) |
	(ans::Vclos(t,x,c')::s,table,APP::c,dump) ->execute([],(Var x,ans)::t,c',(s,table,c)::dump)|
	(a::s,table,RET::c,(st,tab,code)::dump) -> execute(a::st,tab,code,dump)|

	(a::s,table,BIND(x)::c,dump) ->execute(s,(Var x,a)::table,c,dump)|
(* 	(var::val::s,table,SDEF::c,dump) ->execute(s,(var,val)::table,c,dump)|
	(var::val::s,table,PDEF::c,dump) ->execute(s,(var,val)::table,c,dump)| *)
	(Res n2::Res n1::s,table,LDEF::c,dump) ->execute(s,del_n1_after_n2_table(table,n1,n2),c,dump)|

	(s,table,UNBIND(l)::c,dump) ->execute(s,remove_list(table,l),c,dump)|




		_ -> Tr;;






(* (*BASE DEF*)
let b1= Bool true;;
let b2  = Bool false;;
let e1 = Var("x");;
let e2 = Const 3;;
let e3 = Const 7;;
(*ARITHMETIC DEF*)
let e4 = Plus (e2,e3);;
let e5 = Sub(e2,e3);;
let e6 = Mult (e2,e3);;
let e7 = Grt(e2,e3);;
let e8 = Grt(e3,e2);;
let e9 = Grte(e2,e3);;
let e10 = Grte(e2,e2);;
let e11 = Grte(e3,e2);;
let e12 = Let(e2,e2);;
let e13 = Let(e3,e2);;
let e14 = Lete(e3,e2);;
let e15 = Sub(e2,e3);;
let e18 = Tup([e1;e2;e3;e4;e5]);;
(*CONDITIONAL DEF*)
let e14 = ITE(e13,e2,e14);;
(*ABSTRACTIONS DEF*)
let e15 = Pair(Lambda (Var "x",Mult(Var "x",Const 4)),Plus (Const 2, Const 1));;
let e20 = Lambda(Var "x",Plus(Plus(Var "x",Var "y"),Var "z"));;
let e21 = Pair (e20,e2);;
(*DEFINITINS DEF*)
let d1 = Def(Var "x" , Const 333);;
let d2 = Def(Var "y" , Const 333);;
let d3 = Def(Var "x" , Const 300);;
let d4 = Def(Var "z", Plus(Var "y",Const 56));;
 (*SEQUENTIAL_DEFINITIONS DEF*)
let s1 = Sdef(d1,d2);;
(*PARALLEL_DEFINITIONS DEF*)
let p1 = Pdef(d1,d2);;
let p2 = Pdef(d1,d3);;  (* raise Same_Var_Cant_be_Defined_Parallely *)
LOCAL_DEFINITIONS 
let l1 = Ldef(d2,d4)

(* QUALIFICATION *)
let e16 = Letdef(d1,Plus(Var "x" ,Const 17));; 
let e17 = Letdef(s1,Plus(Var "x" ,Const 17));;
let e19 = Letdef(l1,Equ(Var "z" , Const 389));;




(*  ---- COMPILE and EXECUTE TESTCASES------*)

execute([],table,compile e1,[]);;
execute([],table,compile e2,[]);;
execute([],table,compile e3,[]);;
execute([],table,compile e4,[]);;
execute([],table,compile e5,[]);;
execute([],table,compile e6,[]);;
execute([],table,compile e7,[]);;
execute([],table,compile e8,[]);;
execute([],table,compile e9,[]);;
execute([],table,compile e10,[]);;
execute([],table,compile e11,[]);;
execute([],table,compile e12,[]);;
execute([],table,compile e13,[]);;
execute([],table,compile e14,[]);;
execute([],table,compile e15,[]);;
execute([],table,compile e16,[]);;
compile e17;;
execute([],table,compile e17,[]);;
compile e18;;
execute([],table,compile e18,[]);;
compile e19;;
execute([],table,compile e19,[]);;
compile e20;;
execute([],table,compile e20,[]);;
compile e21;;
execute([],table,compile e21,[]);; *)











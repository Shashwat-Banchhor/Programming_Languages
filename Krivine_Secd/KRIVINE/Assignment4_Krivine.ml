type exp = Var of string |
		   Lambda of exp*exp |
		   Pair of exp*exp |
		   Const of int |
		   Unit|
		   Plus of exp*exp|
		   Mult of exp*exp|
		   Sub of exp*exp|
		   ITE of exp*exp*exp|
		   Grt of exp*exp|
  		   Grte of exp*exp|	
  		   Let of exp *exp|
  		   Lete of exp*exp|
  		   Equ of exp * exp|
  		   Tup of exp list|
  		   Def of (exp*exp)|
  		   Letdef of (exp*exp)|
 		   Res of int |
		   V of string| 
		   Tr|
	  	   Fu|
		   Vclos of ((exp*exp) list)*exp|
		   Clos of (exp *exp) list * exp|
		   COND of exp*exp|
		   PLUS|
		   SUB|
		   MULT|
		   GRT|
  		   GRTE|	
  		   LET|
  	       LETE |
  		   EQU ;;


exception Typ_Exception;;
exception No_Operation_to_Perform;;

let add(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> Const (n1+n2)
| _ -> raise Typ_Exception;;

let sub(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> Const (n1-n2)
| _ -> raise Typ_Exception;;

let mul(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> Const (n1*n2)
| _ -> raise Typ_Exception;;

let grt(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> if n1>n2 then Tr else Fu
| _ -> raise Typ_Exception;;

let grte(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> if n1>=n2 then Tr else Fu
| _ -> raise Typ_Exception;;

let lest(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> if n1<n2 then Tr else Fu
| _ -> raise Typ_Exception;;

let leste(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> if n1<=n2 then Tr else Fu
| _ -> raise Typ_Exception;;

let equ(e1,e2) = match  e1,e2 with
Const n1 , Const n2 -> if n1=n2 then Tr else Fu
| _ -> raise Typ_Exception;;



let rec find_in_table (x, table) = match x,table with 
		(x,[]) -> Vclos([],x)|
		(x,(y,ans)::t) -> if (x=y) then ans else find_in_table(x,t);;



let table = [(Var "x",Vclos([],Const 5));(Var "x",Vclos([],Const 5));(Var "x",Vclos([],Const 5))] ;;  (* exp*exp list *)


let rec remove_arg1_from_table (x,table) = match x,table with
		x,[] -> []|
		x,(x1,y)::tail -> if (x=x1) then remove_arg1_from_table(x,tail) else (x1,y)::remove_arg1_from_table(x,tail);;

let rec map_exec(execute,l,stack) = match l with 
		[] -> []|
		x::xs -> (execute (Clos([],x),stack))::map_exec(execute,xs,stack);; 


let rec execute(term,stack) = match term,stack with 

	Clos(table,Const n),[] -> Vclos([],Const n)|
	Vclos(table,expr),[] -> Vclos(table,expr)|
	Unit,[]      -> raise No_Operation_to_Perform|


	Clos(table,Tr),[] -> Vclos([],Tr)|
	Clos(table,Fu),[] -> Vclos([],Fu)|
	Clos(table,Tr),cl::s -> execute(cl,Vclos([],Tr)::s)	|
	Clos(table,Fu),cl::s -> execute(cl,Vclos([],Fu)::s)	|

	Clos(table,Var s),[] -> find_in_table(Var s,table)|
	Clos(table,Var p),cl::s  ->  execute(cl,find_in_table(Var p,table)::s)|
(* Arithmetic Inductive Cases*)
	
	Unit,cl::s -> execute(cl,s)|
	Vclos([],e),Clos(t,e1)::s -> execute(Clos(t,e1),Vclos([],e)::s)| 

	Clos(table,Const n),Vclos([],e)::s -> execute(Unit,Vclos([],Const n)::Vclos([],e)::s)|
	Clos(table,Const n),cl::s -> execute(cl,Vclos([],Const n)::s)|

	Clos(table,Plus(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[PLUS]@s)|
	Clos(table,Sub(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[SUB]@s)|
	Clos(table,Mult(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[MULT]@s)|
	Clos(table,Grt(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[GRT]@s)|
	Clos(table,Grte(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[GRTE]@s)|
	Clos(table,Let(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[LET]@s)|
	Clos(table,Lete(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[LETE]@s)|
	Clos(table,Equ(e1,e2)),s -> execute(Clos(table,e1),Clos(table,e2)::[EQU]@s)|
	Clos(table,Tup(l)),s -> execute(Unit,Vclos([],Tup(map_exec(execute,l,[])))::s)|


	Vclos([],Const n1),Vclos([],Const n2)::PLUS::s ->  execute(Unit,Vclos([],add(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::SUB::s -> execute(Unit,Vclos([],sub(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::MULT::s -> execute(Unit,Vclos([],mul(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::GRT::s -> execute(Unit,Vclos([],grt(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::GRTE::s -> execute(Unit,Vclos([],grte(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::LET::s -> execute(Unit,Vclos([],lest(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::LETE::s -> execute(Unit,Vclos([],leste(Const n2,Const n1))::s)|
	Vclos([],Const n1),Vclos([],Const n2)::EQU::s -> execute(Unit,Vclos([],equ(Const n2,Const n1))::s)|

	Clos(table,Lambda(Var x,e1)),cl::s -> execute(Clos((Var x,cl)::table,e1),s)|
	Clos(table,Pair(e1,e2)),s  -> execute(Clos(table,e1),execute(Clos(table,e2),[])::s)|

	Clos(table,ITE(e0,e1,e2)),s -> execute(Clos(table,e0),Clos(table,COND(e1,e2))::s)	|
	Clos(table,(COND(e1,e2))),Vclos([],Tr)::s -> execute(Clos(table,e1),s)|				
	Clos(table,(COND(e1,e2))),Vclos([],Fu)::s -> execute(Clos(table,e2),s)|

	Clos(table,Def(Var x,e)),Clos(t,e1)::s -> execute(Clos((Var x,execute(Clos(table,e),[]))::t,e1),s) |
	Clos(table,Letdef(d,e)),s  -> execute( Clos(table,d),Clos(table,e)::s)|
 
	_ -> raise Typ_Exception;;


(*     (table,Const n),[] -> Vclos(n,[])|
(* 	(table,Const n ),[] -> unpack(table,Const)|
 *)	(table,Bool true),[] -> (Tr,table)|
	(table,Bool false),[] -> (Fu,table)|
	(table,Var(x)),s -> find_in_table(remove_arg1_from_table (Var x , table)),s|
	(table,(Lambda (x,e1))),cl::s -> (e1,(x,cl)::table),s|
	(table,Pair(e1,e2)),s -> (e1,table),(e2,table)::s|
	(table,Const n),s ->  (Res n,table)::;; 
 *)

(* 	*************TestCases *********
(*BASE DEF*)
let b1= Bool true;;
let b2  = Bool false;;
let e1 = Var("x");;   
let e2 = Const 3;;
let e3 = Const 7;;
(*ARITHMETIC DEF*)     
let e4 = Plus (e2,e3);;
let e5 = Sub(e2,e4);;
let e6 = Mult (e2,e3);;
let e7 = Grt(e2,e3);;
let e8 = Grt(e3,e2);;
let e9 = Grte(e2,e3);;
let e10 = Grte(e2,e2);;
let e11 = Grte(e3,e2);;
let e12 = Let(e2,e2);;
let e13 = Let(e3,e2);;
let e14 = Lete(e3,e2);;
let e15 = Lete(e2,e3);;
let e16 = Lete(e2,e2);;
let e17 = Equ(e2,e2);;
let e18 = Equ(e2,e3);;
let e19 = Sub(Sub (e2,Mult(e2,e3)),e5);;
let e20 = Tup([e1;e2;e3;e4;e5]);;
let e21 = Plus(Var "x",Var "y");;
let e22 = Lambda(Var "x",e21);; 
let e23 = Pair(e22,e3);;
let e24 = ITE(Lete(Const 4 , Sub(Sub(Sub(Const 2,Const 1),Const 4),Const (8))),Const 5,Const 6);;
let d1 = Def(Var "x",Const 77);;
let e25 = Letdef(d1,Plus(Var "x",Const 3));;





let c1= Clos([],e1);;
let c2= Clos([],e2);;
let c3= Clos([],e3);;
let c4= Clos([],e4);;
let c5= Clos([],e5);;
let c6= Clos([],e6);;
let c7= Clos([],e7);;
let c8= Clos([],e8);;
let c9= Clos([],e9);;
let c10= Clos([],e10);;
let c11= Clos([],e11);;
let c12= Clos([],e12);;
let c13= Clos([],e13);;
let c14= Clos([],e14);;
let c15= Clos([],e15);;
let c16= Clos([],e16);;
let c17= Clos([],e17);;
let c18= Clos([],e18);;
let c19= Clos([],e19);;
let c20= Clos([],e20);;
let c21= Clos([(Var "x",Vclos([],Const 3));(Var "y",Vclos([],Const 7))],e21);;
let c22= Clos([],e22);;
let c23= Clos([(Var "y",Vclos([],Const 7))],e23);;
let c24= Clos([],e24);;
let c25= Clos([],e25);;






execute(c1,[]);;
execute(c2,[]);;
execute(c3,[]);;
execute(c4,[]);;
execute(c5,[]);;
execute(c6,[]);;
execute(c7,[]);;
execute(c8,[]);;
execute(c9,[]);;
execute(c10,[]);;
execute(c11,[]);;
execute(c12,[]);;
execute(c13,[]);;
execute(c14,[]);;
execute(c15,[]);;
execute(c16,[]);;
execute(c17,[]);;
execute(c18,[]);;
execute(c19,[]);;
execute(c20,[]);;
execute(c21,[]);;
execute(c22,[]);;
execute(c23,[]);;
execute(c24,[]);;
execute(c25,[]);;
 *)

















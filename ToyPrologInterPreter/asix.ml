type predicate  = And|
				  Or|
				  If|
				  Not|
				  Pred of string;;

(* type function =  *)

type  term = Var of string |
			 V of string|
			 Const of int | 
			 Func of string*int*(term list)|Nil ;;




type atomic_formula =  AtomicFormula of predicate*(term list)|Fail|Cut  ;;

type goal = Seq of atomic_formula list;;

type head = Head of atomic_formula ;;

type body = And of body | Or of body | L of atomic_formula list ;; 

type clauses =  Clause of head * body  ;;

type program = List of (atomic_formula* body) list;;



type answer =  Bool of bool| Res of  int|Rest of answer list;;

exception ExistanceError;;

(* let rec  give_var_term( v,check_list ) = match  v,check_list with
	v,[]        -> v|
	v,(w,t)::xs -> if (v=w) then t else  give_var_term(v,xs) ;; *)

let rec  give_var_term( v,check_list ) = match  v,check_list with
	v,[]        -> Nil|
	v,(w,t)::xs -> if (v=w) then t else  give_var_term(v,xs) ;;

let rec get_sec_var(v,l,va) = match v,l,va with 
	v,[],va -> []  |
	v,(x,w)::xs,va -> if (v=w) then ((x,va)::get_sec_var(v,xs,va)) else (x,w)::get_sec_var(v,xs,va);;

let rec update_sec_var(l1,l2) = match l1,l2 with
	[],l2 -> l2|
	(Var (x),w)::l1s , l2 -> update_sec_var(l1s,get_sec_var(Var(x),l2,w));;

(* update_sec_var( get_unify_list( [Var("Z");V "b"] , [Var("X");Var("X")] , [] ) ,get_unify_list( [Var("Z");V "b"] , [Var("X");Var("X")] , [] ));; 
 *)

(* 
let rec update(l1,l2) = match l1,l2 with *)





(* let check_var_can_be_used(variable, term, check_list) = match variable,term,check_list with 
	v,t,[] ->  true|
	v,t,check_list -> if (give_var_term(v,check_list) = t || give_var_term(v,check_list) = v) then true else  false;;  *)

let check_var_can_be_used(variable, term, check_list) = match variable,term,check_list with 
	v,t,[] ->  true|
	v,t,check_list -> if (give_var_term(v,check_list) = t || give_var_term(v,check_list) = Nil) then true 
						else (match  give_var_term(v,check_list) with 
									Var(y) -> true | _ -> false );; 

(* let rec unify_term(t1,t2) = match t1,t2 with
		[],[] -> raise ExistanceError|
 *)
(*  let rec unifiable_terms(tl1,tl2,check_list) =   (* tl1 is one with vaiable and tl2 is a fact*)
 	match tl1,tl2,check_list with

		 	[],[],check_list -> true|    (*  Next line is the variable checking *)
		 	Var(x)::xs,y::ys,check_list -> if (check_var_can_be_used(Var(x),y,check_list)=false) then false else if (give_var_term(Var(x),check_list)= Var(x)) then unifiable_terms(xs,ys,(Var(x),y)::check_list) else unifiable_terms(xs,ys,check_list)|
		 	x::xs , Var(y)::ys,check_list  -> if (check_var_can_be_used(Var(y),x,check_list)=false) then false else if (give_var_term(Var(y),check_list)= Var(y)) then unifiable_terms(xs,ys,(Var(y),x)::check_list) else unifiable_terms(xs,ys,check_list)|
		 	t1::xs,t2::ys,check_list    -> if (t1=t2) then unifiable_terms(xs,ys,check_list) else false ;; *)(* unifiable_terms(xs,ys,check_list) ;;  *)


 let rec unifiable_terms(tl1,tl2,check_list) =   (* tl1 is one with vaiable and tl2 is a fact*)
 	match tl1,tl2,check_list with

		 	[],[],check_list -> true|    (*  Next line is the variable checking *)
		 	Var(x)::xs,y::ys,check_list -> if (check_var_can_be_used(Var(x),y,check_list)=false) then false 

		 								   else (if (give_var_term(Var(x),check_list)= Nil) then unifiable_terms(xs,ys,(Var(x),y)::get_sec_var(Var(x),check_list,y) ) 
		 								   		else( match give_var_term(Var(x),check_list) with

		 								   		 		Var(z) -> ( match (give_var_term(Var(z),check_list)) with 
		 								   		 						Nil -> unifiable_terms(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						y ->   unifiable_terms(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						Var(i) ->  unifiable_terms(xs,ys,get_sec_var(Var(z),check_list,y)) )   |
		 											

		 												_ -> unifiable_terms(xs,ys,check_list))) |
		 	y::xs , Var(x)::ys,check_list  -> if (check_var_can_be_used(Var(x),y,check_list)=false) then false 

		 								   else (if (give_var_term(Var(x),check_list)= Nil) then unifiable_terms(xs,ys,(Var(x),y)::get_sec_var(Var(x),check_list,y)) 
		 								   		else( match give_var_term(Var(x),check_list) with

		 								   		 		Var(z) -> ( match (give_var_term(Var(z),check_list)) with 
		 								   		 						Nil -> unifiable_terms(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						y ->   unifiable_terms(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						Var(i) ->  unifiable_terms(xs,ys,get_sec_var(Var(z),check_list,y)) )   |
		 											

		 												_ -> unifiable_terms(xs,ys,check_list))) |
		 	t1::xs,t2::ys,check_list    -> if (t1=t2) then unifiable_terms(xs,ys,check_list) else false ;;
(* 
let rec get_unify_list(tl1,tl2,check_list) = match tl1,tl2,check_list with
	 [],[],check_list -> check_list|    (*  Next line is the variable checking *)
		 	Var(x)::xs,y::ys,check_list -> if (give_var_term(Var(x),check_list)= Var(x)) then get_unify_list(xs,ys,(Var(x),y)::check_list) else get_unify_list(xs,ys,check_list)|
		 	t1::xs,t2::ys,check_list    ->  get_unify_list(xs,ys,check_list) ;;
 *)


let rec get_unify_list(tl1,tl2,check_list) = match tl1,tl2,check_list with
	 [],[],check_list -> check_list|    (*  Next line is the variable checking *)
		 	Var(x)::xs,y::ys,check_list -> (if (give_var_term(Var(x),check_list)= Nil) then (match y with 
		 																							Var(k) -> if (give_var_term(Var(k),check_list)=Nil) then  get_unify_list(xs,ys,(Var(x),y)::get_sec_var(Var(x),check_list,y))
		 																										else   get_unify_list(xs,ys,(Var(x),give_var_term(Var(k),check_list))::get_sec_var(Var(x),check_list,give_var_term(Var(k),check_list)))       |
		 																							p     -> get_unify_list(xs,ys,(Var(x),p)::get_sec_var(Var(x),check_list,p))

		 																									)  
		 								   		else( match give_var_term(Var(x),check_list) with

		 								   		 		Var(z) -> ( match (give_var_term(Var(z),check_list)) with 
		 								   		 						Nil -> get_unify_list(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						Var(i)->   get_unify_list(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						_ ->  get_unify_list(xs,ys,get_sec_var(Var(z),check_list,y)) )   |
		 											

		 												_ -> get_unify_list(xs,ys,check_list)))  |
		 	

		 	y::xs,Var(x)::ys,check_list ->  (if (give_var_term(Var(x),check_list)= Nil) then get_unify_list(xs,ys,(Var(x),y)::get_sec_var(Var(x),check_list,y)) 
		 								   		else( match give_var_term(Var(x),check_list) with

		 								   		 		(* let c = print_string ("Yes") in  *)
		 								   		 		Var(z) -> ( match (give_var_term(Var(z),check_list)) with 
		 								   		 						Nil -> get_unify_list(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						Var(i) ->   get_unify_list(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						_ ->  get_unify_list(xs,ys,get_sec_var(Var(z),check_list,y))  )   |
		 											

		 												_ -> get_unify_list(xs,ys,check_list)   ))  |
		 	

			t1::xs,t2::ys,check_list    ->  get_unify_list(xs,ys,check_list) ;; (* unifiable_terms(xs,ys,check_list)*) 

(* let rec get_unify_list2(tl1,tl2,check_list) = match tl1,tl2,check_list with
	 [],[],check_list -> check_list|    (*  Next line is the variable checking *)
		 	Var(x)::xs,y::ys,check_list -> if (give_var_term(Var(x),check_list)= Var(x)) then get_unify_list2(xs,ys,(Var(x),y)::check_list) else get_unify_list2(xs,ys,check_list)|
		 	(* x::xs,Var(y)::ys,check_list -> if (give_var_term(Var(y),check_list)= Var(y)) then get_unify_list(xs,ys,(Var(y),x)::check_list) else get_unify_list(xs,ys,check_list)| *)
		 	t1::xs,t2::ys,check_list    ->  get_unify_list2(xs,ys,check_list) ;; *)(* unifiable_terms(xs,ys,check_list) ;;  *)

let rec get_unify_list2(tl1,tl2,check_list) = match tl1,tl2,check_list with
	 [],[],check_list -> check_list|    (*  Next line is the variable checking *)
		 	Var(x)::xs,y::ys,check_list ->  (if 	(give_var_term(Var(x),check_list)= Nil) then (match y with 																	(* (give_var_term(Var(x),check_list)= Nil) then get_unify_list2(xs,ys,(Var(x),y)::get_sec_var(Var(x),check_list,y))  *)
		 								   																Var(k) -> if (give_var_term(Var(k),check_list)=Nil) then  get_unify_list(xs,ys,(Var(x),y)::get_sec_var(Var(x),check_list,y))
		 																										else   get_unify_list(xs,ys,(Var(x),give_var_term(Var(k),check_list))::get_sec_var(Var(x),check_list,give_var_term(Var(k),check_list)))       |
		 																							p     -> get_unify_list(xs,ys,(Var(x),p)::get_sec_var(Var(x),check_list,p))

		 																									) 

		 								   		else( match give_var_term(Var(x),check_list) with

		 								   		 		Var(z) -> ( match (give_var_term(Var(z),check_list)) with 
		 								   		 						Nil -> get_unify_list2(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						Var(i) ->   get_unify_list2(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						_ ->  get_unify_list2(xs,ys,get_sec_var(Var(z),check_list,y)) )   |
		 											

		 												_ -> get_unify_list2(xs,ys,check_list)))  |
		 	
		 	

		 	y::xs,Var(x)::ys,check_list ->  (if (give_var_term(Var(x),check_list)= Nil) then get_unify_list2(xs,ys,(get_sec_var(Var(x),check_list,y))) 
		 								   		else( match give_var_term(Var(x),check_list) with

		 								   		 		Var(z) -> ( match (give_var_term(Var(z),check_list)) with 
		 								   		 						Nil -> get_unify_list2(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						Var(i) ->   get_unify_list2(xs,ys,get_sec_var(Var(z),check_list,y)) |
		 								   		 						_ ->  get_unify_list2(xs,ys,get_sec_var(Var(z),check_list,y)) )   |
		 											

		 												_ -> get_unify_list2(xs,ys,check_list)))  |

		 	t1::xs,t2::ys,check_list    ->  get_unify_list2(xs,ys,check_list) ;;

let rec update_all_sol( l1,l2) = match l1,l2 with 
	[],l2 -> [l2]|
	x::xs,l2 -> update_sec_var(x,l2) :: update_all_sol(xs,l2)  ;; 


(* let rec remove_duplicates() *)



let get_first(a) = match a with
	(a,b) -> a;;



let get_sec(a) = match a with
	(a,b) -> b;;


let rec get_sec_all(l) = match l with
	[] -> []|
	(a,b)::xs -> b :: get_sec_all(xs);;


let get_second(a) = match a with
	(a,And(L(b))) -> b|
	(a,Or(L(b))) -> b ;;


let get_second_operation(a) = match a with
	(a,And(L(b))) -> "And" |
	(a,Or(L(b))) -> "Or" |
	(a,(L(b))) -> "Nil" ;;

let get_bool(l) = match l with
	[] -> false|
	x::xs -> get_first(x);;



let rec get_map(var,map_list) = match var,map_list with   (* Gives the Mapping of the variable *)
	var,[] -> var|
	var,(v,t)::xs -> if (var = v) then t else get_map(var,xs);; 


let rec union(l1,l2) = match l1,l2 with 
	[],[] -> []|
	[],l2 -> l2|
	l1,[] -> l1|
	x::xs,l2 -> if (List.mem x l2) then  union(xs,l2) else union(xs,x::l2) ;;




let rec subst(map_list,term_list) = match term_list with
		[] -> []|
		Var(x)::xs -> get_map(Var(x),map_list) :: subst(map_list,xs) |
		x::xs     ->   x:: subst(map_list,xs) 	;;


let rec substitute(map_list,atm_for)= match map_list , atm_for with
		map_list , []     ->    [] |
		map_list , AtomicFormula(p,h1):: xs ->  AtomicFormula(p,subst(map_list,h1))::substitute(map_list,xs) ;;

let rec substitute_single(map_list,atm_for)= match map_list , atm_for with
		map_list , AtomicFormula(p,h1)->  AtomicFormula(p,subst(map_list,h1)) ;;

 (* substitute(get_unify_list(get_head(cl),get_head(get_first(cl1)),[]), get_second(cl1)) *)

let unify_head(cl1,cl2) = match cl1,cl2 with 
	AtomicFormula(Pred(x),h1),(AtomicFormula(Pred(y),h2)) ->  if (x=y) then  unifiable_terms(h1,h2,[])  else  false|  (*  if (x=y) then (unifiable_terms(h1,h2,[])) | *)
	AtomicFormula(Pred(x),h1),(AtomicFormula(Pred(y),h2)) -> false ;;  


let get_body(cl) = match cl with
   	AtomicFormula(p,h),b -> b ;;


let get_term_prog(cl) = match cl with 
	AtomicFormula(p,h),b -> h ;;
	
let get_head(cl) = match cl with 
	AtomicFormula(p,h) -> h ;;

let rec has_cut(cl1) = let b = get_second(cl1) in 
							(List.mem Cut b);;

(* let rec commute_unif(l1,check_list) = match l1,check_list with
		 [],check_list -> []|
		 x::xs,check_list ->  *)

let rec mapIftrueAnd(l,check_clause,prog) = match l with   (* If a fact use And *)
		 [] -> true|
		 Fail::xs -> false|
		 x::xs -> if (check_clause(x,prog,prog)) then mapIftrueAnd(xs,check_clause,prog) else false ;;

(* let rec mapIftrueAnd(l,check_clause,prog,map_list) = match l with   (* If a fact use And *)
		 [] -> true|
		 Fail::xs -> false|
		 AtomicFormula(p,h)::xs -> if (check_clause(AtomicFormula(p,subst(map_list,h)),prog,prog)) then mapIftrueAnd(xs,check_clause,prog,) else false ;; *)


let rec mapIftrueOr(l,check_clause,prog) = match l with 
		 [] -> false|
		 x::xs -> if (check_clause(x,prog,prog)) then true else mapIftrueOr(xs,check_clause,prog) ;;

let rec check_clause(cl,check_rule_prog,prog) = match cl,check_rule_prog ,prog with
		cl,List([]),prog -> raise ExistanceError|
		cl,List(cl1::p),prog -> if (unify_head(cl,get_first(cl1))=false) then false else (if (get_second_operation(cl1)="And") then mapIftrueAnd( get_second(cl1),check_clause ,prog) else if (get_second_operation(cl1)="Or") then mapIftrueOr( get_second(cl1),check_clause ,prog) else check_clause(cl,List(p),prog));;

let rec check_clause_h(cl,check_rule_prog,prog) = match cl,check_rule_prog ,prog with
		cl,List([]),prog -> []|
		cl,List(cl1::p),prog -> if (unify_head(cl,get_first(cl1))=false) then check_clause_h(cl,List(p),prog)(*[(false,[])]*) else (if (get_second_operation(cl1)="And") then (if (mapIftrueAnd( substitute(get_unify_list(get_head(cl),get_head(get_first(cl1)),[]), get_second(cl1)) ,check_clause ,prog) = true) then [(true, get_unify_list2(get_head(cl),get_head(get_first(cl1)),[]))] @ check_clause_h(cl,List(p),prog) else check_clause_h(cl,List(p),prog)(*[(false,[])]*) ) else if (get_second_operation(cl1)="Or") then (if (mapIftrueOr(  substitute(get_unify_list(get_head(cl),get_head(get_first(cl1)),[]), get_second(cl1))  ,check_clause ,prog) = true) then [(true,get_unify_list2(get_head(cl),get_head(get_first(cl1)),[] ))] @ check_clause_h(cl,List(p),prog) else  check_clause_h(cl,List(p),prog) (*[(false,[])]*)  ) else check_clause_h(cl,List(p),prog));;


let rec solve_sub_goal(l,prog_pointer,prog,map_list) = match l,prog_pointer,prog with
		[],prog_pointer,prog -> [(true,map_list)]|
		l,[],prog    -> [] | 
		 Fail::xs,cl::ps,prog -> [(false,[])]| 
		 Cut::xs, cl::ps,prog -> solve_sub_goal(xs,cl::ps,prog,map_list)	|
		x::xs,cl::ps,prog -> if (unify_head(substitute_single(map_list,x),get_first(cl)) =true) then 
													( if ( get_bool(solve_sub_goal(substitute(map_list,get_second(cl)),prog,prog, update_sec_var(  get_unify_list(subst( map_list,get_head(x)),subst(map_list,get_head(get_first(cl))) , [] ),get_unify_list(subst( map_list,get_head(x)),subst(map_list,get_head(get_first(cl))) ,[]))(*get_unify_list( get_head(x) ,get_head(get_first(cl)),[]) *)      ) ) = true) then 
																solve_sub_goal(xs,prog,prog,  union ( update_sec_var(get_unify_list(subst( map_list,get_head(x)),subst(map_list,get_head(get_first(cl))) , [] ),map_list) ,update_sec_var(get_unify_list2(subst( map_list,get_head(x)),subst(map_list,get_head(get_first(cl))) , [] ),get_unify_list2(subst( map_list,get_head(x)),subst(map_list,get_head(get_first(cl))) ,[])))  ) @ solve_sub_goal(x::xs,ps,prog,map_list)
													else solve_sub_goal(x::xs,ps,prog,map_list) ) 
							else    solve_sub_goal(x::xs,ps,prog,map_list)  ;; 



let rec rem_dup(cl,l)  = match  l with
	[]  -> cl|
	(Var x , V p) :: xs  -> if (List.mem (Var x , V p) cl ) then rem_dup(cl,xs) else (rem_dup((Var x , V p)::cl,xs) )|
	x::xs  -> rem_dup(cl,xs);;



let rec rem_d(cl,l)  = match  l with
	[]  -> cl|
	x::xs  -> if (List.mem x cl) then rem_d(cl,xs) else (rem_d(x::cl,xs) ) ;;



let rec map_d(l) = match l with
		[] -> []|
		x::xs -> rem_dup([],x) :: map_d(xs) ;;



let rec evaluate(goal,prog) = match goal,prog with	
		[],prog  	 ->  []| 
		goal,List([]) ->  raise ExistanceError|
		cl::remgoal,prog -> if (check_clause_h(cl,prog,prog) = []) then [(false,[])]::evaluate(remgoal,prog) else check_clause_h(cl,prog,prog)::evaluate(remgoal,prog);;

let rec check_clause_h1(cl,check_rule_prog,prog) = match cl,check_rule_prog ,prog with
		cl,List([]),List(prog) -> []|
		cl,List(cl1::p),List(prog) -> if (unify_head(cl,get_first(cl1))=false) then [] @ ( check_clause_h1(cl,List(p),List(prog)) ) (*[(false,[])]*) 

									  else 
									  	if (has_cut(cl1)) then 
									  					(if (get_bool( solve_sub_goal(get_second(cl1),prog,prog,get_unify_list( get_head(cl),get_head(get_first(cl1)), []))) = true) then ([(true,rem_d([],map_d(update_all_sol(get_sec_all(solve_sub_goal(get_second(cl1),prog,prog,get_unify_list( get_head(cl),get_head(get_first(cl1)), []))) ,get_unify_list2( get_head(cl),get_head(get_first(cl1)), []) ) )) )])
														else (check_clause_h1(cl,List(p),List(prog))) )
									  	else	(if (get_bool( solve_sub_goal(get_second(cl1),prog,prog,get_unify_list( get_head(cl),get_head(get_first(cl1)), []))) = true) then ([(true,rem_d([],map_d(update_all_sol(get_sec_all(solve_sub_goal(get_second(cl1),prog,prog,get_unify_list( get_head(cl),get_head(get_first(cl1)), []))) ,get_unify_list2( get_head(cl),get_head(get_first(cl1)), []) ))) )]@check_clause_h1(cl,List(p),List(prog))) 
											else (check_clause_h1(cl,List(p),List(prog))) ) ;;

(* let rec print_vars v = match v with
	[] -> Printf.printf "\n"|
(Var x)::l -> Printf.printf "Var %s ;"(x);print_vars(l);; *)

let rec print_term_list v = match v with
	[] -> Printf.printf "\n"|
(Var x)::l -> Printf.printf "Var(%s);"(x);print_term_list(l)|
(V x)::l -> Printf.printf "V(%s);"(x);print_term_list(l)  ;;


let rec p_t(t) = match t with 
	Var (x) -> Printf.printf "Var(%s);"(x)|
	V(x) -> Printf.printf "V(%s);"(x) ;;


let rec p_tt_list v = match v with
	[] -> Printf.printf "]"|
(Var (x),V(y))::l -> Printf.printf "(Var(%s),"(x);Printf.printf "V(%s));"(y); p_tt_list(l)   ;; 

let rec p_tt_ll(l) = match l with 
	[] ->  Printf.printf "]" |
	x::xs -> Printf.printf "[" ; p_tt_list(x); p_tt_ll(xs);;



let rec p_sol(ans)  = match ans with
	(false,x) -> Printf.printf "(false,[[]])\n"|
	(true,y)  -> Printf.printf "(true,[";p_tt_ll(y);Printf.printf ")";;




let rec p_e2(an) =  match an with
	[] ->   Printf.printf""|
	x::xs -> p_sol(x);p_e2(xs) ;;

let rec sp_e2(an) =  match an with
	[] -> Printf.printf""|
	x::xs -> p_e2(x);Printf.printf"\n";sp_e2(xs) ;;

let rec p_e3(ans) =  match ans with
	[] -> Printf.printf""|
	x::xs -> sp_e2(x);Printf.printf"\n\n";p_e3(xs) ;;









let rec print_atomic_formula(a_f) = match a_f with 
		[AtomicFormula(Pred(p),t_l)] -> Printf.printf "AtomicFormula(Pred(%s),["(p); print_term_list(t_l);Printf.printf "])";;

let rec evaluate2(goal,prog) = match goal,prog with	
		[],prog  	 ->  []| 
		goal,List([]) ->  raise ExistanceError|
		cl::remgoal,prog -> if (check_clause_h1(cl,prog,prog) = []) then [(false,[[]])]::evaluate2(remgoal,prog) else check_clause_h1(cl,prog,prog)::evaluate2(remgoal,prog);;

let rec evaluate3(goal,prog) = match goal,prog with
	   [], prog -> []|
	   goal,List([]) -> raise ExistanceError|
	   g1::goals,prog -> evaluate2([g1],prog) :: evaluate3(goals,prog) ;;


let rec p_evaluate3(goal,prog) = match goal,prog with
	   [], prog -> Printf.printf""|
	   goal,List([]) -> raise ExistanceError|
	   g1::goals,prog -> sp_e2(evaluate2([g1],prog));Printf.printf"\n" ;p_evaluate3(goals,prog) ;;


let p = List([ (AtomicFormula(Pred("edge"),[V("a");V("b")]),And(L([]))) ; (AtomicFormula(Pred("edge"),[V("b");V("c")]),And(L([]))) ;(AtomicFormula(Pred("edge"),[V("c");V("d")]),And(L([]))) ;(AtomicFormula(Pred("edge"),[V("d");V("e")]),And(L([]))) ; (AtomicFormula(Pred("path"),[Var("X");Var("X")]),And(L([])))  ; (AtomicFormula(Pred("path"),[Var("X");Var("Y")]),And(L([AtomicFormula(Pred("edge"),[Var("X");Var("Z")]) ; AtomicFormula(Pred("path"),[Var("Z");Var("Y")]) ])))]) ;;
let p1 = List([ (AtomicFormula(Pred("edge"),[V("a");V("b")]),And(L([]))) ;(AtomicFormula(Pred("edge"),[V("b");V("c")]),And(L([]))) ; (AtomicFormula(Pred("path"),[Var("X");Var("X")]),And(L([])))  ; (AtomicFormula(Pred("path"),[Var("X");Var("Y")]),And(L([AtomicFormula(Pred("edge"),[Var("X");Var("Z")]) ; AtomicFormula(Pred("path"),[Var("Z");Var("Y")]) ])))]) ;;

let cl1 = (AtomicFormula(Pred("edge"),[V("a");V("b")]),And(L([]))) ;;
let cl2 =  (AtomicFormula(Pred("edge"),[V("b");V("c")]),And(L([]))) ;;
let cl3 =  (AtomicFormula(Pred("path"),[Var("X");Var("X")]),And(L([])))  ;;
let cl4 = (AtomicFormula(Pred("path"),[Var("X");Var("Y")]),And(L([AtomicFormula(Pred("edge"),[Var("X");Var("Z")]) ; AtomicFormula(Pred("path"),[Var("Z");Var("Y")]) ]))) ;;
let q1 = AtomicFormula(Pred("edge"),[V("a");V("b")]) ;;
let q2 = AtomicFormula(Pred("edge"),[V("b");V("c")]) ;;
let q3 = AtomicFormula(Pred("path"),[V("a");V("a")]) ;;
let q4 = AtomicFormula(Pred("path"),[V("a");V("b")]) ;;
let q5 = AtomicFormula(Pred("path"),[V("a");V("c")]) ;;
let q6 = AtomicFormula(Pred("path"),[V("a");V("d")]) ;;
let q7 = AtomicFormula(Pred("path"),[V("a");V("e")]) ;;
let q8 = AtomicFormula(Pred("path"),[Var("X");V("c")]) ;;

let e = evaluate2([q8],p);;
(* p_sol(((true,[[]]))) ;; *)

(* sp_e2(e);; *)



let prog = [(AtomicFormula(Pred("edge"),[V("a");V("b")]),And(L([]))) ; (AtomicFormula(Pred("edge"),[V("b");V("c")]),And(L([]))) ; (AtomicFormula(Pred("path"),[Var("X");Var("X")]),And(L([])))  ; (AtomicFormula(Pred("path"),[Var("X");Var("Y")]),And(L([AtomicFormula(Pred("edge"),[Var("X");Var("Z")]) ; AtomicFormula(Pred("path"),[Var("Z");Var("Y")]) ])))] ;;
let ch_1 = check_clause_h1(q4,p,p);;
let s = solve_sub_goal(get_second(cl4),prog,prog,get_unify_list( get_head(q4),get_head(get_first(cl4)), [])) ;;
let un1= unify_head(q4,get_first(cl4));;


let s2 =solve_sub_goal(get_second(cl4),prog,prog, get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]));;


let u1 = get_unify_list( get_head(q4),get_head(get_first(cl4)), []) ;;
let s2 = solve_sub_goal(substitute([],get_second(cl4)),prog,prog, update_sec_var(  get_unify_list(subst( [],get_head(q4)),subst([],get_head(get_first(cl4))) , [] ),get_unify_list(subst( [],get_head(q4)),subst([],get_head(get_first(cl4))) ,[]))) ;;
let u1 = update_sec_var(  get_unify_list(subst( [],get_head(q4)),subst([],get_head(get_first(cl4))) , [] ),get_unify_list(subst( [],get_head(q4)),subst([],get_head(get_first(cl4))) ,[])) ;;
let u1 = get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]);;
let gs = get_second(cl4);;
let s3 = solve_sub_goal([AtomicFormula(Pred("edge"),[Var("X");Var("Z")])],prog,prog, get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]))
let a1 = AtomicFormula(Pred("edge"),[Var("X");Var("Z")]);;
let a2 = AtomicFormula(Pred("path"),[Var("Z");Var("Y")]);;
let s1 = substitute_single(u1,a1);;
let u2 = unify_head(s1,get_first(cl1)) ;;
let s2 = substitute(u1,get_second(cl1))
let unify_list_1 = get_unify_list( get_head(a1) , get_head(get_first(cl1)),[]);;

let m1 = update_sec_var( get_unify_list(  subst( get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]),get_head(a1)),   subst(get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]),get_head(get_first(cl1))) , [] )  ,get_unify_list(subst( get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]),get_head(a1)),subst(get_unify_list( get_head(q4) , get_head(get_first(cl4)),[]),get_head(get_first(cl1))) ,[]) ) ;;

let s4 = solve_sub_goal( [a2],prog,prog,unify_list_1 );;
let p = List([(AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")]),And(L([])));(AtomicFormula(Pred("surname"),[V("Satwik");V("banchhor")]),And(L([])))]) ;;
let p = List([(AtomicFormula(Pred("Son"),[V("Raghwendra")]),And(L([])));(AtomicFormula(Pred("Father"),[Var("X");Var("Y")]),And(L([AtomicFormula(Pred("Son"),[Var("Y")])])))]) ;;
let a = AtomicFormula(Pred("Father"),[V("CK");V("Raghwendra")]);; 

(* let t = List([AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")])  , And(L([])) ]) ;;

let a = AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")]);;

let t_l = [V("shashwat");V("banchhor")];; *)

(* let p = List([AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")]),And(L([]))]) ;;

let p1 = List([AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")]),And(L([Fail]))])

let a = AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")]);;

let d =  AtomicFormula(Pred("surname"),[Var("X");Var("Y")]);;

let c = AtomicFormula(Pred("surname"),[Var("X");V("banchhor")]);; 

let a = AtomicFormula(Pred("Son"),[V("Shashwat")]);; 
let b = AtomicFormula(Pred("Son"),[V("Satwik")]);; 
let c = AtomicFormula(Pred("Son"),[V("Raghwendra")]);; 
let a = AtomicFormula(Pred("Father"),[V("CK");V("Raghwendra")]);; 

let p = List([ AtomicFormula(Pred("surname"),[V("shashwat");V("banchhor")]),And(L([]))]) ;;


*)
 
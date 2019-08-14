
type variable  = Var of string;;
type symbol  = Sym of string;;
type term = V of variable | Node of symbol*(term list);;

(*  Signature is represented as list of (Symbol,arity) pairs *)
let signature = [(Sym "Zero",0);(Sym "One",0);(Sym "Sum",2);(Sym "Succ",1);(Sym "Pred",1)];;



let f x e= if (x==e) then true else false;; (*Returns true if a and e are equal*)
let find l e = List.mem e l ;; (*Finds a given element e in the list l*)
let rec get_first l = match l with (* ('a * 'b) list -> 'a list*)
	[] -> []|
	(e1,e2)::xs -> e1::(get_first(xs));;  (*returns the list of the first element of each element of the pair list*)
let rec get_second l = match l with (* ('a * 'b) list -> 'b list*)
	[] -> []|
	(e1,e2)::xs -> e2::(get_second(xs));;   (*returns the list of the second element of each element of the pair list*)

let symbols = get_first(signature);;
let arities = get_second (signature);;

let rec check_sig l = match l with (*('a * int) list -> bool *)
 
	[] -> true|
	(s,a)::xs -> if ((find (get_first xs) s) ||(a<0)) then false else  (check_sig xs);; (*Checks similarity with the first element returns accordingly*)

(*	****Testcases****
		let signature = [(Sym "Zero",0);(Sym "Sum",2);(Sym "Succ",1)];; (true)
		let signature = [(Sym "Zero",0);(Sym "Sum",2);(Sym "Sum",1)];;   /// Same symbol name with different arrity is also not allowed
		let signature = [(Sym "Zero",-1);(Sym "Sum",2);(Sym "Succ",1)];; (false -ve arrity)
		let signature = [(Sym "Zero",0);(Sym "Sum",2);(Sym "Succ",1);(Sym "Pred",1)];; (true)
		let signature = [(Sym "Zero",-1);(Sym "Sum",-2);(Sym "Succ",1);(Sym "Pred",1)];;(false)
		let signature = [(Sym "Sum",-1);(Sym "Sum",2);(Sym "Sum",1)];;(false)

		check_sig signature;;
*)

let rec map f  l = match l with (*('a -> 'b)->'a list-> 'b list*)
		    [] -> []|  
		  	x::xs -> (f x)::(map f xs);; (*map function converts 'a list  to 'b list *)
let rec length l = match  l with    (* 'a list -> int   returns length of the list*)
 [] -> 0| x::xs -> 1+(length xs);;
let conj (p,q) = match p,q with (*Logical And operation  (bool*bool->bool) *)
			true ,true-> true|
			_ -> false;;    

exception Not_present;;

let rec find_pos l e n = match l,e,n with (*'a list -> 'a -> int -> int *)
	[],e,n -> raise Not_present|
	x::xs,e,n -> if (x=e) then n else (find_pos xs e (n+1));;

let rec get_n l n = match l,n with(*'a list -> int -> 'a*)
	[] ,n -> raise Not_present|
	x::xs,0 -> x |
	x::xs,n -> get_n xs (n-1);; 

let rec foldl f e l = match l,e with (*('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)
	[],e -> e|
	x::xs,e -> foldl f (f(x,e)) xs;;


let check_arity s l symbols arities =   (* 'a -> 'b list -> 'a list -> int list -> bool*)
	let x = (find_pos symbols s 0) in
		let y = (get_n arities x) in 
			if  length l = y then true else false;;  (*  Checks wheather the arrity of symbol s is consistent with length of l   *)

let max_of(x,e) =if (x>e) then x else e;; (*returns maximum of x and e*)
let sum(x,e) = x+e;; (*Add the two inputs*)




let rec  wfterm  symbols arities t = match t with (* symbol list -> int list -> term -> bool  *) 
	V x -> true|
	Node (s,[]) -> if (check_arity s [] symbols arities) then  true else false|
	Node (s,l) -> if (check_arity s l symbols arities) then  (foldl conj true (map (wfterm symbols arities) l)) else false;;


(*	****Testcases****
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Zero",[])]));; (true)
   	let  t = (Node (Sym "Zero",[Node(Sym "Zero",[]);Node(Sym "Zero",[])]));; (false )
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Pred",[])]));;  (false Pred doesnt have arrity 0)
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Pred",[Node (Sym "Zero",[])])]));; (true)
   	let  t = (Node (Sym "Succ",[Node(Sym "Zero",[]);Node(Sym "Zero",[])]));; (false Succ has arrity 1 )
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Sum",[Node (Sym "Zero",[]);Node (Sym "Zero",[])])]));;(true)
   	let  t = (Node (Sym "Sum",[Node (Sym "Zero",[]);V (Var "x")]));; (true)
   


		wfterm symbols arities t ;;
*)



let rec ht t = match t with  (* term -> int *)
	V x -> 0|
	Node(s,[]) -> 0|
	Node(s,l) -> 1+(foldl max_of 0 (map ht l));; (* returns the heigth of the preterm with height of leaves assumed 0 *)
	(*	****Testcases****
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Zero",[])]));; (1)
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Pred",[Node (Sym "Zero",[])])]));; (2)
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Sum",[Node (Sym "Zero",[]);Node (Sym "Zero",[])])]));; (2)
   	let  t = (Node (Sym "Zero",[]));; (0)
   	let  t = (Node (Sym "Sum",[Node (Sym "Zero",[]);V (Var "x")]));;(1)


		ht t ;;
*)
 
let rec size t = match t with  (* term -> int*)
	V x -> 1|
	Node(s,[]) -> 1|
	Node(s,l) -> 1+ (foldl sum 0 (map size l));; (* Applying flodl using sum function to  add all the sizes
													of subtrees mapping the size function to the term list*)

	(*	****Testcases****
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Zero",[])]));; (3)
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Pred",[Node (Sym "Zero",[])])]));; (4)
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Sum",[Node (Sym "Zero",[]);Node (Sym "Zero",[])])]));; (5)
   	let  t = (Node (Sym "Zero",[]));; (1)
   	let  t = (Node (Sym "Sum",[Node (Sym "Zero",[]);V (Var "x")]));; (3)


		size t ;;
*)


let is_unique (l,e) =   List.mem e l ;; (* a list * a -> bool   checks for e  in list l *)

let add_if_not (e,l) =  if is_unique(l,e) then l else e::l;; (*Add if not present in the list*)


let union(l1,l2) = match l1,l2 with  (* a list * alist -> a list*)
	l1,[] -> l1|
	[],l2 -> l2|
	l1,l2 -> foldl add_if_not l1 l2;; (*Does the union of the input lists using add_if_not function and l1 as the accumulator*)



let rec vars t = match t with (* term -> variable list*)
	V x -> [x]|
	Node (s,[]) -> []|
	Node (s,l) -> foldl union [] (map vars l);;

(*	****Testcases****
	let  t = (Node (Sym "Sum",[V(Var "x");Node(Sym "Sum",[V (Var "y");V (Var "z")])]));;([Var "x"; Var "y"; Var "z"])
   	let  t = (Node (Sym "Sum",[V(Var "y");Node(Sym "Sum",[V (Var "y");V (Var "z")])]));; ([Var "y"; Var "z"]) 
   	let  t = (Node (Sym "Sum",[Node(Sym "Zero",[]);Node(Sym "Sum",[V (Var "y");V (Var "z")])]));; ([Var "y"; Var "z"])
   	let  t = (Node (Sym "Zero",[]));; ([])
   	let  t = (Node (Sym "Sum",[Node (Sym "Zero",[]);V (Var "x")]));; ([Var "x"])


		vars t ;;
*)



(*Represent  Substitution as a list of pairs  [(V (Var "x"),V(Var "y"))] // x is substituted as y
	Identity substitution is represented as an empty list 
	*)


let rec find_in_table sub_list element   = match sub_list  with
					 [] -> []|
					 (e1,e2)::xs ->if ((e1=V  element) && e1!=e2 ) then [(e1,e2)] else  find_in_table xs element ;;
					

(*let find_in_table table element l(*   V x *       *) = 
					try ( let x = Hashtbl.find table element  in (*fisrt find in the list then if there then find in the table*)
						 	if (V element = x) then l else (* returns an empty list if the substitut(x) -> x else the new term*)
						  (V element,x)::l)
				    with Not_present -> l ;;*)

let hd_2 l = match l with (*'a * 'b) list -> 'b*)
	[] -> raise Not_present|
	(e1,e2)::xs -> e2;;


let rec subst sub_list t  = match t with (* 'a -> term -> term  *)
			V y -> if (length (find_in_table sub_list y ) = 0) then V y else (hd_2 (find_in_table sub_list y ))| (* if substitution causes a change then the change otherwise the same term*)
			Node (s,[]) -> Node(s,[])| (* Constants cant be substituted*)
			Node (s,l) -> Node (s,(map (subst sub_list) l));;(*Recuresively called on the subterms*)


(*	****Testcases****
	let  t = (Node (Sym "Sum",[V(Var "x");Node(Sym "Sum",[V (Var "y");V (Var "z")])]));;(Node (Sym "Sum",
 		[Node (Sym "Zero", []);
 			Node (Sym "Sum",
   				[V (Var "y");
    				Node (Sym "Sum", [Node (Sym "Zero", []); Node (Sym "Zero", [])])])]))
    subst s t ;;
    let s = [] ;;
   	let  t = V (Var "y");; (V (Var "y")) 
   	 let s =  [(V (Var "x"),V (Var "y"));(V (Var "y"),V (Var "x"))];;

	let  t = (Node (Sym "Zero",[]));; (Node (Sym "Zero",[]))
   	let  t = (Node (Sym "Sum",[Node (Sym "Zero",[]);V (Var "x")]));; (Node (Sym "Sum", [Node (Sym "Zero", []); Node (Sym "Zero", [])]))


		subst substitut t ;;
		subst s t ;;
*)

exception NOT_UNIFIABLE;;

let rec make_subst sub_list1 l = match sub_list1,l with (* ('a * 'b) list -> 'a list -> ('a * 'a) list*)
		[],[] -> []|
		(e1,e2)::xs,y::ys -> if (e1=y) then make_subst xs ys else (e1,y):: make_subst xs ys |
		_ -> raise NOT_UNIFIABLE;;

let rec new_lis first2 second_list = match second_list with  (* 'a list -> ('a * 'b) list -> ('a * 'b)*)
			[]-> []|
			(e1,e2)::xs -> if (List.mem e1 first2) then new_lis first2 xs else  (e1,e2)::new_lis first2 xs;;


let compose sub_list1 sub_list2 =  (*(term * term) list -> (term * term) list -> (term * term) list*)
		let l = map (subst sub_list2) (get_second sub_list1) in 
			let  p = make_subst sub_list1 l in 
			 let m = get_second sub_list1 in 
					let n = new_lis m sub_list2 in
						p@n;;



let rec reduce f l1 l2 = match l1,l2 with  (*('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
	l1,[] -> raise NOT_UNIFIABLE|
	[],l2 -> raise NOT_UNIFIABLE|
	x::xs,y::ys -> (f x y )::(reduce  f xs ys);; 


(*let table = Hashtbl.create 100000;;*)

let rec foldfl f1 f2 sub_list l m= match l,m with (*('a * 'a -> (term * term) list) ->
  ((term * term) list -> 'b -> 'a) ->
  (term * term) list -> 'b list -> 'b list -> (term * term) list*)
	[],[] -> sub_list|
	x::xs,y::ys -> foldfl f1 f2 (compose  (f1 (f2 sub_list x,f2 sub_list y)) sub_list  )xs ys |
	_ -> raise NOT_UNIFIABLE;;


let rec mgu (t1,t2) = match t1,t2 with (*term * term -> (term * term) list *)
	V (Var k) , V (Var l) -> if (k=l) then [] else [(V (Var k) , V (Var l))]|
	V (Var k) , Node(s,[]) -> [(V (Var k) , Node(s,[]))]|
	Node(s,[]),V (Var k)  -> [(V (Var k) , Node(s,[]))]|
	V (Var k) , Node(s,l) -> (if List.mem (Var k)  (foldl union [] (map vars l))
								then raise NOT_UNIFIABLE else [(V (Var k) , Node(s,l))])|
	Node(s,l),V (Var k)  -> (if List.mem (Var k)  (foldl union [] (map vars l))
								then raise NOT_UNIFIABLE else [(V (Var k) , Node(s,l))])|
	Node (s1,[]),Node(s2,[]) -> if (s1=s2) then [] else raise NOT_UNIFIABLE |
	Node (s,[]),Node(s1,l) -> raise NOT_UNIFIABLE|
	Node (s1,l1),Node(s2,l2) ->  if (length l1 = length l2 && s1 = s2)  then foldfl mgu subst [] l1 l2 else raise NOT_UNIFIABLE ;;
(* Testcases 
									
	1. t1 = V (Var "x")  & t2 =  V (Var "x")
		mgu (t1,t2) = []

	2. t1 = V (Var "x")  & t2 =  V (Var "y")
		mgu (t1,t2) = [(V (Var "x"), V (Var "y"))]

	3. t1 = V (Var "x")  &  t2 = Node (Sym "Zero", [])
		mgu (t1,t2) = [(V (Var "x"), Node (Sym "Zero", []))]

	4. t1 = V (Var "x")  &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])])	// vars t2 doesnt contain x 
		mgu (t1,t2) =	[(V (Var "x"), Node (Sym "Sum", [Node (Sym "Zero", []); Node (Sym "Zero", [])]))]
	
	5. t1 = V (Var "x")  &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);V (Var "y")])   			// vars t2 doesnt contain x 
			mgu (t1,t2)  =	[(V (Var "x"), Node (Sym "Sum", [Node (Sym "Zero", []); V (Var "y")]))]

	6. t1 = V (Var "x")  &  t2  = Node (Sym "Sum", [V (Var "z");V (Var "y")])
			mgu (t1,t2)  =	[(V (Var "x"), Node (Sym "Sum", [V (Var "z"); V (Var "y")]))]

	7. t1 = V (Var "x")  &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);V (Var "x")])    // vars t2 contains x 
			mgu (t1,t2)  =	Exception: NOT_UNIFIABLE.

	8. t1 = Node (Sym "Zero", []) & t2 = Node (Sym "Zero", [])  // two same constants
			mgu (t1,t2)  = []

	9. t1 = Node (Sym "Zero", []) & t2 = Node (Sym "One", []) 	// two different constants
			mgu (t1,t2)  =  Exception: NOT_UNIFIABLE.
	
	10. t1 = Node (Sym "Zero", []) &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])])  // // constant and a  k-ary symbol
			mgu (t1,t2)  =  Exception: NOT_UNIFIABLE.

	11. t1  = Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])]) &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])])
			mgu (t1,t2)  = []   		// two same k-ary symbols

	12. t1  = Node (Sym "Pred", [Node (Sym "Zero", [])]) &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])])
			mgu (t1,t2)  =  Exception: NOT_UNIFIABLE. // k-ary and m-ary symbols	

	13. t1  = Node (Sym "Sum", [Node (Sym "Zero", []);V (Var "x")]) &  t2  = Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])])
						mgu (t1,t2) = [(V (Var "x"), Node (Sym "Zero", []))] 

	14. t1 = Node (Sym "Sum",[Node (Sym "Sum", [Node (Sym "Zero", []);Node (Sym "Zero", [])]);Node (Sym "Sum", [V (Var "x");V (Var "z")])]) 
		t2 = Node (Sym "Sum",[Node (Sym "Sum", [V (Var "x");Node (Sym "Zero",[])]);Node (Sym "Sum", [Node (Sym "Zero", []);V (Var "x")])]) 
					mgu (t1,t2) = [(V (Var "z"), Node (Sym "Zero", [])); (V (Var "x"), Node (Sym "Zero", []))]		

	15.	let t1 = Node(Sym "Sum",[V (Var "z");Node(Sym "Sum",[Node(Sym "ZERO",[]);V(Var "x")])]);;
		let t2 = Node(Sym "Sum",[V(Var "y");Node(Sym "Sum",[V(Var "y");Node (Sym "One",[])])]);;
		let p = mgu t1 t2 	 = [(V (Var "x"), Node (Sym "One", [])); (V (Var "y"), Node (Sym "ZERO", []));
 (V (Var "z"), V (Var "y"))]


		 *)

let rec print_vars v = match v with
	[] -> Printf.printf "\n"|
(Var x)::l -> Printf.printf "Var %s ;"(x);print_vars(l);;



let sig1 = [(Sym("X"),0);(Sym("Y"),0);(Sym("f"),1);(Sym("g"),2);(Sym("h"),3);(Sym("*"),2)];;
let sig2 = [(Sym("X"),0);(Sym("Y"),0);(Sym("Z"),0);(Sym("f"),1);(Sym("g"),2);(Sym("f"),3);(Sym("*"),2)];;
let sig3 = [(Sym("f"),1)];;
let sig4 = [(Sym("X"),0);(Sym("Y"),0);(Sym("Z"),0)];;

let term1 = (Node (Sym("f"),[V(Var( ("X")))]));;
let term2 = (Node (Sym("g"),[V(Var( ("X")));Node(Sym("h"),[Node(Sym("f"),[V(Var( ("X")))]);V(Var( ("Y")))])]));;
let term3 = (Node (Sym("g"),[V(Var( ("X")));Node(Sym("*"),[V(Var( ("Y")));Node (Sym("*"),[V(Var( ("X")));V(Var( ("Y")))])])]));;
let term4 = (Node (Sym("g"),[V(Var(("X")));Node(Sym("*"),[V(Var( ("Y")));V(Var( ("X")))])]));;
let term5 = (Node (Sym("g"),[V(Var( ("Z")));Node(Sym("*"),[V(Var( ("X")));V(Var( ("Z")))])]));;
let term6 = (Node (Sym("g"),[V(Var( ("Z")));Node(Sym("g"),[V(Var( ("X")));V(Var( ("Z")))])]));;
let term7 = V (Var "X");;
let term8 = (Node (Sym("K"),[]));;
let term9 = (Node (Sym("X"),[]));;
let term10 = (Node (Sym("g"),[V(Var( ("X")));Node(Sym("h"),[Node(Sym("f"),[V(Var( ("X")))]);V(Var( ("Y")));Node (Sym("X"),[])])]));;
let term11 = (Node (Sym("g"),[V(Var( ("X")));Node(Sym("h"),[Node(Sym("f"),[V(Var( ("X")))]);V(Var( ("Y")));Node (Sym("f"),[V(Var( ("X")))])])]));;
let term12 = (Node (Sym("g"),[V(Var( ("Z")));Node(Sym("*"),[V(Var( ("Z")));Node (Sym("*"),[V(Var( ("X")));V(Var( ("Y")))])])]));;
let term13 = (Node (Sym("$"),[V(Var( ("P")));V(Var(("Q")))]));;
let term14 = (Node (Sym("$"),[Node (Sym("2"),[]); Node (Sym("4"),[])]));;
let term15 = (Node (Sym("$"),[Node (Sym("2"),[]); Node (Sym("3"),[])]));;

let symbols1 = get_first(sig1);;
let arities1 = get_second (sig1);;

let symbols2 = get_first(sig2);;
let arities2 = get_second (sig2);;

let symbols3 = get_first(sig3);;
let arities3 = get_second (sig3);;

let symbols4 = get_first(sig4);;
let arities4 = get_second (sig4);;



Printf.printf "(1)check_sig sig1 : %B\n" (check_sig sig1);;
Printf.printf "(2)check_sig sig2 : %B\n" (check_sig sig2);;
Printf.printf "(3)check_sig sig3 : %B\n" (check_sig sig3);;
Printf.printf "(4)check_sig sig4 : %B\n\n" (check_sig sig4);;

Printf.printf "(5)wfterm term1 sig1 : %B\n" (wfterm symbols1 arities1 term1 );;
Printf.printf "(6)wfterm term2 sig1 : %B\n" (wfterm symbols1 arities1 term2 );;
Printf.printf "(7)wfterm term7 sig4 : %B\n" (wfterm symbols4 arities4 term7 );;
(*  Printf.printf "(8)wfterm term8 sig4 : %B\n" (wfterm symbols4 arities4 term8 );;
 *)Printf.printf "(9)wfterm term9 sig4 : %B\n\n" (wfterm symbols4 arities4 term9 );; 

Printf.printf "(10)ht term9 : %d\n" (ht term9);;
Printf.printf "(11)ht term7 : %d\n" (ht term7);;
Printf.printf "(12)ht term4 : %d\n" (ht term4);;
Printf.printf "(13)ht term10 : %d\n" (ht term10);;
Printf.printf "(14)ht term11 : %d\n\n" (ht term11);;

Printf.printf "(15)size term9 : %d\n" (size term9);;
Printf.printf "(16)size term7 : %d\n" (size term7);;
Printf.printf "(17)size term4 : %d\n" (size term4);;
Printf.printf "(18)size term10 : %d\n" (size term10);;
Printf.printf "(19)size term11 : %d\n\n" (size term11);;

Printf.printf "(20)vars term9 : ";;print_vars(vars term9);; Printf.printf("\n");;
Printf.printf "(21)vars term7 : ";;print_vars(vars term7);; Printf.printf("\n");;
Printf.printf "(22)vars term4 : ";; print_vars(vars term4);; Printf.printf("\n");;
Printf.printf "(23)vars term10 : ";; print_vars(vars term10);; Printf.printf("\n");;
Printf.printf "(24)vars term11 : ";; print_vars(vars term11);; Printf.printf("\n\n");;


Printf.printf "(31)mgu term14 term13 : ";; (mgu (term14 ,term13));; Printf.printf("\n");;
Printf.printf "(33)mgu term3  term12 : ";; (mgu (term3 ,term12));; Printf.printf("\n");;
Printf.printf "(34)mgu term12 term3  : ";; (mgu (term12, term3));; Printf.printf("\n\n");;

Printf.printf "(33.1)subst term12 (mgu term3 term12)  : ";; (subst  (mgu (term3 ,term12)) term12);; Printf.printf("\n");;
Printf.printf "(33.2)subst term3  (mgu term3 term12)  : ";; (subst (mgu (term3 ,term12))  term3 );; Printf.printf("\n\n");;

Printf.printf "(34.1)subst term12 (mgu term12 term3)  : ";; (subst (mgu (term12, term3)) term12 );; Printf.printf("\n");;
Printf.printf "(34.2)subst term3  (mgu term12 term3)  : ";; (subst (mgu (term12, term3))  term3 );; Printf.printf("\n\n");;






		
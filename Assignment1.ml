(*Assumptions :-
	
	1. A data type a_rep is made to represent editable strings .
	2. The function create takes as argument a string over any given Alphabet ; i.e. the string given as input to create  is of that alphabet .Alphabet is the nowhere taken as input separately.
	3. Then all other functions take in as argument a_rep data type . 
	4. The Marker is the centre string in the Nod which stores the editable part of the editable string.
*)


type a_rep = Nil| Nod of string*string *string*string*string ;;			(*functional data type to represent editable string*)

(* type a_rep represents strings as Nil -> "" |  Nod(first_letter,letters before_marker , marker , letters after_marker , last_letter )*)

let lgh s  = match s with 						(* returns non - negative integer length of the editable string ; type  a_rep -> int *)
  	Nil -> 0|  
  	Nod    (p,a,b,c,q) -> (String.length a)+(String.length b) + (String.length c)  ;;
  							(*length of  before_marker + length of marker + length of after_marker *)

let nonempty s = match s with  (* a_rep ->  bool *)
  	Nil -> false |
  	Nod(a,b,c,d,e) -> true ;; 


let concat n1 n2  = match n1 ,n2 with (* a_rep -> a_rep -> a_rep    O(max((lgh n1) , (lgh n2))*)   
		Nil ,pi -> pi|
		qw , Nil -> qw|
		Nod(p,a,b,c,d) , Nod(r,s,t,u,v) -> 		
				Nod(p,"",p,String.sub (a^b^c^s^t^u) 1 ((String.length (a^b^c^s^t^u))-1),v);;

(* 	Prove that lgh(concat s1 s2)= lgh (s1) + lgh(s2) 
		Induction on lgh of s
		Base case : 
			lgh (s1) = lgh s2 = 0
			s1 , s2 both are Nil 
			concat Nil Nil - > Nil 	
			lgh (Nil) = 0 
			= lgh (s1) + lgh(s2) = 0+0 = 0 
			Hence Base case is  true
	
		Induction Hypothesis : Let the equation be true for editable strings of lgh s <= k 
		
		Induction Step :
			Let lgh (s1) >  k
				then s1 can be written as lgh (concat s1' s2') where s1' and s2' are strings of length <=k
				concat s1 s2 -> an editable string which is the general concatenation of two string s1 ,s2  
				lgh (concat s1 s2) =  (lgh (s1')+ lgh (s2')) + lgh (s2)  using associativity of the general '+' sign .
				As s1' s2' s2 are editable strings of length<= k hence the equation is true for all lenths of editable strings . 	
*)

let rec revers s = match s with 
        		 "" -> ""|
				p->(String.sub p  ((String.length p)-1) 1)^(revers (String.sub p 0 ((String.length p)-1)))        ;;


 let  reverse n = match n with  (* a_rep -> a_rep Complexity O(lgh n)*)
  	 Nil -> Nil|Nod(d,a,b,c,e) -> Nod(e,"",String.sub c ((String.length c)-1) 1,revers(a^b^(String.sub c 0 ((String.length c)-1))),d);;    


  	 (*
		Prove that lgh(reverse s) = lgh s 

				Induction on lgh s
				Base case : lgh s = 0
				s= Nil
				reverse s = Nil ; lgh Nil = lgh Nil  Hence Base case is  true;;

				Induction Hypothesis : Let the equation be true for editable strings of lgh s  <= k 				

				Induction Step :
					Let lgh (s1) =  k+1
					then s1 can be written as lgh (concat s1' s2') where lgh s1' = k and lgh s2' = 1  the last character of editable string;s1' and s2 'are strings of length <=k
					Now  lgh s1' = lgh (reverse s1') lgh s2' = lgh (reverse s2')

					lgh s1' +lgh s2' = lgh (reverse s2') +lgh (reverse s1') = lgh (concat (reverse s2') (reverse s1')
					lgh s1 = lgh (reverse s1)
		
				 hence the equation is true for all lenths of editable strings .
  	 *)

exception Empty
let first n = match n with  (*a_rep -> string   O(1) *)

	 Nil -> raise Empty |
	Nod(a,b,c,d,e) -> a;;



let last n = match n with  	(*  a_rep -> string  O(1) *)
	 Nil -> raise Empty |
	Nod(a,b,c,d,e) -> e;;


let create s  = match s with  (* string -> a_rep *)
  	""->Nil| 
    s->Nod((String.sub s 0 1),"",(String.sub s 0 1),(String.sub s 1 ((String.length s)-1)),String.sub s ((String.length s)-1) 1);;

exception AtLast  ;; 
exception AtFirst ;;


let forward n = match n with   (* a_rep -> a_rep    O(n)  *)
	Nil ->Nil |
	Nod(p,q,r,"",t)-> raise AtLast|
	Nod(a,b,c,d,e) -> Nod(a,b^c,String.sub d 0 1 , String.sub d 1 ((String.length d)-1),e);;



let back n = match n with (* a_rep -> a_rep    O(n)  *)
	Nil ->Nil |               
	Nod(p,"",r,s,t)-> raise AtFirst|
	Nod(a,b,c,d,e) -> Nod(a,String.sub b 0 ((String.length b)-1),String.sub b ((String.length b)-1) 1 , c^d,e);; 


exception TooShort ;;

let moveTo n s = match n,s with  (*int -> a_rep -> a_rep   O(lgh s) *)
	n,Nil -> Nil|
	n,Nod(a,b,c,d,e) -> if n>=( lgh (Nod(a,b,c,d,e))) then raise TooShort else Nod(a,String.sub (b^c^d) 0 n,String.sub (b^c^d) n 1,String.sub (b^c^d) (n+1) ((String.length  (b^c^d))-n-1),e);;



let replace w s = match w,s with 		(* string -> a_rep -> a_rep    O(1)*)
	w,Nil -> Nil |
	w,Nod(a,b,c,d,e) -> Nod(a,b,w,d,e);;

(*
	Prove that lgh (replace w s) = lgh s
	Induction on lgh s
				Base case : lgh s = 1
				Inthis case s contains only a single letter this  when replaced by w gives a single letter w 
				hence lgh (replace  w s ) = 1 

				Hence Base case is true .

				Induction Hypothesis : Let the equation be true for editable strings s with  (lgh s)  <= k 	

				Induction Step :
					lgh (s ) = lgh (concat (before_marker) (concat marker after_marker))
									   =  (lgh before_marker) + (lgh marker) + (lgh after_marker)
									   	All  before_marker , marker , after_marker are edt_str with lgh <=k
									   = (lgh before_marker) + (lgh w) + (lgh after_marker)  :: From Base Case
									   = (lgh (replace w s ));;
				hence the equation is true for all lenths of editable strings .

*)
let alphabet=[Nod("1","","1","","1"); Nod("2","","2","","2"); Nod("a","","a","","a");Nod("b","","b","","b"); Nod("c","","c","","c"); Nod("A","","A","","A")];;


let print_nod nod = match nod with
	Nil -> Printf.printf"%s"("\n")|
	Nod(a,b,c,d,e) -> Printf.printf"%s"(b);Printf.printf"%s"(c);Printf.printf"%s\n"(d);;

	let print_nod_marker_indicated nod = match nod with
	Nil -> Printf.printf"%s"("\n")|
	Nod(a,b,c,d,e) -> Printf.printf"%s"(b);Printf.printf"*%s*"(c);Printf.printf"%s\n"(d);;




Printf.printf"%d\n"(lgh Nil);;
Printf.printf"%d\n"(lgh (Nod("a","","a","","a")));;
Printf.printf"%d\n"(lgh (Nod("a","","a","bc","c")));;
Printf.printf"%d\n"(lgh (Nod("1","","1","2","2")));;Printf.printf"%b\n"(nonempty Nil);;
Printf.printf"%b\n"(nonempty (Nod("a","","a","","a")));;
Printf.printf"%b\n"(nonempty (Nod("1","","1","2","2")));;
print_nod(concat Nil Nil);;
print_nod(concat Nil (Nod("a","","a","","a")));;
print_nod(concat (Nod("1","","1","","1")) Nil);;
print_nod(concat (Nod("1","","1","A","A")) (Nod("a","","a","bc","c")));;
print_nod(reverse Nil);;
print_nod(reverse (Nod("a","","a","bc","c")));;
print_nod(reverse (Nod("1","","1","2","2")));;
(* Printf.printf"%s\n"(first Nil);;
 *)Printf.printf"%s\n"(first (Nod("a","","a","","a")));;
Printf.printf"%s\n"(first (Nod("a","","a","bc","c")));;
(* Printf.printf"%s"(last Nil);;
 *)Printf.printf"%s\n"(last (Nod("a","","a","","a")));;
Printf.printf"%s\n"(last (Nod("a","","a","bc","c")));;
print_nod_marker_indicated(forward editable);;
print_nod_marker_indicated(back editable);;

print_nod_marker_indicated(moveTo 10 editable);;
print_nod_marker_indicated(replace "b" editable);; 




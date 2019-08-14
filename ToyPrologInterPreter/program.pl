edge(a,b).
edge(b,c).


likes(joe,books).
likes(joe,mary).
likes(mary,books).
likes(john,books).
likes(sue,joe).
likes(john,mary).
likes(mary,joe).
likes(mary,movies).

likes(bill,X) :- likes(X,books), likes(X,movies).
likes(alice,X) :- likes(X,mary).

parent(fred, sally).
parent(tina, sally).
parent(sally, john).
parent(sally, diane).
parent(sam, bill).
ancestor(bob, susan).
ancestor(A, X) :- parent(A, X).
ancestor(A, X) :- parent(A, C), ancestor(C, X).


connected(X,Y) :- connectedTo(X,Y).
connected(X,Y) :- connectedTo(Y,X).

triple(X,Y,Z) :- connected(X,Y), connected(Y,Z), connected(Z,X).
tripleWith1T(X,Y,Z) :- triple(X,Y,Z), startsWithT(X).

triples(L) :- findall([X,Y,Z],tripleWith1T(X,Y,Z),L).
triplesSet(S) :- triples(L), maplist(sort(),L,L1),sort(L1,L2),list_to_set(L2,S).
solve1(X) :- triplesSet(S),length(S,X).

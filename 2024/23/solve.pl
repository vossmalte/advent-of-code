connected(X,Y) :- connectedTo(X,Y).
connected(X,Y) :- connectedTo(Y,X).

triple(X,Y,Z) :- connected(X,Y), connected(Y,Z), connected(Z,X).
tripleWith1T(X,Y,Z) :- triple(X,Y,Z), startsWithT(X).

triples(L) :- findall([X,Y,Z],tripleWith1T(X,Y,Z),L).
triplesSet(S) :- triples(L), maplist(sort(),L,L1),sort(L1,L2),list_to_set(L2,S).
solve1(X) :- triplesSet(S),length(S,X).

connectsToAll(_, []).
connectsToAll(Y,[X|R]) :- connected(X,Y), connectsToAll(Y,R).

network([]).
network([X|R]) :- connectsToAll(X,R), network(R).

isSorted(L) :- sort(L,L).

nw(1,[_]).
nw(2,[X,Y]) :- connected(X,Y), isSorted([X,Y]).
nw(S,[X|R]) :- SN is S - 1, nw(SN,R), isSorted(R), connectsToAll(X,R).

degree(X,Degree) :- aggregate_all(count,connected(X,_),Degree).
% ^ magic 13
solve2(L) :- nw(13,L1), sort(L1,L).

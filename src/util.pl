empty_list([]).
get_same(A, A).

% Jätab listi Count arv elemetne, lõikab ära viimased
splice([], _, []) :- !.
splice(_, C, []) :- C < 1, !.
splice([H|_], 1, [H]):-!.
splice([H|T], Count, [H|NewList]):-
    C is Count - 1,
    splice(T, C, NewList).
    
% Eemaldab listi algusest Count arv elemente
splice_begin([], _, []).
splice_begin([H|T], Count, NewList):-
	((Count < 1), get_same([H|T], NewList), !);
	NewCount is Count - 1,
	splice_begin(T, NewCount, NewList).

% Sorteerib tipud hinna järgi kasvavalt
sort_by_cost(List,Sorted):-
	insert_sort(List,[],Sorted).
insert_sort([],Acc,Acc).
insert_sort([H|T],Acc,Sorted):-
	insert(H,Acc,NAcc),
	insert_sort(T,NAcc,Sorted).
insert([N,X],[[N2,Y]|T],[[N2,Y]|NT]):-
	X >= Y,
	insert([N,X],T,NT),
	!.
insert([N,X],[[N2,Y]|T],[[N,X],[N2,Y]|T]):-
	X =< Y.
insert([N,X],[],[[N,X]]).

% Leiab keskmise hinna, nulliga jagades annab lõpmatuse
avg(_, 0, 999999):-!.
avg(Cost, Count, Avg):-
    Avg is Cost / Count.
    
% Lõikab ära kõik lapsed mis ületavad MaxCosti
prune_by_cost([], _, []) :- !.
prune_by_cost([[Child, Cost]|T], MaxCost, [[Child, Cost]|NewList]):-
    Cost =< MaxCost,
    prune_by_cost(T, MaxCost, NewList).
prune_by_cost([[_, Cost]|T], MaxCost, NewList):-
    Cost > MaxCost,
    prune_by_cost(T, MaxCost, NewList).

% Teeb tippude lastest listi
merge_children([], []).
merge_children([[Node, _]|Rest], Append):-
    tree(Node, _, Children),
    merge_children(Rest, MergedChildren),
    append(Children, MergedChildren, Append).
merge_children([[Node, _]|Rest], MergedChildren):-
    not(tree(Node, _, _)), %pole lapsi (on leht)
    merge_children(Rest, MergedChildren).

% Leiab kombinatsioonid 1..N Listist
combinations(List, Combinations):-
    length(List, Length),
    combinations(Length, List, Combinations).
    
combinations(0, _, []).
combinations(N, List, Combinations):-
    findall(C, combination(N, List, C), NCombis),
    N > 0,
    N1 is N - 1,
    combinations(N1, List, RestCombis),
    append(NCombis, RestCombis, Combinations).

combination(0, _, []).
combination(N, [H|T], [H|Comb]):-
	N > 0,
	N1 is N - 1,
	combination(N1, T, Comb).
combination(N, [_|T], Comb):-
	N > 0,
	combination(N, T, Comb).
    
   % Names = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,q],
a(Children):-
    writeln('A'),
    random(0, 4, Rnd),
    b(Rnd, Children).
b(0, []):-!.
b(Count, [Name|Rest]):-
    a(Children),
    writeln('B'),
    gensym(node_, Name),
    Child = tree(Name, 0, Children),
    writeln(Child),
  	C is Count - 1,
  	b(C, Rest).

empty_list([]).
get_same(A, A).

% Jätab listi Count arv elemetne, lõikab ära viimased
splice([], _, []) :- !.
splice(_, C, []) :- C < 1, !.
splice([H|_], 1, [H]):-!.
splice([H|T], Count, [H|NewList]):-
    C is Count - 1,
    splice(T, C, NewList).

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
    
prune_by_cost([], _, []) :- !.
prune_by_cost([[Child, Cost]|T], MaxCost, [[Child, Cost]|NewList]):-
    Cost =< MaxCost,
    prune_by_cost(T, MaxCost, NewList).
prune_by_cost([[_, Cost]|T], MaxCost, NewList):-
    Cost > MaxCost,
    prune_by_cost(T, MaxCost, NewList).

merge_children([], []).
merge_children([[Node, _]|Rest], Append):-
    tree(Node, _, Children),
    merge_children(Rest, MergedChildren),
    append(Children, MergedChildren, Append).
merge_children([[Node, _]|Rest], MergedChildren):-
    not(tree(Node, _, _)), %pole lapsi (on leht)
    merge_children(Rest, MergedChildren).

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

combinations(-1, _, []).
combinations(N, List, Combinations):-
    findall(C, combination(N, List, C), NCombis),
    N > -1,
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

gen:-
	reset_gensym(el_),
	a(_, 0);
	!.
a(_, 0):-
	b(1, 0, Children).
a([], Depth):-
	Depth >= 10, !.
a(Children, Depth):-
    random(0, 4, Rnd),
    b(Rnd, Depth, Children).
	
b(0, _, []):-!.
b(Count, Depth, [Name|Rest]):-
	NextDepth is Depth + 1,
    a(Children, NextDepth),
    gensym(el_, Name),
	add_random_prices(Children, PricedChildren),
    Child = tree(Name, Depth, PricedChildren),
    writeln(Child),
	length(Children, L),
	writetree(Child, L),
	C is Count - 1,
	b(C, Depth, Rest).
	
	
add_random_prices([], []).
add_random_prices([H|T], [[H, RndPrice]|Rest]):-
	random(1, 10, RndPrice),
	add_random_prices(T, Rest).
	
writetree(_, 0):-!.
writetree(Tree, _):-
    open('gen_tree.txt', append, OS),
    write(OS, Tree),
    write(OS, '.\n'),
    close(OS).
	
load_tree:-
    open('gen_tree_1.txt', read, Stream),
    read_file(Stream, Lines),
    close(Stream).

read_file(Stream, []) :-
    at_end_of_stream(Stream).
read_file(Stream, [H|T]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, H),
	assert(H),
    read_file(Stream, T).
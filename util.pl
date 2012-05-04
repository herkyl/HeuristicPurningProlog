empty_list([]).
get_same(A, A).

% Jätab listi Count arv elemetne, lõikab ära viimased
splice([], _, []) :- !.
splice(_, C, []) :- C < 1, !.
splice([H|_], 1, [H]):-!.
splice([H|T], Count, [H|NewList]):-
    C is Count - 1,
    splice(T, C, NewList).

sort_by_cost(List,Sorted):-
	insert_sort(List,[],Sorted).
insert_sort([],Acc,Acc).
insert_sort([H|T],Acc,Sorted):-
	insert(H,Acc,NAcc),
	insert_sort(T,NAcc,Sorted).
insert([N,X],[[N2,Y]|T],[[N2,Y]|NT]):-
	X > Y,
	insert([N,X],T,NT).
insert([N,X],[[N2,Y]|T],[[N,X],[N2,Y]|T]):-
	X =< Y.
insert([N,X],[],[[N,X]]).
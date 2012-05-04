% tree(Root, Distance, List_of_leaves).
:- dynamic tree/3.
:- dynamic pruned_tree/3.

tree(root, 0, [[a, 1], [b, 1]]).
tree(a, 1, [[c, 1], [d, 1]]).
tree(b, 1, [[e, 1]]).
tree(c, 2, []).
tree(d, 2, []).
tree(e, 2, [[x, 1], [y, 1], [z, 1], [u, 1], [v, 2]]).


% PEA PREDIKAAT - jooksutab programmi
prune_main:-
    retractall(pruned_tree(_, _, _)),
    compile('util.pl'),
    compile('prune.pl'),
    calculate_tree(Cost, Count),
    writeln(['Cost:', Cost, 'Count:', Count]),
	ignore(dfs([[root, 0]])),
    calculate_pruned_tree(Cost2, Count2),
    writeln(['Cost:', Cost2, 'Count:', Count2]).

% Depth first search
dfs([[Child, Cost]|Rest]):-
	write(Child), nl,
	tree(Child, Depth, C),
	prune(C, Depth, Children),
	assert(pruned_tree([Child, Cost], Depth, Children)),
	dfs(Children);
	dfs(Rest).

% Level order search
los([[[Child, _], _]|Rest]):-
	write(Child), nl,
	los(Rest),
	!.
los([[Child, _]|_]):-
	write(Child), nl,
	tree(Child, _, Children),
	los(Children).

% Arvutab puu suuruse ja hinna
calculate_tree(Cost, Count):-
    findall(X, tree(_, _, X), Nodes),
    calculate_list_of_children(Nodes, Cost, Count).
calculate_pruned_tree(Cost, Count):-
    findall(X, pruned_tree(_, _, X), Nodes),
    calculate_list_of_children(Nodes, Cost, Count).
calculate_list_of_children([], 0, 0).
calculate_list_of_children([List|T], Cost, ChildrenCount):-
    calculate_children(List, C),
    calculate_list_of_children(T, C2, CC),
    length(List, Length),
    ChildrenCount is CC + Length,
    Cost is C + C2.
calculate_children([], 0).
calculate_children([[_, CC]|T], Cost):-
    calculate_children(T, TailCost),
    Cost is CC + TailCost.
% tree(Root, Distance, List_of_leaves).
:- dynamic tree/3.
:- dynamic pruned_tree/3.

tree(root, 0, [[a, 2], [b, 1]]).
tree(a, 1, [[c, 1], [d, 1]]).
tree(b, 1, [[e, 1]]).
tree(c, 2, []).
tree(d, 2, []).
tree(e, 2, [[x, 2], [y, 1], [z, 1], [u, 1], [v, 2]]).


% PEA PREDIKAAT - jooksutab programmi
prune_main:-
    init,
    calculate_tree(Cost, Count),
    writeln(['Cost:', Cost, 'Count:', Count]),
	ignore(dfs([[root, 0]])),
    calculate_pruned_tree(Cost2, Count2),
    writeln(['Cost:', Cost2, 'Count:', Count2]).
    
init:-
    compile('util.pl'),
    compile('prune.pl'),
    retractall(pruned_tree(_, _, _)).

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
    
    
    
:- begin_tests(lists).
:- use_module(library(lists)).

test(splice) :-
	splice([1,2,3], 2, [1, 2]),
	splice([], 2, []),
	splice([], -1, []),	 
	splice([1, 2, 3], 0, [1, 2, 3]),
	splice([1, 2, 3], -1, [1, 2, 3]),
	splice([1, 2, 3], 5, [1, 2, 3]).
	
test(avg):-
    ignore(avg(0, 0, A)),
    A > 0,
    avg(1, 1, 1),
    avg(2, 4, 0.5).
    
test(prune_by_cost):-
    prune_by_cost([], 1, []),
    prune_by_cost([], -1, []),
    prune_by_cost([], 0, []),
    prune_by_cost([[a, 1], [b, 2], [c, 1]], 1, [[a, 1], [c, 1]]).

test(sort_by_cost):-
    sort_by_cost([[a, 3], [b, 2], [c, 1]], [[c, 1], [b, 2], [a, 3]]),
    sort_by_cost([[a, 3], [b, 2], [c, 4]], [[b, 2], [a, 3], [c, 4]]),
    sort_by_cost([[a, 3]], [[a, 3]]),
    sort_by_cost([], []).
    
test(static_depth_prune):-
    static_depth_prune([1, 2], 9999, []),
    static_depth_prune([], 9999, []),
    static_depth_prune([], -1, []),
    static_depth_prune([1, 2], -1, [1, 2]),
    static_depth_prune([1, 2], 0, [1, 2]),
    static_depth_prune([], 0, []).
    
test(static_children_prune):-
    static_children_prune([1, 2], _, [1]),
    static_children_prune([1, 2, 3, 4], _, [1]),
    static_children_prune([1], _, [1]),
    static_children_prune([], _, []).
    
test(static_children_half_prune):-
    static_children_half_prune([1, 2], _, [1]),
    static_children_half_prune([1, 2, 3], _, [1, 2]),
    static_children_half_prune([1], _, [1]),
    static_children_half_prune([], _, []).

:- end_tests(lists).
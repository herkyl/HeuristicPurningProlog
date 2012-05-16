% tree(Root, Distance, List_of_leaves).
:- dynamic tree/3.

%      ____root____ 
%     |            |
%  ___a___         b
% |       |        |
% c       d   _____e_____ 
%            |  |  |  |  |
%            x  y  z  u  v

tree(root, 0, [[a, 1], [b, 1]]).
tree(a, 1, [[c, 1], [d, 1]]).
tree(b, 1, [[e, 1]]).
tree(e, 2, [[x, 2], [y, 1], [z, 1], [u, 1], [v, 2]]).


% PEA PREDIKAAT - jooksutab programmi
prune_main:-
    init,
    prune_terms(Terms),
    prune_main(Terms, _, bfs),
    !.
    
run:-
    open('output.csv', write, OS),
    close(OS),
    prune_terms(Terms),
    combinations(Terms, Combis),
    run(Combis).
  
run([]):- !.  
run([Terms|Rest]):-
    writeln(['>>>', Terms]),
    prune_main(Terms, _, bfs),
    run(Rest).

prune_main(Terms, Pruned, SearchAlgo):-
    init,
    calculate_tree(Cost, Count),
    writeln(['>>>>>>>>>>>>>>>ALGO:', SearchAlgo, 'Cost:', Cost, 'Count:', Count]),
    ignore(call(SearchAlgo, [[root, 0]], Terms, Pruned)),
	%writeln(Pruned),
    calculate_pruned_tree(Pruned, Cost2, Count2),
    writerow(Terms, Cost2, Count2),
    writeln(['Cost:', Cost2, 'Count:', Count2]), 
    !.
    
prune_main_test(Terms, Pruned, SearchAlgo):-
    init,
    ignore(call(SearchAlgo, [[root, 0]], Terms, Pruned)).
    
writerow(Algos, Cost, Count):-
    open('output.csv', append, OS),
    write(OS, Algos),
    write(OS, ';'),
    write(OS, Cost),
    write(OS, ';'),
    write(OS, Count),
    write(OS, '\n'),
    close(OS).
    
init:-
    compile('util.pl'),
    compile('prune.pl'),
    retract(total_count(_)),
    retract(total_cost(_)),
    assert(total_count(0)),
    assert(total_cost(0)),
    retract(total_child_count(_)),
    retract(total_child_cost(_)),
    assert(total_child_count(0)),
    assert(total_child_cost(0)),
	retractall(tree(_, _, _)),
	load_tree.

% Depth-first search
dfs([], _, []).
dfs([[Child, Cost]|Rest], Terms, [[Child, Cost]|Append]):-
	tree(Child, Depth, C),
	ChildrenDepth is Depth + 1,
	prune(Terms, C, ChildrenDepth, Children),
	ignore(dfs(Children, Terms, ChildrenList)),
	ignore(dfs(Rest, Terms, RestList)),
	append(ChildrenList, RestList, Append).
dfs([[Child, Cost]|Rest], Terms, [[Child, Cost]|RestList]):-
	not(tree(Child, _, _)), % tegu on lehega
	ignore(dfs(Rest, Terms, RestList)).
	
% Breadth-first search
bfs(Level, Terms, Pruned):-
	bfs(Level, Terms, 0, Pruned), !.
bfs([], _, _, []):- !.
bfs(Level, Terms, 0, All):- % juurt ei kärbi
    merge_children(Level, NextLevel),
    bfs(NextLevel, Terms, 1, Rest),
    append(Level, Rest, All).
bfs(Level, Terms, Depth, All):-
    prune(Terms, Level, Depth, PrunedLevel),
    merge_children(PrunedLevel, NextLevel),
    NewDepth is Depth + 1,
    bfs(NextLevel, Terms, NewDepth, Rest),
    append(PrunedLevel, Rest, All).
	
% Arvutab puu suuruse ja hinna
calculate_tree(Cost, Count):-
    findall(X, tree(_, _, X), Nodes),
    calculate_list_of_children(Nodes, Cost, Count).
calculate_pruned_tree(Nodes, Cost, Count):-
    length(Nodes, Count),
    calculate_children(Nodes, Cost).
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
    sort_by_cost([[a,1], [b,1], [c,2], [d,0]], [[d, 0], [a, 1], [b, 1], [c, 2]]),
    sort_by_cost([[a,1], [b,1]], [[a, 1], [b, 1]]),
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
    
% PUU LÄBIMISE TESTID
    
%test(static_depth_prune):-
%    Terms = [static_depth_prune],
%    prune_main_test(Terms, List1, dfs),
%    List1 == [[root,0],[a,1],[c,1],[d,1],[b,1],[e,1]],
%    prune_main_test(Terms, List2, bfs),
%    List2 == [[root,0],[a,1],[b,1],[c,1],[d,1],[e,1]]. 
     
    
:- end_tests(lists).
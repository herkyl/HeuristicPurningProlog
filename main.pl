% el_tree(Root, Distance, List_of_leaves).
:- dynamic el_tree/3.

el_tree(root, 0, [[a, 1], [b, 1]]).
el_tree(a, 1, [[c, 1], [d, 1]]).
el_tree(b, 1, [[e, 1]]).
el_tree(c, 2, []).
el_tree(d, 2, []).
el_tree(e, 2, [[x, 1], [y, 1], [z, 1], [u, 1], [v, 1]]).

% PEA PREDIKAAT - jooksutab programmi
prune_main:-
    compile('util.pl'),
    compile('prune.pl'),
	dfs([[root, 0]]).

% Depth first search
dfs([[Child, _]|Rest]):-
	write(Child), nl,
	el_tree(Child, Depth, C),
	prune(C, Depth, Children),
	dfs(Children);
	dfs(Rest).

% Level order search
los([[[Child, _], _]|Rest]):-
	write(Child), nl,
	los(Rest),
	!.
los([[Child, _]|_]):-
	write(Child), nl,
	el_tree(Child, _, Children),
	los(Children).



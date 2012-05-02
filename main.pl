% el_tree(Root, Distance, List_of_leaves).
:- dynamic el_tree/3.

el_tree(root, 0, [a, b]).
el_tree(a, 1, [c, d]).
el_tree(b, 1, [e]).
el_tree(c, 2, []).
el_tree(d, 2, []).
el_tree(e, 2, [x, y, z, u, v]).

% root, a b, c d e, x q z u v

prune_main:-
	dfs([root]).

prune(Children,	Depth, R):-
	static_depth_trim(Children, Depth, B),
	static_children_half_trim(B, R).

% Depth first search
dfs([Child|Rest]):-
	write(Child), nl,
	el_tree(Child, Depth, C),
	prune(C, Depth, Children),
	dfs(Children);
	dfs(Rest).

% Level order search
los([Child|Rest]):-
	write(Child), nl,
	los(Rest),
	!.
los([Child|Rest]):-
	write(Child), nl,
	el_tree(Child, _, Children),
	los(Children).


% Meetod 1
% Lõikab ära harud, kui sügavus on üle max
static_depth_trim_max_depth(2).
static_depth_trim(Children, Depth, TrimmedChildren):-
	static_depth_trim_max_depth(MaxDepth),
	(	Depth < MaxDepth,
		get_same(Children, TrimmedChildren),
		!
	);
	empty_list(TrimmedChildren).

% Meetod 4.1
% Lõikab ära harud, kui neid on rohkem kui lubatud
static_children_trim_max_children(1).
static_children_trim(Children, TrimmedChildren):-
	static_children_trim_max_children(MaxChildren),
	length(Children, Count),
	TrimCount is Count - MaxChildren,
	splice(Children, TrimCount, TrimmedChildren).

% Meetod 4.2
% Lõikab ära pooled harud, ümardab ülesse
static_children_half_trim(Children, TrimmedChildren):-
	length(Children, Length),
	round(Length/2, Round),
	TrimCount is Length - Round,
	splice(Children, TrimCount, TrimmedChildren).

empty_list([]).
get_same(A, A).

% Eemaldab listist Count arv elemente
splice([], _, []).
splice([H|T], Count, NewList):-
	((Count < 1), get_same([H|T], NewList), !);
	NewCount is Count - 1,
	splice(T, NewCount, NewList).

% kas on mõistlik laadida kogu sügavus ära, ja siis otsustada selle
% järgi

% el_tree/3 ei ole antud tipu hinda

% el_tree(Root, Distance, List_of_leaves).
:- dynamic el_tree/3.

el_tree(root, 0, [a, b]).
el_tree(a, 1, [c, d]).
el_tree(b, 1, [e]).
el_tree(c, 2, []).
el_tree(d, 2, []).
el_tree(e, 2, [x, y, z, u, v]).


% Meetod 1
% Lõikab ära harud, kui sügavus on üle max
static_depth_trim_max_depth(1).
static_depth_trim(Node, TrimmedChildren):-
	el_tree(Node, Depth, Children),
	static_depth_trim_max_depth(MaxDepth),
	(	Depth < MaxDepth,
		get_same(Children, TrimmedChildren),
		!
	);
	empty_list(TrimmedChildren).

% Meetod 4.1
% Lõikab ära harud, kui neid on rohkem kui lubatud
static_children_trim_max_children(1).
static_children_trim(Node, TrimmedChildren):-
	el_tree(Node, _, Children),
	static_children_trim_max_children(MaxChildren),
	length(Children, Count),
	TrimCount is Count - MaxChildren,
	splice(Children, TrimCount, TrimmedChildren).

% Meetod 4.2
% Lõikab ära pooled harud, ümardab ülesse
static_children_half_trim(Node, TrimmedChildren):-
	el_tree(Node, _, Children),
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

% Arvamus selle kohta, kas on mÃµistlik laadida kogu sÃ¼gavus Ã¤ra, ja siis otsustada selle jÃ¤rgi

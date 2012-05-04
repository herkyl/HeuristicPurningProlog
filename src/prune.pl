% Annab kärpimis termide listi
prune_terms(Terms):-
	A = static_depth_trim,
	B = static_children_half_trim,
	Terms = [A, B].

% Children - kärpimata lapsed
% Depth - sügavus
% Trimmed - kärbitud laste list
prune(Children,	Depth, Trimmed):-
    prune_terms(PruneTerms),
	prune_rec(PruneTerms, Children, Depth, Trimmed).

% Rekursiivne meetod Termide kärpimis listi rakendamiseks
% Call/2+ kasutame =.. ja call/1 asemel
prune_rec([], Children, _, Children).
prune_rec([Term|Rest], Children, Depth, R):-
	call(Term, Children, Depth, Trimmed),
	prune_rec(Rest, Trimmed, Depth, R).
	
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
static_children_trim(Children, _, TrimmedChildren):-
	static_children_trim_max_children(MaxChildren),
	splice(Children, MaxChildren, TrimmedChildren).

% Meetod 4.2
% Lõikab ära pooled harud, ümardab ülesse
static_children_half_trim(Children, _, TrimmedChildren):-
	length(Children, Length),
	round(Length/2, Round),
	splice(Children, Round, TrimmedChildren).
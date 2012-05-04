% Kõikide tippude arv
:- dynamic total_count/1.
% Kõikide tippude koguhind
:- dynamic total_cost/1.
% Tippude arv millel ei ole harusid
:- dynamic branch_count/1.
% Tippude millel ei ole harusid koguhind
:- dynamic branch_cost/1.

total_count(0).
total_cost(0).

% Annab kärpimis termide listi
prune_terms(Terms):-
	A = static_depth_prune,
	B = static_children_half_prune,
	C = dynamic_children_depth_prune,
	D = static_price_prune,
	E = dynamic_avgerage_cost_prune,
	Terms = [A, B, C, D, E].

% Children - kärpimata lapsed
% Depth - sügavus
% Trimmed - kärbitud laste list
prune(Children,	Depth, Trimmed):-
    prune_terms(PruneTerms),
    sort_by_cost(Children, Sorted),
	prune_rec(PruneTerms, Sorted, Depth, Trimmed).

% Rekursiivne meetod Termide kärpimis listi rakendamiseks
% Call/2+ kasutame =.. ja call/1 asemel
prune_rec([], Children, _, Children).
prune_rec([Term|Rest], Children, Depth, R):-
	call(Term, Children, Depth, Trimmed),
	prune_rec(Rest, Trimmed, Depth, R).
	
% Meetod 1
% Lõikab ära harud, kui sügavus on üle max
static_depth_prune_max_depth(2).
static_depth_prune(Children, Depth, TrimmedChildren):-
	static_depth_prune_max_depth(MaxDepth),
	(	Depth < MaxDepth,
		get_same(Children, TrimmedChildren),
		!
	);
	empty_list(TrimmedChildren).

% Meetod 4.1
% Lõikab ära harud, kui neid on rohkem kui lubatud
static_children_prune_max_children(1).
static_children_prune(Children, _, TrimmedChildren):-
	static_children_prune_max_children(MaxChildren),
	splice(Children, MaxChildren, TrimmedChildren).

% Meetod 4.2
% Lõikab ära pooled harud, ümardab ülesse
static_children_half_prune(Children, _, TrimmedChildren):-
	length(Children, Length),
	round(Length/2, Round),
	splice(Children, Round, TrimmedChildren).
	
% Meetod 5
% Alampuu kärpimine harude arvu ja sügavuse suhte kaudu 
dynamic_children_depth_prune_const(2).
dynamic_children_depth_prune(Children, Depth, TrimmedChildren):-
    dynamic_children_depth_prune_const(Const),
    Max is Const - Depth,
    splice(Children, Max, TrimmedChildren).
    
% Meetod 6
% Kärpimine staatilise hinna järgi
static_price_prune_max_cost(1).
static_price_prune(Children, _, TrimmedChildren):-
    static_price_prune_max_cost(MaxCost),
    static_price_prune2(Children, MaxCost, TrimmedChildren).

static_price_prune2([], _, []).
static_price_prune2([[H,C]|T], MaxCost, [[H,C]|T2]):-
    C =< MaxCost,
    static_price_prune2(T, MaxCost, T2).
static_price_prune2([[_,C]|T], MaxCost, T2):-
    C > MaxCost,
    static_price_prune2(T, MaxCost, T2).
    
% Meetod 7
% Kärpimine keskmise tipu hinna järgi 
dynamic_avgerage_cost_prune(Children, _, TrimmedChildren):-
    total_count(Count),
    total_cost(Cost),
    calculate_children(Children, ChildrenCost),
    length(Children, Length),
    NewCost is Cost + ChildrenCost,
    NewCount is Count + Length,
    retract(total_count(Count)),
    retract(total_cost(Cost)),
    assert(total_cost(NewCost)),
    assert(total_count(NewCount)),
    avg(Cost, Count, Avg),
    prune_by_cost(Children, Avg, TrimmedChildren),
    !.
    

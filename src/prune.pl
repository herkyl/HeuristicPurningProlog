% Kõikide tippude arv
:- dynamic total_count/1.
% Kõikide tippude koguhind
:- dynamic total_cost/1.
% Tippude arv millel ei ole harusid
:- dynamic branch_count/1.
% Tippude millel ei ole harusid koguhind
:- dynamic branch_cost/1.
% Tippude arv ilma lehtedeta
:- dynamic total_child_count/1.
% Tippude hind ilma lehtedeta
:- dynamic total_child_cost/1.

total_count(0).
total_cost(0).
total_child_count(0).
total_child_cost(0).

% Annab kärpimis termide listi
prune_terms(Terms):-
	A = static_depth_prune,
	B = static_breadth_prune,
	C = dynamic_depth_breadth_prune,
	D = static_children_half_prune,
	E = dynamic_children_depth_prune,
	F = static_price_prune,
	G = dynamic_avgerage_cost_prune,
	Terms = [C].

% PruneTerms - kärpimis meetodid
% Children - kärpimata lapsed
% Depth - sügavus
% Trimmed - kärbitud laste list
prune(_, [], _, []).
prune([], L, _, L).
prune(PruneTerms, Children,	Depth, Trimmed):-
    sort_by_cost(Children, Sorted),
	prune_rec(PruneTerms, Sorted, Depth, Trimmed).

% Rekursiivne meetod Termide kärpimis listi rakendamiseks
% Call/2+ kasutame =.. ja call/1 asemel
prune_rec([], Children, _, Children).
prune_rec([Term|Rest], Children, Depth, R):-
	ignore(call(Term, Children, Depth, Trimmed)),
	prune_rec(Rest, Trimmed, Depth, R).
	
% Meetod 1
% Lõikab ära harud, kui sügavus on üle max
static_depth_prune_max_depth(2).
static_depth_prune(Children, Depth, TrimmedChildren):-
	static_depth_prune_max_depth(MaxDepth),
	(	Depth =< MaxDepth,
		get_same(Children, TrimmedChildren),
		!
	);
	empty_list(TrimmedChildren).

% Meetod 2
% Kärpimine staatilise laiuse järgi
static_breadth_prune_max(2).
static_breadth_prune(Children, _, TrimmedChildren):-
	static_breadth_prune_max(Max),
	splice(Children, Max, TrimmedChildren).
	
% Meetod 3
% Kärpimine puu sügavuse ja laiuse järgi 
dynamic_depth_breadth_prune_max(1).
dynamic_depth_breadth_prune(Children, Depth, TrimmedChildren):-
    dynamic_depth_breadth_prune_max(Max),
    MaxChildren is Max * Depth,
	splice(Children, MaxChildren, TrimmedChildren).
    

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
    writeln(Avg),
    prune_by_cost(Children, Avg, TrimmedChildren),
    !.
    
%dynamic_average_children_prune(Children, 0, Children).
%dynamic_average_children_prune(Children, 1, Children).
dynamic_average_children_prune([], _, []).
dynamic_average_children_prune(Children, _, TrimmedChildren):-
    total_child_count(Count),
    total_child_cost(Cost),
    avg(Cost, Count, Avg),
    writeln([Cost, Count, Avg]),
    dynamic_average_child_prune(Children, Count, Cost, Avg, TrimmedChildren),
    writeln([Children, TrimmedChildren]).

dynamic_average_child_prune([], _, _, _, []).
dynamic_average_child_prune([[Child, Cost]|Rest], TotalCount, TotalCost, Avg, [[Child, Cost]|R]):-
    Cost =< Avg,
	retract(total_child_count(TotalCount)),
	retract(total_child_cost(TotalCost)),
	NewCount is TotalCount + 1,
	NewCost is TotalCost + Cost,
	assert(total_child_count(NewCount)),
	assert(total_child_cost(NewCost)),
	dynamic_average_child_prune(Rest, NewCount, NewCost, Avg, R).
dynamic_average_child_prune([[_, Cost]|Rest], TotalCount, TotalCost, Avg, R):-
    Cost > Avg,
	dynamic_average_child_prune(Rest, TotalCount, TotalCost, Avg, R).

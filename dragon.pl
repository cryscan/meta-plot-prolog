% introduce characters.
fact(character(princess)).
fact(character(dragon)).
fact(character(felix)).
fact(character(troy)).

% introduce roles.
fact(victim(princess)).
fact(villan(dragon)).
fact(hero(felix)).
fact(hero(troy)).

% introduce locations.
fact(path(castle-wood)).
fact(path(castle-heritage)).
fact(path(wood-dungeon)).

fact(home(dragon, dungeon)).
fact(home(princess, castle)).
fact(home(troy, wood)).
fact(home(felix, castle)).

fact(level(princess, 1)).
fact(level(felix, 4)).
fact(level(troy, 5)).
fact(level(dragon, 8)).

fluent(protection(castle, true)).
fluent(protection(wood, true)).
fluent(protection(dungeon, false)).
fluent(protection(heritage, true)).

fluent(at(princess, castle)).
fluent(at(felix, castle)).
fluent(at(troy, wood)).
fluent(at(dragon, dungeon)).

fluent(affection(felix-princess, true)).
fluent(affection(troy-princess, true)).
fluent(affection(princess-felix, false)).
fluent(affection(princess-troy, false)).

% hatred is motivation of violence.
fluent(hatred(princess-felix, false)).
fluent(hatred(princess-troy, false)).
fluent(hatred(princess-dragon, false)).

fluent(hatred(felix-princess, false)).
fluent(hatred(felix-troy, false)).
fluent(hatred(felix-dragon, false)).

fluent(hatred(troy-princess, false)).
fluent(hatred(troy-felix, false)).
fluent(hatred(troy-dragon, false)).

fluent(married(princess, false)).
fluent(married(felix, false)).
fluent(married(troy, false)).

fluent(kidnaped(princess, false)).

fluent(dead(princess, false)).
fluent(dead(felix, false)).
fluent(dead(troy, false)).
fluent(dead(dragon, false)).

% s0, the initial situation, is the (ordered) set
% of fluents
s0(Situation) :-
    setof(S, fluent(S), Situation).

% Take a list of Actions and execute them
execute_process(S1, [], S1). % Nothing to do
execute_process(S1, [Action|Process], S2) :-
    poss(Action, S1), % Ensure valid Process
    result(S1, Action, Sd),
    execute_process(Sd, Process, S2).

% Does a fluent hold (is true) in the Situation?
% This is the query mechanism for Situations
% Use-case 1: check a known fluent
holds(Fluent, Situation) :-
    ground(Fluent),
    ord_memberchk(Fluent, Situation), !.
% Use-case 2: search for a fluent
holds(Fluent, Situation) :-
    member(Fluent, Situation).

% Utility to replace a fluent in the Situation
replace_fluent(S1, OldEl, NewEl, S2) :-
    ord_del_element(S1, OldEl, Sd),
    ord_add_element(Sd, NewEl, S2).

team(C1, C2, S) :-
    holds(at(C1, X), S),
    holds(at(C2, X), S),
    holds(hatred(C1-C2, false), S),
    holds(hatred(C2-C1, false), S).

hate(S1, C1-C2, S2) :-
    holds(hatred(C1-C2, H), S1),
    replace_fluent(S1,
                   hatred(C1-C2, H),
                   hatred(C1-C2, true),
                   S2).

all_hate(S1, []-C, S2) :-
    fact(character(C)),
    S2=S1, !.
all_hate(S1, [C1|T]-C2, S2) :-
    hate(S1, C1-C2, Sa),
    all_hate(Sa, T-C2, S2), !.

% all character occurs hate C
all_hate(S1, C, S2) :-
    findall(C2,
            ( fact(character(C2)),
              \+ C2=C,
              holds(at(C, X), S1),
              holds(at(C2, X), S1),
              holds(hatred(C2-_, _), S1)
            ),
            Cs),
    all_hate(S1, Cs-C, S2).

% all character likes C2 hate C1
all_hate(S1, C1, C2, S2) :-
    findall(C3,
            ( fact(character(C3)),
              \+ C3=C1,
              \+ C3=C2,
              holds(affection(C3-C2, true), S1)
            ),
            Cs),
    all_hate(S1, Cs-C1, S2).

poss(goto(C, L), S) :-
    holds(dead(C, false), S),
    holds(at(C, X), S),
    (   fact(path(X-L))
    ;   fact(path(L-X))
    ),
    (   fact(victim(C))
    ->  holds(protection(L, true), S),
        holds(kidnaped(C, false), S)
    ;   fact(villan(C))
    ->  findall(V,
                ( fact(victim(V)),
                  holds(kidnaped(V, true), S)
                ),
                [])
    ;   true
    ).

poss(kidnap(C1, C2), S) :-
    dif(C1, C2),
    holds(dead(C1, false), S),
    holds(dead(C1, false), S),
    fact(villan(C1)),
    fact(victim(C2)),
    holds(kidnaped(C2, false), S),
    holds(at(C1, X), S),
    holds(at(C2, X), S),
    holds(protection(X, false), S),
    fact(level(C1, N1)),
    fact(level(C2, N2)),
    N1>N2.

poss(attack(C), S) :-
    holds(dead(C, false), S),
    fact(villan(C)),
    holds(at(C, L), S),
    holds(protection(L, true), S).

poss(restore(C), S) :-
    holds(dead(C, false), S),
    fact(hero(C)),
    holds(at(C, L), S),
    holds(protection(L, false), S),
    % there is no villan
    findall(V,
            ( fact(villan(V)),
              holds(at(V, L), S)
            ),
            []).

% C1 persuade C2 to hate C3
poss(persuade(C1, C2, C3), S) :-
    dif(C1, C2),
    holds(dead(C1, false), S),
    holds(dead(C2, false), S),
    holds(dead(C3, false), S),
    fact(hero(C2)),
    fact(villan(C3)),
    team(C1, C2, S),
    holds(hatred(C1-C3, true), S),
    holds(hatred(C2-C3, false), S).

poss(kill(C1, C2), S) :-
    dif(C1, C2),
    holds(dead(C1, false), S),
    holds(dead(C2, false), S),
    (   fact(victim(C1))
    ->  holds(kidnaped(C1, false), S)
    ;   true
    ),
    holds(at(C1, X), S),
    holds(at(C2, X), S),
    holds(hatred(C1-C2, true), S),
    fact(level(C2, N2)),
    (   fact(level(C1, N1)),
        N1>N2
    ;   team(C1, C3, S),
        fact(level(C1, N1)),
        fact(level(C3, N3)),
        N1+N3>N2
    ).

poss(free(C1, C2), S) :-
    dif(C1, C2),
    holds(dead(C1, false), S),
    holds(dead(C2, false), S),
    holds(at(C1, X), S),
    holds(at(C2, X), S),
    fact(hero(C1)),
    fact(victim(C2)),
    holds(kidnaped(C2, true), S),
    holds(hatred(C1-C2, false), S),
    findall(C3,
            ( holds(dead(C3, false), S),
              holds(hatred(C1-C3, true), S),
              holds(hatred(C2-C3, true), S)
            ),
            []).

poss(marry(C1, C2), S) :-
    dif(C1, C2),
    holds(dead(C1, false), S),
    holds(dead(C2, false), S),
    holds(affection(C1-C2, true), S),
    holds(affection(C2-C1, true), S),
    holds(married(C1, false), S),
    holds(married(C2, false), S),
    holds(at(C1, X), S),
    holds(at(C2, X), S),
    (   fact(home(C1, X))
    ;   fact(home(C2, X))
    ),
    (   fact(victim(C1))
    ->  holds(kidnaped(C1, false), S)
    ;   true
    ),
    (   fact(victim(C2))
    ->  holds(kidnaped(C2, false), S)
    ;   true
    ).

result(S1, goto(C, L), S2) :-
    holds(at(C, X), S1),
    replace_fluent(S1,
                   at(C, X),
                   at(C, L),
                   S2).
                    
result(S1, kidnap(C1, C2), S2) :-
    all_hate(S1, C1, Sa),
    % bring back to home.
    fact(home(C1, L)),
    holds(at(C1, X), S1),
    replace_fluent(Sa,
                   at(C1, X),
                   at(C1, L),
                   Sb),
    replace_fluent(Sb,
                   at(C2, X),
                   at(C2, L),
                   Sc),
    replace_fluent(Sc,
                   kidnaped(C2, false),
                   kidnaped(C2, true),
                   S2).

result(S1, attack(C), S2) :-
    all_hate(S1, C, Sa),
    holds(at(C, L), Sa),
    replace_fluent(Sa,
                   protection(L, true),
                   protection(L, false),
                   S2).
result(S1, restore(C), S2) :-
    holds(at(C, L), S1),
    replace_fluent(S1,
                   protection(L, false),
                   protection(L, true),
                   S2).

result(S1, persuade(_, C2, C3), S2) :-
    hate(S1, C2-C3, S2).

result(S1, kill(C1, C2), S2) :-
    all_hate(S1, C1, C2, Sa),
    replace_fluent(Sa,
                   dead(C2, false),
                   dead(C2, true),
                   S2).

result(S1, free(C1, C2), S2) :-
    (   holds(affection(C2-C1, false), S1)
    ->  replace_fluent(S1,
                       affection(C2-C1, false),
                       affection(C2-C1, true),
                       Sa)
    ;   Sa=S1
    ),
    replace_fluent(Sa,
                   kidnaped(C2, true),
                   kidnaped(C2, false),
                   S2).

result(S1, marry(C1, C2), S2) :-
    replace_fluent(S1,
                   married(C1, false),
                   married(C1, true),
                   Sa),
    replace_fluent(Sa,
                   married(C2, false),
                   married(C2, true),
                   S2).
    % all_hate(Sb, C1, C2, Sc),
    % all_hate(Sc, C2, C1, S2).
beat(prelude-1, 0).
beat(prelude-2, 0).
beat(rising, 1).
beat(climax, 2).
beat(ending-1, 3).
beat(ending-2, 3).

premise(prelude-1, S) :-
    holds(dead(dragon, false), S),
    holds(kidnaped(princess, false), S).

premise(prelude-2, S) :-
    holds(dead(dragon, false), S),
    holds(kidnaped(princess, false), S).

premise(rising, S) :-
    holds(dead(dragon, false), S),
    holds(kidnaped(princess, true), S).

premise(climax, S) :-
    holds(dead(dragon, false), S).

premise(ending-1, S) :-
    holds(dead(dragon, true), S),
    (   holds(affection(princess-troy, true), S)
    ;   holds(affection(princess-felix, true), S)
    ).

premise(ending-2, S) :-
    holds(dead(dragon, true), S).

candidate_beats(Situation, Beats) :-
    setof(Beat, premise(Beat, Situation), Beats).

candidate_beats(Situation, Phase, Beats) :-
    setof(Beat,
          (premise(Beat, Situation), beat(Beat, Phase)),
          Beats).

goal(prelude-1, kidnaped(princess, true)).
goal(prelude-1, hatred(troy-dragon, true)).

goal(prelude-2, kidnaped(princess, true)).
goal(prelude-2, hatred(felix-dragon, true)).

goal(rising, hatred(troy-dragon, true)).
goal(rising, hatred(felix-dragon, true)).
goal(rising, protection(castle, true)).
goal(rising, kidnaped(princess, true)).

goal(climax, dead(dragon, true)).
goal(climax, kidnaped(princess, false)).

goal(climax, protection(dungeon, true)).
goal(climax, protection(wood, true)).
goal(climax, protection(heritage, true)).

goal(ending-1, at(troy, castle)).
goal(ending-1, at(felix, castle)).
goal(ending-1, at(princess, castle)).
goal(ending-1, married(princess, true)).

goal(ending-2, dead(dragon, true)).
goal(ending-2, married(princess, false)).
goal(ending-2, kidnaped(princess, false)).

% The Goal Situation is the (ordered) set of fluents that
% describe a goal
goal_situation(Beat, S) :-
    setof(G, goal(Beat, G), S).

% Test to see if Situation satifies the Goal
% Note that the Situation can contain fluents
% not described in Goal
reached_goal(GoalSituation, Situation) :-
    ord_subtract(GoalSituation, Situation, []). % [] -> no goals not in Situation
goal_beats(Situation, Phase, Beats) :-
    setof(Beat,
          (beat(Beat, Phase), goal_situation(Beat, S), reached_goal(S, Situation)),
          Beats).

:- use_module(library(heaps)).

% Use to order search
heuristic_distance_to_goal(GoalSituation, Situation, Distance) :-
    ord_subtract(GoalSituation, Situation, Dif),
    length(Dif, Distance).

% Add a Cost-Sit-Process triple to the search heap
% of open nodes. Carrying Process for interest.
add_to_open_nodes(AccCost, H1, Sit-Process, Goal, H2) :-
    heuristic_distance_to_goal(Goal, Sit, D),
    succ(AccCost, ActCost), % one action has been taken, so incr
    Priority is ActCost+D, % Priority cost
    add_to_heap(H1, Priority, ActCost-Sit-Process, H2).

% Add a list of Sit-Process Pairs
open_add_pairs(_, Heap, _, [], _, Heap).
open_add_pairs(AccCost, H1, Sits, [S-P|T], G, H2) :-
    (   ord_memberchk(S, Sits)
    ->  add_to_open_nodes(AccCost, H1, S-P, G, Hd)
    ;   Hd = H1
    ),
    open_add_pairs(AccCost, Hd, Sits, T, G, H2).

% Convenience predicate to heap
get_from_open_nodes(H1, Sit-Process, H2) :-
    get_from_heap(H1, _Priority, Sit-Process, H2).
% convenience predicate to start a_star
% and reverse the process for natural reading
plan(Beat, Sit, Process) :-
    s0(S0),
    goal_situation(Beat, GoalSituation),
    a_star(S0, GoalSituation, Sit-Answer),
    reverse(Answer, Process).

plan(Beat, S0, Sit, Process) :-
    goal_situation(Beat, GoalSituation),
    a_star(S0, GoalSituation, Sit-Answer),
    reverse(Answer, Process).

% a_star search setup
a_star(StartSituation, GoalSituation, Answer) :-
    % Create heap of open search nodes
    heuristic_distance_to_goal(GoalSituation, StartSituation, D),
    singleton_heap(Open, D, 0-StartSituation-[]),
    % Do the search
    a_star(Open, GoalSituation, [StartSituation], Answer).

a_star(Open, GoalSituation, Closed, Answer) :-
    % Get the most promising Sit-Process pair
    get_from_open_nodes(Open, AccCost-Sit-Process, RemainingSearch),
    % If we've reached the goal, return the Answer
    (   reached_goal(GoalSituation, Sit), Answer = Sit-Process
    % Otherwise (or searching for other solutions), find the
    % reachable situations via some additional action A that is
    % recorded onto the process
    ;   setof(S-[A|Process], (poss(A, Sit), result(Sit, A, S)), AS_Pairs),
        % Exclude visited nodes
        pairs_keys(AS_Pairs, Children),
        ord_union(Closed, Children, Closed1, Sits),
        % Add new open nodes (with tag-along processes) to search heap
        open_add_pairs(AccCost, RemainingSearch, Sits, AS_Pairs, GoalSituation, Open1),
        % Carry on searching
        a_star(Open1, GoalSituation, Closed1, Answer)
    ).
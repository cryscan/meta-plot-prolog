fluent(location(robbie, hallway)).
fluent(location(car-key, hallway)).
fluent(location(garage-key, hallway)).
fluent(location(vacuum-cleaner, kitchen)).
fluent(door(hallway-kitchen, unlocked)).
fluent(door(kitchen-garage, locked)).
fluent(door(garage-car, locked)).
fluent(holding(nothing)).
fluent(clean(car, false)).

%facts, unlike fluents, don't change
fact(home(car-key, hallway)).
fact(home(garage-key, hallway)).
fact(home(vacuum-cleaner, kitchen)).

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

% Lots of actions to declare here...
% Still less code than writing out the
% graph we're representing
% 
% Robbie's Action Repertoire :
%  - goTo(Origin, Destination)
%  - pickup(Item)
%  - drop(Item)
%  - put_away(Item) % the tidy version of drop
%  - unlock(Room1-Room2)
%  - lock(Room1-Room2)
%  - clean_car
poss(goto(L), S) :-
    % If robbie is in X and the door is unlocked
    holds(location(robbie, X), S),
    (   holds(door(X-L, unlocked), S)
    ;   holds(door(L-X, unlocked), S)
    ).
poss(pickup(X), S) :-
    % If robbie is in the same place as X and not
    % holding anything
    dif(X, robbie), % Can't pickup itself!
    holds(location(X, L), S),
    holds(location(robbie, L), S),
    holds(holding(nothing), S).
poss(put_away(X), S) :-
    % If robbie is holding X, it belongs in L
    % and robbie is in L (location(X, L) is implicit)
    holds(holding(X), S),
    fact(home(X, L)),
    holds(location(robbie, L), S).
poss(drop(X), S) :-
    % Can drop something if holding it
    % Can't drop nothing!
    dif(X, nothing),
    holds(holding(X), S).
poss(unlock(R1-R2), S) :-
    % Can unlock door between R1 and R2
    % Door is locked
    holds(door(R1-R2, locked), S),
    % Holding the key to the room
    holds(holding(R2-key), S),
    % Located in one of the rooms
    (   holds(location(robbie, R1), S)
    ;   holds(location(robbie, R2), S)
    ).
poss(lock(R1-R2), S) :-
    % Can lock door R1-R2
    % Only if it's locked, robbie has the key
    % and is in one of the rooms
    holds(door(R1-R2, unlocked), S),
    holds(holding(R2-key), S),
    (   holds(location(robbie, R1), S)
    ;   holds(location(robbie, R2), S)
    ).
poss(clean_car, S) :-
    % Robbie is in the car with the vacuum-cleaner
    holds(location(robbie, car), S),
    holds(holding(vacuum-cleaner), S).

result(S1, goto(L), S2) :-
    % Robbie moves
    holds(location(robbie, X), S1),
    replace_fluent(S1,
                   location(robbie, X),
                   location(robbie, L),
                   Sa),
    % If Robbie is carrying something, it moves too
    dif(Item, nothing),
    (   holds(holding(Item), S1),
        replace_fluent(Sa,
                       location(Item, X),
                       location(Item, L),
                       S2)
    ;   \+ holds(holding(Item), S1),
        S2=Sa
    ).
result(S1, pickup(X), S2) :-
    % Robbie is holding X
    replace_fluent(S1, holding(nothing), holding(X), S2).
result(S1, drop(X), S2) :-
    % Robbie is no-longer holding X,
    % its location is not changed
    replace_fluent(S1, holding(X), holding(nothing), S2).
result(S1, put_away(X), S2) :-
    % Robbie is no-longer holding X,
    % its location is not changed
    replace_fluent(S1, holding(X), holding(nothing), S2).
result(S1, unlock(R1-R2), S2) :-
    % Door R1-R2 is unlocked
    replace_fluent(S1,
                   door(R1-R2, locked),
                   door(R1-R2, unlocked),
                   S2).
result(S1, lock(R1-R2), S2) :-
    % Door R1-R2 is locked
    replace_fluent(S1,
                   door(R1-R2, unlocked),
                   door(R1-R2, locked),
                   S2).
result(S1, clean_car, S2) :-
    % The car is clean
    replace_fluent(S1, clean(car, false), clean(car, true), S2).

goal(clean(car, true)).
goal(door(garage-car, locked)).
goal(door(kitchen-garage, locked)).
% Everything must be tidied away
goal(location(X, L)) :-
    fact(home(X, L)).
goal(holding(nothing)).

% The Goal Situation is the (ordered) set of fluents that
% describe a goal
goal_situation(S) :-
    setof(G, goal(G), S).

% Test to see if Situation satifies the Goal
% Note that the Situation can contain fluents
% not described in Goal
reached_goal(GoalSituation, Situation) :-
    ord_subtract(GoalSituation, Situation, []). % [] -> no goals not in Situation
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
a_star(Sit, Process) :-
    s0(S0),
    goal_situation(GoalSituation),
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
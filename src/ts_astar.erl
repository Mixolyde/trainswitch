%%%----FILE ts_astar.erl----

%%% various astar functions, helpers and heuristics
-module(ts_astar).
-import(ts_util, [cars_on_track/2, move_front_to_back/2, move_back_to_front/2,
  update_tracks/2, print_solution_path/2, new_solution/1, update_solution/2]).
-import(ts_util, [dijkstra_all/1, locate_car/2]).
-import(trainswitch, [possible_moves/2, apply_move/2]).
-include("../include/trainswitch.hrl").

-compile([debug_info, export_all]).

astar_search(Problem, HOfStateFun) ->
    %define initial stuff
    Goal = Problem#problem.goal_state,
    Yard = Problem#problem.yard,
    HOfStateForProblem = fun(State) -> HOfStateFun(State, Goal, Yard) end,
    FringeState = new_solution_astar(Problem, HOfStateForProblem),
    io:format("Init state: ~p~n", [FringeState]),
    ClosedStates = [],
    %call astar stack handler
    Solution = astar_search_rec(Problem, [FringeState], ClosedStates, HOfStateForProblem),
    %return solution
    % io:format("return solution: ~w~n", [Solution]),
    case Solution of
        {notfound, Reason} ->
            io:format("Not found: ~w~n", [Reason]);
        {found, FoundSolution} ->
            print_astar_solution_path(Problem, FoundSolution),
            % return solution path
            lists:reverse(FoundSolution#astar_solution_state.solution_state#solution_state.moves)
    end.

astar_search_not_on_goal(Problem) ->
    astar_search(Problem, fun ts_astar:not_on_goal_count/3).

astar_search_dijkstra(Problem) ->
    astar_search(Problem, fun ts_astar:dijkstra_sum_heuristic/3).


%Empty Fringe list means no solution can be found
astar_search_rec(_Problem, [], _ClosedStates, _HOfState) ->
    {not_found, no_solution_exists};
%if Goals match, we're done
astar_search_rec(#problem{goal_state = Goal},
    [ #astar_solution_state{solution_state = #solution_state{state = Goal }} = FirstFringe | _RestFringe ],
    _ClosedStates, _HOfState) ->
    {found, FirstFringe};
astar_search_rec(Problem, [FirstFringe | RestFringe], ClosedStates,HOfState) ->
    % io:format("~w Fringe States and ~w ClosedStates~n", [length(RestFringe) + 1, length(ClosedStates)]),
    % io:format("Examining FirstFringe: ~w~n", [FirstFringe]),
    %generate successors
    Successors = expand_astar_solution(Problem, FirstFringe, HOfState),

    NewFringe = handle_successors(Successors, RestFringe, ClosedStates, RestFringe),
    %sort the open array
    astar_search_rec(Problem, NewFringe, [FirstFringe] ++ ClosedStates, HOfState) .

handle_successors([], _Fringe, _ClosedStates, FringeAcc) ->
    %%sort and return accumulated fringe
    FringeAcc;
handle_successors([FirstSuccessor | RestSuccessors], Fringe, ClosedStates, FringeAcc) ->
    %%test successor
    %io:format("Testing Successor: ~w~n", [FirstSuccessor]),
    %% if it's in the closed list and a bigger fvalue do nothing
    %% if it's in the open list and a bigger fvalue do nothing
    %% else push it onto the fringe
    BetterClosed =
    lists:filter(fun(ClosedState) -> equal_astar_states(FirstSuccessor, ClosedState) and
    (ClosedState#astar_solution_state.fvalue < FirstSuccessor#astar_solution_state.fvalue)
    end, ClosedStates),
    BetterFringe =
    lists:filter(fun(FringeState) -> equal_astar_states(FirstSuccessor, FringeState)  and
    (FringeState#astar_solution_state.fvalue < FirstSuccessor#astar_solution_state.fvalue)
    end, Fringe),

    %% io:format("BetterClosed count: ~w, BetterFringe count: ~w~n", [length(BetterClosed), length(BetterFringe) ]),

    case BetterClosed ++ BetterFringe == [] of
        false ->
            %%handle the rest
            %%io:format("Not adding to fringe~n", []),
            handle_successors(RestSuccessors, Fringe, ClosedStates, FringeAcc);
        true ->
            %% add new state to fringe accumulator
            %% io:format("Inserting into fringe sorted~n", []),
            handle_successors(RestSuccessors, Fringe, ClosedStates, insert_astar_state(FirstSuccessor,FringeAcc))
    end.

%% get a list of neighboring tracks to this track
%% fold a filtering function over the yard list
get_neighbors(Yard, Track) ->
    get_neighbors_acc(Yard, Track, []).

get_neighbors_acc([], _Track, Acc) ->
    lists:reverse(Acc);
get_neighbors_acc([{Track, Neighbor} | RestTrack], Track, Acc) ->
    get_neighbors_acc(RestTrack, Track, [Neighbor | Acc]);
get_neighbors_acc([{Neighbor, Track} | RestTrack], Track, Acc) ->
    get_neighbors_acc(RestTrack, Track, [Neighbor | Acc]);
get_neighbors_acc([_other | RestTrack], Track, Acc) ->
    get_neighbors_acc(RestTrack, Track, Acc).

%%% Heuristics

%look at each track in the state
%for each element of each track, up the count if that element is not found
%in the corresponding goal track
not_on_goal_count(State, Goal, _Yard) ->
    not_on_goal_count_acc(State, Goal, 0).

not_on_goal_count_acc([], _Goal, Acc) ->
    Acc;
not_on_goal_count_acc([{Track, Cars} | RestOfTracks], Goal, Acc) ->
    %io:format("Checking ~w against ~w for cars on goal track~n", [Cars, cars_on_track(Track, Goal)]),
    Sum = cars_not_on_goal_track_count_acc(Cars, cars_on_track(Track, Goal), 0),
    not_on_goal_count_acc(RestOfTracks, Goal, Acc + Sum).

cars_not_on_goal_track_count_acc([], _GoalCars, Acc) ->
    Acc;
cars_not_on_goal_track_count_acc([Car | RestOfCars], GoalCars, Acc) ->
    case lists:member(Car, GoalCars) of
        false ->
            cars_not_on_goal_track_count_acc(RestOfCars, GoalCars, Acc + 1);
        true ->
            cars_not_on_goal_track_count_acc(RestOfCars, GoalCars, Acc)
    end.

out_of_order_score(_Current, [], _Yard) -> 0;
out_of_order_score(Current, [{GoalTrack, GoalCars} | RestGoal], Yard) ->
    CurrentCars = cars_on_track(GoalTrack, Current),
    if
        length(GoalCars) > 2 andalso
        length(CurrentCars) > 0 andalso
        CurrentCars =/= GoalCars ->
                % io:format("Counting out of order cars between ~p and ~p~n", [Cars, GoalCars] ),
                %% return the out of order count for this track and recurse
                2 * out_of_order_track(CurrentCars, GoalCars, GoalCars, 0) +
                out_of_order_score(Current, RestGoal, Yard);
        true ->
            %% nothing to comput for this track so just recurse
            out_of_order_score(Current, RestGoal, Yard)
    end.

% (* 1 2 3 4 5)
% (5 4 3 2 1 *)
% (* 1 2 4 3 5)

out_of_order_track([], _GoalCars, _OriginalGoal, Acc) -> Acc;
out_of_order_track(_Cars, [], _OriginalGoal, Acc) -> Acc;
out_of_order_track([FirstCar | Cars], [FirstCar | GoalCars], OriginalGoal, Acc) ->
    % io:format("First Cars match, recursing with ~p and ~p~n", [Cars, GoalCars]),
    out_of_order_track(Cars, GoalCars, OriginalGoal, Acc);
out_of_order_track([FirstCar | Cars], [DifferentCar | GoalCars], OriginalGoal, Acc)
    when FirstCar =/= DifferentCar ->
    Member = lists:member(FirstCar, OriginalGoal),
    if
        Member ->
            % io:format("First Cars don't match. Adding 1 and recursing with ~p and ~p~n", [Cars, GoalCars]),
            out_of_order_track(Cars, GoalCars, OriginalGoal, 1 + Acc);
        true ->
            % else the cars are different, but it doesn't go on this track
            out_of_order_track(Cars, GoalCars, OriginalGoal, Acc)
    end.


%% sum the distance of every car not on its goal track to the goal
dijkstra_sum_heuristic(State, Goal, Yard) ->
    TrackDistances = dijkstra_all(Yard),
    % io:format("Got all track distances to other tracks~n~w~n", [TrackDistances]),
    %for each track in the goal state, get the distance of each element to
    %where it is in the current State and sum those
    %initial sums are 0
    %get current location of the engine and distances from that track for computing heuristic
    lists:foldl(fun({Track, Cars}, Sum) ->
        {Track, CurrentDistances} = lists:keyfind(Track, 1, TrackDistances),
        lists:foldl(fun(Car, TrackSum) ->
            CurrentTrack = locate_car(Car, State),
            {CurrentTrack, Distance} = lists:keyfind(CurrentTrack, 1, CurrentDistances),
            %add this distance to the current sum and fold
            Distance  + TrackSum
        end, 0, Cars)
        + Sum end, 0, Goal)
        + out_of_order_score(State, Goal, Yard).

%% AStar util stuff
new_solution_astar(#problem{init_state = Init, goal_state = _Goal, yard = _Yard}, HFun) ->
    #astar_solution_state{solution_state =
        #solution_state{state = Init,
            moves = []},
        fvalue = HFun(Init) } .

%% expand solution
expand_astar_solution(Problem, Astar_Solution_State, HFunOfState) ->
    lists:map(fun(Move) -> update_astar_solution(Astar_Solution_State, Move, HFunOfState) end,
        possible_moves(Problem#problem.yard, Astar_Solution_State#astar_solution_state.solution_state#solution_state.state) ).

update_astar_solution(Astar_Solution_State, Move, HFunOfState) ->
    Depth = length(Astar_Solution_State#astar_solution_state.solution_state#solution_state.moves) + 1,
    State = apply_move(Move, Astar_Solution_State#astar_solution_state.solution_state#solution_state.state),
    #astar_solution_state{solution_state =
        #solution_state{
            state = State,
            moves = [Move] ++ Astar_Solution_State#astar_solution_state.solution_state#solution_state.moves},
        fvalue = Depth + HFunOfState(State) }.

equal_astar_states( #astar_solution_state{solution_state = #solution_state{state = State}},
    #astar_solution_state{solution_state = #solution_state{state = State}} ) -> true;
equal_astar_states(_, _) -> false.

%%insert sort InsertState into a list of states by fvalue
insert_astar_state(InsertState, [])  -> [InsertState];
insert_astar_state(InsertState, States) -> insert_astar_state(InsertState, [], States).

%%if fvalue is higher than all states, put it on the end
insert_astar_state(InsertState, LowerStates, []) -> LowerStates ++ [InsertState];
%%if fvalue is lower or equal to the current item, insert and end
insert_astar_state(InsertState, LowerStates, [CurrentState | RestStates] ) when
    InsertState#astar_solution_state.fvalue =< CurrentState#astar_solution_state.fvalue ->
        LowerStates ++ [InsertState] ++ [CurrentState] ++ RestStates;
%% else move to the next item and recurse
insert_astar_state(InsertState, LowerStates, [CurrentState | RestStates] ) ->
    insert_astar_state(InsertState, LowerStates ++ [CurrentState], RestStates).


%%print the path of moves that lead from the initial state to the current one in this solution step
print_astar_solution_path(Problem, Solution_state) ->
    io:format("Solution state at depth: ~p with fvalue ~p~n",
    [length(Solution_state#astar_solution_state.solution_state#solution_state.moves),
     Solution_state#astar_solution_state.fvalue]),
    io:format("To go from ~p to~n", [Problem#problem.init_state]),
    io:format("~p~n", [Solution_state#astar_solution_state.solution_state#solution_state.state]),
    io:format("Apply: ~p~n", [lists:reverse(Solution_state#astar_solution_state.solution_state#solution_state.moves)]).

print_problem1_path(_State, []) -> io:format("Finished~n", []);
print_problem1_path(State, [FirstMove| RestMoves]) ->
    NewState = apply_move(FirstMove, State),
    io:format("             ~p~n", [cars_on_track(t2, NewState)]),
    io:format("~p:~p:~p:~p~n",
    [cars_on_track(t1, NewState), cars_on_track(t3, NewState), cars_on_track(t5, NewState), cars_on_track(t6, NewState)]),
    io:format("             ~p~n", [cars_on_track(t4, NewState)]),
    print_problem1_path(NewState, RestMoves).
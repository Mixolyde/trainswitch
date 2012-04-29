%%% train switch yard AI programming assignment
%%% assignment: http://www.cis.udel.edu/~decker/courses/681s07/prog1.html
%%% cd ("C:/Users/bwilliams/Dropbox/dev/erl").
%%% c(trainswitch).

-module(trainswitch).
-author("Brian E. Williams").
-compile([debug_info, export_all]).
-import(ts_util, [cars_on_track/2, move_front_to_back/2, move_back_to_front/2,
  update_tracks/2, print_solution_path/2, new_solution/1, update_solution/2]).
-include("trainswitch.hrl").

%%Homework Problem 1
%% returns all possible moves for a yard in a given state based on the
%% "generate and test" ai paradigm
possible_moves(Yard, State) ->
    lists:filter(
        fun(Move) -> not is_illegal_move(Move, Yard, State) end, generate_all_moves(Yard) ).

%%generate_all_moves(Yard)
%%tail recursive generation using built in fold function and list comprehension
generate_all_moves(Yard) ->
    lists:foldl(
        fun({Left, Right}, Moves) -> [{left, Right, Left}, {right, Left, Right} | Moves] end, [], Yard).

%% check for illegal Move data type or tuple size
is_illegal_move(Move, _Yard, _State) when not is_tuple(Move); size(Move) /= 3 -> true;
is_illegal_move({left, Right, Left}, Yard, State) ->
    (not lists:member({Left, Right}, Yard)) %check for legal connection in yard
    orelse
    length(cars_on_track(Right, State)) == 0 %check that there's something to move
    orelse
    (not lists:member(engine, cars_on_track(Right, State)) and %check that the engine is available
     not lists:member(engine, cars_on_track(Left, State)));
is_illegal_move({right, Left, Right}, Yard, State) ->
    (not lists:member({Left, Right}, Yard)) %check for legal connection in yard
    orelse
    length(cars_on_track(Left, State)) == 0 %check that there's something to move
    orelse
    (not lists:member(engine, cars_on_track(Right, State)) and %check that the engine is available
     not lists:member(engine, cars_on_track(Left, State)));
is_illegal_move(_Move, _Yard, _State) -> true.

%%Homework Problem 2
%%apply_move: apply a move to a state and return the new state
%%trainswitch:apply_move({left, t2, t1}, Yard1#problem.init_state).
apply_move({left, Right, Left}, State) ->
    %io:format("Moving car from ~w to ~w in state: ~w~n",[Right, Left, State]),
    {NewRightList, NewLeftList} = move_front_to_back(cars_on_track(Right, State), cars_on_track(Left, State)),
    %io:format("Moved car from ~w to ~w~n",[{Right, NewRightList}, {Left, NewLeftList}]),
    update_tracks(State, [{Right, NewRightList}, {Left, NewLeftList}]); %return the new state
apply_move({right, Left, Right}, State) ->
    %io:format("Moving car from ~w to ~w in state: ~w~n",[Left, Right, State]),
    { NewLeftList, NewRightList} = move_back_to_front(cars_on_track(Left, State), cars_on_track(Right, State) ),
    %io:format("Moved car from ~w to ~w~n",[{Left, NewLeftList}, {Right, NewRightList}]),
    update_tracks(State, [{Right, NewRightList}, {Left, NewLeftList}]). %return the new state

%%Homework Problem 3
%%expand: take a yard and a state and return a list of all states reachable in one step
expand(State, Yard) ->
    lists:map(fun(Move) -> apply_move(Move,State) end, possible_moves(Yard, State)).

%%Homework Problem 5
%% iterative deepening blind search
iterative_deepening_search(Problem) ->
    ids_recursive(Problem, 1).

ids_recursive(Problem, Limit) ->
    case depth_limited_search(Problem, Limit) of
        {found, FoundState} ->
            print_solution_path(Problem, FoundState),
            {found, FoundState};
        _Other ->
            io:format("Solution not found at depth ~w, iterating~n", [Limit]),
            ids_recursive(Problem, Limit + 1)
    end.

%% depth limited search
%% starts a depth limited search, prints solution if found
depth_limited_search(Problem, Limit) ->
    case dls_recursive(Problem, new_solution(Problem), Limit) of
        {found, FoundState} ->
            {found, FoundState};
        Other ->
            Other
    end.

%first try to match the goal state
dls_recursive(
    #problem{goal_state = Goal},
    #solution_state{state = Goal} = SState,
    _Limit) ->
    %io:format("Goals match: ~w~n", [Goal] ),
    {found, SState};
%then see if we hit bottom
dls_recursive(_Problem,
    #solution_state{depth = Depth}, Limit) when Depth >= Limit ->
    {not_found, max_depth_reached};
%else generate the next possible states and iterate through them
dls_recursive(Problem, State, Limit) ->
    %expand the solution with all possible moves and go deeper
    New_SStates = expand_solution(Problem, State),
    %try each one until it passes
    dls_recurse_with_cut_off(Problem, New_SStates, Limit).

%% expand solution
expand_solution(Problem, Solution_State) ->
    lists:map(fun(Move) -> update_solution(Solution_State, Move) end,
        possible_moves(Problem#problem.yard, Solution_State#solution_state.state) ).

%recurse over the generated states as a stack
%if we empty the stack return out of moves
dls_recurse_with_cut_off(_Problem, [], _Limit) ->
    {not_found, out_of_moves};
%take the top state of the stack, if goal return immediately
%else try the next move on the stack
dls_recurse_with_cut_off(Problem, [SState | Rest], Limit) ->
    case dls_recursive(Problem, SState, Limit) of
        {found, FoundState} ->
            {found, FoundState};
        {not_found, _Reason}    ->
            dls_recurse_with_cut_off(Problem, Rest, Limit)
    end.

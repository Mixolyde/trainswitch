%%%----FILE ts_unit_test----
-module(ts_unit_test).

-compile([debug_info, export_all]).

-import(ts_util, [cars_on_track/2, move_front_to_back/2, move_back_to_front/2,
  update_tracks/2, print_solution_path/2, new_solution/1, update_solution/2]).
-import(ts_util, [dijkstra_all/1, locate_car/2, list_tracks/1, get_next_tracks/2, dijkstra/2]).
-import(trainswitch, [possible_moves/2, apply_move/2]).
-import(trainswitch, [is_illegal_move/3, expand/2, dls_recursive/3, expand_solution/2,
  depth_limited_search/2, iterative_deepening_search/1]).
-import(ts_astar, [get_neighbors/2, not_on_goal_count/3, new_solution_astar/2, print_astar_solution_path/2,
  expand_astar_solution/3, equal_astar_states/2, insert_astar_state/2,
  astar_search_not_on_goal/1, astar_search_dijkstra/1, dijkstra_sum_heuristic/3,
  out_of_order_score/3]).
-include("../include/trainswitch.hrl").


%% Unit test module
%% ts_unit_test:unit_test().
unit_test() ->
    [engine] = cars_on_track(t1, ?PROBLEM1#problem.init_state),
    [] = cars_on_track(t2, ?PROBLEM5#problem.goal_state),
    [d] = cars_on_track(t2, ?PROBLEM2#problem.init_state),
    [d] = cars_on_track(t4, ?PROBLEM4#problem.init_state),
    [b] = cars_on_track(t3, ?PROBLEM3#problem.init_state),
    [b, c] = cars_on_track(t3, ?PROBLEM4#problem.init_state),
    [c, b] = cars_on_track(t3, ?PROBLEM5#problem.init_state),
    true = is_illegal_move(a, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move([], ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({a}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({left}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({test, test, test}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({left, test, test}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({test, t4, t6}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({left, t4, t6}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({right, t4, t6}, ?PROBLEM1#problem.yard, ?PROBLEM1#problem.init_state),
    true = is_illegal_move({left, t1, t2}, ?PROBLEM5#problem.yard, ?PROBLEM1#problem.goal_state),
    true = is_illegal_move({left, t2, t1}, ?PROBLEM5#problem.yard, ?PROBLEM1#problem.goal_state),
    false = is_illegal_move({left, t2, t1}, ?PROBLEM5#problem.yard, ?PROBLEM1#problem.init_state),
    false = is_illegal_move({right, t1, t2}, ?PROBLEM5#problem.yard, ?PROBLEM1#problem.init_state),
    {[],[b,a]} = move_front_to_back([a], [b]),
    {[b],[a]} = move_front_to_back([a,b], []),
    {[b,c],[d,e,a]} = move_front_to_back([a,b,c], [d, e]),
    {[],[a,b]} = move_back_to_front([a], [b]),
    {[a],[b]} = move_back_to_front([a,b], []),
    {[a,b],[c,d,e]} = move_back_to_front([a,b,c], [d, e]),
    [{t1, [engine]}, {t2, [a]}, {t3, [b]}] = update_tracks([{t1, [engine]}, {t2, [a]}, {t3, [b]}], [] ),
    [{t1, [engine]}, {t2, [a]}, {t3, [c]}] = update_tracks([{t1, [engine]}, {t2, [a]}, {t3, [b]}], [{t3, [c]}] ),
    [{t1, [engine]}, {t2, [b]}, {t3, [c]}] = update_tracks([{t1, [engine]}, {t2, [a]}, {t3, [b]}], [{t2, [b]}, {t3, [c]}] ),
    [{t1, [engine]}, {t2, [a]}] = update_tracks([{t1, [engine]}, {t2, [a]}, {t3, [b]}], [{t3, []}] ),
    [{t1, [engine]}, {t3, [b]}] = update_tracks([{t1, [engine]}, {t2, [a]}, {t3, [b]}], [{t2, []}] ),
    [{t1, [engine]}, {t2, [a, c]}, {t3, [b]}] = update_tracks([{t1, [engine]}, {t2, [a]}, {t3, [b]}], [{t2, [a, c]}] ),
    [{t1, [engine]}, {t2, [a, c]}, {t3, [b]}] = update_tracks([{t1, [engine]}, {t3, [b]}], [{t2, [a, c]}] ),
    [{t1, [engine, a]}, {t3, [b]}] = apply_move({left, t2, t1}, [{t1, [engine]}, {t2, [a]}, {t3, [b]}]),
    [{t1, [a]}, {t3, [b]}] = apply_move({left, t2, t1}, [{t2, [a]}, {t3, [b]}]),
    [{t2, [engine, a]}, {t3, [b]}] = apply_move({right, t1, t2}, [{t1, [engine]}, {t2, [a]}, {t3, [b]}]),
    [{t2, [engine]}, {t3, [b]}] = apply_move({right, t1, t2}, [{t1, [engine]}, {t3, [b]}]),
    [[{t2,[e]},{t3,[engine]},{t4,[b,c,a]},{t6,[d]}],
     [{t1,[engine,e]},{t4,[b,c,a]},{t6,[d]}],
     [{t2,[engine,e]},{t4,[b,c,a]},{t6,[d]}]] =
    expand(?PROBLEM1#problem.init_state, ?PROBLEM1#problem.yard),
    SState1 = new_solution(?PROBLEM3),
    SState2 = update_solution(SState1, {left, t2, t1}),
    SState3 = update_solution(SState2, {left, t3, t1}),
    {found, SState3} = dls_recursive(?PROBLEM3, SState3, 0),
    {not_found, max_depth_reached} = dls_recursive(?PROBLEM3, SState2, 1),
    io:format("SState expansion: ~w~n", [expand_solution(?PROBLEM3, SState2)] ),
    {not_found, max_depth_reached} = depth_limited_search(?PROBLEM3, 0),
    {not_found, out_of_moves} = depth_limited_search(?PROBLEM3, 1),
    {found, PrintState1 = {solution_state,[{t1,[engine,a,b]}], Sol_Path}} =
        depth_limited_search(?PROBLEM3, 2),
	% [{left,t3,t1}, {left,t2,t1}]
	2 = length(Sol_Path),
    print_solution_path(?PROBLEM3, PrintState1),
    {found, _SState} = iterative_deepening_search(?PROBLEM5),
    [t2, t3] = get_neighbors(?PROBLEM1#problem.yard, t1),
    [t1, t6] = get_neighbors(?PROBLEM1#problem.yard, t2),
    [t1, t5] = get_neighbors(?PROBLEM1#problem.yard, t3),
    [t5] = get_neighbors(?PROBLEM1#problem.yard, t4),
    [t3, t4, t6] = get_neighbors(?PROBLEM1#problem.yard, t5),
    [t2, t5] = get_neighbors(?PROBLEM1#problem.yard, t6),
    0 = not_on_goal_count(?PROBLEM1#problem.init_state, ?PROBLEM1#problem.init_state, ?PROBLEM1#problem.yard),
    5 = not_on_goal_count(?PROBLEM1#problem.init_state, ?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.yard),
    5 = not_on_goal_count(?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.init_state, ?PROBLEM1#problem.yard),
    2 = not_on_goal_count(?PROBLEM3#problem.init_state, ?PROBLEM3#problem.goal_state, ?PROBLEM3#problem.yard),
    4 = not_on_goal_count(?PROBLEM4#problem.init_state, ?PROBLEM4#problem.goal_state, ?PROBLEM4#problem.yard),
    HNotOnGoal3 = fun(State) -> ts_astar:not_on_goal_count(State, ?PROBLEM3#problem.goal_state, ?PROBLEM3#problem.yard) end,
    AStarSState1 = new_solution_astar(?PROBLEM3, HNotOnGoal3),
    print_astar_solution_path(?PROBLEM3, AStarSState1 ),
    [FirstExpand | _RestExpand] = expand_astar_solution(?PROBLEM3, AStarSState1, HNotOnGoal3),
    AStarSState2 = AStarSState1#astar_solution_state{fvalue = 100},
    true = equal_astar_states(AStarSState1, AStarSState1),
    true = equal_astar_states(AStarSState1, AStarSState2),
    false = equal_astar_states(AStarSState1, FirstExpand),
    InsertState1 = #astar_solution_state{fvalue = 1},
    InsertState2 = #astar_solution_state{fvalue = 2},
    InsertState3 = #astar_solution_state{fvalue = 3},
    [InsertState1] = insert_astar_state(InsertState1, []),
    [InsertState1, InsertState2] = insert_astar_state(InsertState1, [InsertState2]),
    [InsertState1, InsertState2] = insert_astar_state(InsertState2, [InsertState1]),
    [InsertState1, InsertState2, InsertState3] = insert_astar_state(InsertState2, [InsertState1, InsertState3]),
    [t2, t3] = get_next_tracks(t1, ?PROBLEM1#problem.yard),
    [t1, t6] = get_next_tracks(t2, ?PROBLEM1#problem.yard),
    [t1, t5] = get_next_tracks(t3, ?PROBLEM1#problem.yard),
    [t5] = get_next_tracks(t4, ?PROBLEM1#problem.yard),
    [t1, t2, t3, t5, t4, t6] = list_tracks(?PROBLEM1#problem.yard),
    [t1, t2, t3, t4, t5] = list_tracks(?PROBLEM2#problem.yard),
    [t1, t2, t3] = list_tracks(?PROBLEM3#problem.yard),
    [t1, t2, t3, t4] = list_tracks(?PROBLEM4#problem.yard),
    [t1, t2, t3, t4] = list_tracks(?PROBLEM5#problem.yard),
    [{t1,0},{t2,1},{t3,1},{t6,2},{t5,2},{t4,3}] = dijkstra(t1, ?PROBLEM1#problem.yard),
    [{t1,0},{t2,1},{t3,1}] = dijkstra(t1, ?PROBLEM3#problem.yard),
    t1 = locate_car(engine, ?PROBLEM1#problem.init_state),
    t2 = locate_car(e, ?PROBLEM1#problem.init_state),
    t4 = locate_car(a, ?PROBLEM1#problem.init_state),
    t6 = locate_car(d, ?PROBLEM1#problem.init_state),
    0 = dijkstra_sum_heuristic(?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.yard),
    0 = dijkstra_sum_heuristic(?PROBLEM1#problem.init_state, ?PROBLEM1#problem.init_state, ?PROBLEM1#problem.yard),
    12 = dijkstra_sum_heuristic(?PROBLEM1#problem.init_state, ?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.yard),
    SPath3 = astar_search_not_on_goal(?PROBLEM3),
    SPath3 = astar_search_dijkstra(?PROBLEM3),
    %14 = length(astar_search_dijkstra(?PROBLEM2)),
    0 = out_of_order_score(?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.yard),
    0 = out_of_order_score(?PROBLEM1#problem.init_state, ?PROBLEM1#problem.goal_state, ?PROBLEM1#problem.yard),
    0 = out_of_order_score(?PROBLEM3#problem.goal_state, ?PROBLEM3#problem.goal_state, ?PROBLEM3#problem.yard),
    0 = out_of_order_score(?PROBLEM3#problem.init_state, ?PROBLEM3#problem.goal_state, ?PROBLEM3#problem.yard),
    0 = out_of_order_score([{t1, [engine, a, b, c, d]}], [{t1, [engine, a, b, c, d]}], ?PROBLEM3#problem.yard),
    2 = out_of_order_score([{t1, [a]}], [{t1, [engine, a, b, c, d]}], ?PROBLEM3#problem.yard),
    10 = out_of_order_score([{t1, [a, b, c, d, engine]}], [{t1, [engine, a, b, c, d]}], ?PROBLEM3#problem.yard),
    4 = out_of_order_score([{t1, [engine, a, c, b, d]}], [{t1, [engine, a, b, c, d]}], ?PROBLEM3#problem.yard),
    2 = out_of_order_score([{t1, [engine, a, b, d]}], [{t1, [engine, a, b, c, d]}], ?PROBLEM3#problem.yard),
    0 = out_of_order_score([{t1, [e, a, b, c, d]}], [{t1, [engine, a, b, c, d]}], ?PROBLEM3#problem.yard),
    io:format("Unit tests passed to here~n"),
    test_passed.


test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).

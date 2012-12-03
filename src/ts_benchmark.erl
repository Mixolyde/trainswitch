%%%----FILE ts_unit_test----
-module(ts_benchmark).

-compile([debug_info, export_all]).

-import(trainswitch, [depth_limited_search/2, iterative_deepening_search/1]).
-import(ts_astar, [astar_search_not_on_goal/1, astar_search_dijkstra/1]).

-include("../include/trainswitch.hrl").

% list of benchmark scenarios to run
% {string name, module, function, arguments, number of times to run}
-define(BENCHMARKS,
   [{"Depth Limited Problem 4",       trainswitch, depth_limited_search,       [?PROBLEM4, 4], 5},
    {"Iterative Deepening Problem 4", trainswitch, iterative_deepening_search, [?PROBLEM4], 5},
    {"Not on Goal Problem 4",         ts_astar, astar_search_not_on_goal,      [?PROBLEM4], 5},
    {"Dijkstra Problem 4",            ts_astar, astar_search_dijkstra,         [?PROBLEM4], 5},
    {"Depth Limited Problem 5",       trainswitch, depth_limited_search,       [?PROBLEM5, 6], 5},
    {"Iterative Deepening Problem 5", trainswitch, iterative_deepening_search, [?PROBLEM5], 5},
    {"Not on Goal Problem 5",         ts_astar, astar_search_not_on_goal,      [?PROBLEM5], 5},
    {"Dijkstra Problem 5",            ts_astar, astar_search_dijkstra,         [?PROBLEM5], 5},
    {"Not on Goal Problem 2",         ts_astar, astar_search_not_on_goal,      [?PROBLEM2], 3},
    {"Dijkstra Problem 2",            ts_astar, astar_search_dijkstra,         [?PROBLEM2], 3},
   %{"Not on Goal Problem 1",         ts_astar, astar_search_not_on_goal,      [?PROBLEM1], 1},
    {"Dijkstra Problem 1",            ts_astar, astar_search_dijkstra,         [?PROBLEM1], 1}
   ]).

% run each benchmark scenario through test_avg, collect results, print in a table
benchmark() ->
     Results = [{String, test_avg(Mod, Fun, Args, Num)} || {String, Mod, Fun, Args, Num} <- ?BENCHMARKS],
     io:format("~35s~12s~12s~12s~12s~n", ["Name", "Min", "Max", "Median", "Average"]),
     lists:map(fun({String, [Min, Max, Med, Avg]}) ->
       io:format("~35s~12b~12b~12b~12b~n", [String, Min, Max, Med, Avg]) end, Results).

% run Module:Function(Arguments) N times with timing and return results
test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n" "Median: ~b mics~n" "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    [Min, Max, Med, Avg].

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).


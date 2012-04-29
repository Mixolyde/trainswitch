%%%----FILE dfs_proc.erl----

%%% a thread spawning depth-first search algorithm
%%% spawn a thread for each next state and test them all
%%% not consistent or precisely reproducable
-module(dls_proc).
-include("trainswitch.hrl").

-compile([debug_info, export_all]).

dls_start(Problem, Limit) ->
    io:format("Starting depth limited search with process~n"),
    io:format("Supervisor process: ~p~n", [self()]),
    FirstWorker = spawn(trainswitch, dls_process, [Problem, ts_util:new_solution(Problem), Limit, self()]),
    io:format("Created first worker process: ~p~n", [FirstWorker]),
    receive
        {stop, Child_id} ->
            io:format("Received stop message from ~p~n", [Child_id]);
        {found, SState, Child_id} ->
            io:format("Received found state from ~p~n", [Child_id]),
            {found, SState};
        {not_found, max_depth_reached, Child_id} ->
            io:format("Received max depth reached state from ~p~n", [Child_id]);
        {not_found, expanding, Child_id} ->
            io:format("Received expand and deepen state from ~p~n", [Child_id]);
        Any ->
            Any
    after 5000 ->
        io:format("dls_start ~w timed out~n", [self()])
    end.


%first try to match the goal state
dls_process(
    #problem{goal_state = Goal},
    #solution_state{state = Goal} = SState,
    _Limit, Parent_id) ->
    %io:format("Goals match: ~w~n", [Goal] ),
    Parent_id ! {found, SState, self()};
%then see if we hit bottom
dls_process(_Problem,
    #solution_state{depth = Depth}, Limit, Parent_id) when Depth >= Limit ->
    Parent_id ! {not_found, max_depth_reached, self()};
%else generate the next possible states and iterate through them
dls_process(Problem, State, Limit, Parent_id) ->
    %expand the solution with all possible moves and go deeper
    New_SStates = ts_util:expand_solution(Problem, State),
    %spawn a process for each new state and loop through receives
    Child_Processes = lists:map(
        fun(New_State) ->
            spawn_link(trainswitch, dls_process, [Problem, New_State, Limit, self()])
        end, New_SStates),
    %io:format("Creating child procs: ~p~n", [Child_Processes]),
    dls_loop(Parent_id, Child_Processes).

dls_loop(Parent, []) ->
    %io:format("tell parent we're out of workers~n"),
    Parent ! {not_found, out_of_moves, self()};
dls_loop(Parent, Workers) ->
    receive
        {found, SState, Pid} ->
            io:format("%kill child processes and return found state~n"),
            WorkingProcs = lists:filter(
                fun(Process) -> Process /= Pid end, Workers ),
            lists:map ( fun(Process) -> Process ! {die, self()} end, WorkingProcs ),
            Parent ! {found, SState, self() };
        {not_found, _Reason, Child_id} ->
            %io:format("removing child process from list: ~p~n", [Child_id]),
            dls_loop(Parent, lists:filter(
                fun(Process) -> Process /= Child_id end, Workers ) );
        {die, Parent} ->
            exit(die_message);
        Any ->
            io:format("%received unrecognized message: ~w~n", [Any]),
            exit(unrecognized_message)

    end.
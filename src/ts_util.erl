%%%----FILE ts_util.hrml----
-module(ts_util).
-include("../include/trainswitch.hrl").

-compile([debug_info, export_all]).

%%% train yard problem helper functions

%%% data structure utils
%%trainswitch:cars_on_track(t1, Yard1#problem.init_state).
cars_on_track(Track, TrackList) ->
    %keysearch for a {Track, Cars} tuple in the list
    case lists:keyfind(Track, 1, TrackList) of
        false -> [];
        {_Track, Cars} -> Cars %try to bind track and return its cars
    end.

new_solution(Problem) ->
    #solution_state{state = Problem#problem.init_state,
        moves = []} .

update_solution(Solution_state, Move) ->
    #solution_state{
        state = trainswitch:apply_move(Move, Solution_state#solution_state.state),
        moves = [Move | Solution_state#solution_state.moves]} .

%% move the first element of front to the last element of back and return both new lists
move_front_to_back([Front | Rest], Back ) ->
    {Rest, Back ++ [Front] }.

%%move the last element of front to the first element of back and return both new lists
move_back_to_front(Front, Back) ->
    {lists:sublist(Front, length(Front) - 1), [lists:last(Front)] ++ Back}.

%%take a list of tuples like {t1, [a,b]} and a state, update tracks, return new state
update_tracks(State, []) ->
    %io:format("No Updates to make, returning input state: ~w~n", [State]),
    lists:keysort(1,State);
update_tracks(State, [{UpdateTrack, UpdateList} | RestUpdate]) ->
    %io:format("Updating state ~w with track ~w~n", [State, {UpdateTrack, UpdateList}]),
    case UpdateList of
        [] ->
            %delete the track key and recurse
            %io:format("Deleting track ~w and recursing~n", [UpdateTrack]),
            update_tracks(lists:keydelete(UpdateTrack, 1, State ), RestUpdate);
        _CarsOnTrack ->
            %replace the track key and recurse
            %io:format("Replacing with track ~w~n", [{UpdateTrack, UpdateList}]),
            update_tracks(lists:keystore(UpdateTrack, 1, State, {UpdateTrack, UpdateList}), RestUpdate)
    end.

%%print the path of moves that lead from the initial state to the current one in this solution step
print_solution_path(Problem, Solution_state) ->
    io:format("Solution state at depth: ~w~n", [length(Solution_state#solution_state.moves)]),
    io:format("To go from ~w to~n", [Problem#problem.init_state]),
    io:format("~w~n", [Solution_state#solution_state.state]),
    io:format("Apply: ~w~n", [lists:reverse(Solution_state#solution_state.moves)]).

%% returns a list of all the tracks in a yard
%% {t1, t2}, {t2, t3} -> [t1, t2, t3]
list_tracks(Yard) -> list_tracks(Yard, []).
list_tracks([], Acc) -> lists:reverse(Acc);
list_tracks([{Left, Right} | RestYard ], Acc) ->
    LeftMember = lists:member(Left, Acc),
    RightMember = lists:member(Right, Acc),
    if
        LeftMember and RightMember ->
            list_tracks(RestYard, Acc);
        LeftMember and not RightMember->
            list_tracks(RestYard, [Right | Acc]);
        not LeftMember and RightMember ->
            list_tracks(RestYard, [Left | Acc]);
        not LeftMember and not RightMember ->
            list_tracks(RestYard, [Right] ++ [Left] ++ Acc)
    end.



%%% Dijkstra algorithm stuff

%% returns a dijkstra distance list for each track in the yard
dijkstra_all(Yard) ->
    Tracks = list_tracks(Yard),
    lists:map(fun(Track) -> {Track, dijkstra(Track, Yard)} end, Tracks).

%% Returns a key list of distances from the specified track
%% to every other track in the state
%% i.e.  track t1's distances are [{t1, 0}, {t2, 1}, {t3, 1} ...]
dijkstra(Track, Yard) ->
    TracksToCount = list_tracks(Yard) -- [Track],
    StartDistances = [{Track, 0}],
    Open = lists:map(fun(EachTrack) -> {EachTrack, 1} end, get_next_tracks(Track, Yard)),
    %%recursively get the distance to all of the other tracks
    dijkstra(Open, TracksToCount, StartDistances, Yard).
%% base case - no tracks left
dijkstra(_Open, [], Distances, _Yard) -> lists:reverse(Distances);
dijkstra([{CurrentTrack, CurrentDistance} = CurrentOpen | RestOpen],
    TracksToCount, Distances, Yard) ->
    % add current distance node to the distance list
    NewDistances = [CurrentOpen | Distances],
    NewTracksToCount = TracksToCount -- [CurrentTrack],
    % get the current node's neighbors
    CurrentNeighbors = lists:map(fun(Track) -> {Track, CurrentDistance + 1} end, get_next_tracks(CurrentTrack, Yard)),
    % foreach neighbor node check to see if it's in the lists
    NewOpen = dijkstra_neighbor(CurrentNeighbors, RestOpen, NewDistances),
    dijkstra(NewOpen, NewTracksToCount, NewDistances, Yard).

dijkstra_neighbor([], Open, _Distances) -> Open;
dijkstra_neighbor([{NeighborTrack, _NeighborDistance} = Neighbor | RestNeighbors], Open, Distances) ->
    case not lists:keymember(NeighborTrack, 1, Open) and
        not lists:keymember(NeighborTrack, 1, Distances) of
        true ->
            %add to new open list and recurse
            dijkstra_neighbor(RestNeighbors, Open ++ [Neighbor], Distances);
        false ->
            %just recurse
            dijkstra_neighbor(RestNeighbors, Open, Distances)
    end.




%% Get a list of neighboring tracks to this track
%% i.e. - track t1's neighbors are [t2 t3]
%% not tail recursive
% base case
get_next_tracks(_Track, []) -> [];
get_next_tracks(Track, [{Track, Right} | RestYard]) ->
    [Right] ++ get_next_tracks(Track, RestYard);
get_next_tracks(Track, [{Left, Track} | RestYard]) ->
    [Left] ++ get_next_tracks(Track, RestYard);
get_next_tracks(Track, [_UnmatchedLink | RestYard]) ->
    get_next_tracks(Track, RestYard).

locate_car(_Car, []) -> {error, car_not_found};
locate_car(Car, [{Track, CarList} |Rest]) ->
    case lists:member(Car, CarList) of
        true -> Track;
        false -> locate_car(Car, Rest)
    end.
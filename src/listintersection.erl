-module(listintersection).

-compile([debug_info, export_all]).

% Generates a list of integers of length Size. Each integer is
% evenly distributed from 1 to Max.
gen_list(Max, Size) ->
  gen_list(Max, Size, []).

gen_list(_Max, 0, List) -> List;
gen_list(Max, Size, List) ->
  Value = random:uniform(Max),
  gen_list(Max, Size - 1, [Value | List]).

% Returns the intersection of two lists. The intersection is a
% list of {Value, Count} records, where Value is the integer
% appearing in both lists, and Count is the minimum of the amount
% of times it appears in both lists.
% intersection([1, 1, 1, 2, 2], [ 1, 1, 2, 3]) -> [{1, 2}, {2, 1}].
intersection(List1, List2) ->
  % build hash maps of the two lists
  Hash1 = build_hash(List1),
  Hash2 = build_hash(List2),
  Hash1IsLarger = dict:size(Hash1) > dict:size(Hash2),
  if
    Hash1IsLarger ->
      intersection_hashes(Hash1, Hash2);
    true ->
      intersection_hashes(Hash2, Hash1)
  end.

% Takes the two hash maps and finds all the common entries
intersection_hashes(BiggerDict, SmallerDict) ->
  % io:format("SmallerHash: ~p~n", [SmallerDict]),
  % io:format("BiggerHash: ~p~n", [BiggerDict]),

  % for each key in the smaller dict, return the min of its value and the
  % matching value in the other hash, if it's there
  % the fold method applies a function to each key/value pair in the hash with
  % an accumulator
  dict:fold(
  % fun(Key, Value, Accumulator)
    fun(SmallerKey, SmallerValue, Intersection) ->
      case dict:find(SmallerKey, BiggerDict) of
        {ok, BiggerValue} ->
            % if we find the key in the larger hash, add a tuple to the
            % intersection list
            [{SmallerKey, min(SmallerValue, BiggerValue)} | Intersection];
        error ->
      % no match found, return the accumulator unchanged
            Intersection
      end
    end, [], SmallerDict). % start with an empty accumulator and iterate over
                           % the smaller map's keys

% takes a list of ints and builds up a hash of Key/Count pairs
build_hash(List) ->
  Dict = dict:new(),

  % iterate over the list items, building up the hash map
  lists:foldl(
    fun(Elem, AccDict) ->
      % for each element in the list, initialize its value in the hash to 1,
      % or update the old value by adding 1
      dict:update(Elem, fun(Old) -> Old + 1 end, 1, AccDict)
    end,
    Dict, List). % start with a new Dict


% unit tests with hard-coded data and random generated data
intersection_test() ->
  TestList1 = [1, 2, 3, 4, 5],
  TestList2 = [6, 7, 8, 9, 10],
  TestList3 = [1, 2, 6, 7, 15, 16],
  5 = length(intersection(TestList1, TestList1)),
  % "++" is the list append operator
  5 = length(intersection(TestList1, TestList1 ++ TestList2)),
  0 = length(intersection(TestList1, TestList2)),
  2 = length(intersection(TestList1, TestList3)),

  % should find 2 "1's" and 2 "2's" in this test
  Result1 = intersection(TestList1 ++ TestList1,
             TestList3 ++ TestList3 ++ TestList3),
  2 = length(Result1),
  {1, 2} = lists:keyfind(1, 1, Result1),
  {2, 2} = lists:keyfind(2, 1, Result1),

  2 = length(intersection(TestList3 ++ TestList3 ++ TestList3,
              TestList1 ++ TestList1)),
  2 = length(intersection(TestList2, TestList3)),
  4 = length(intersection(TestList1 ++ TestList2, TestList3)),
  4 = length(intersection(TestList3, TestList2 ++ TestList1)),

  % random data tests
  List1 = gen_list(20, 41),
  List2 = gen_list(20, 40),
  io:format("Random Lists: ~p~n~p~n", [List1, List2]),
  List3 = intersection(List1, List2),
  List4 = intersection(List2, List1),
  Length = length(List3),
  Length = length(List4).

% large data set test with two 1 million random integer lists
big_test() ->
  List1 = gen_list(250000, 1000000),
  List2 = gen_list(250000, 1000000),
  %io:format("Random Lists: ~p~n~p~n", [List1, List2]),
  List3 = intersection(List1, List2),
  List3.

bench() ->
  ts_benchmark:test_avg(listintersection, big_test, [], 5).
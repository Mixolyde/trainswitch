%% lisp practice methods
%% cd ("c:/Documents and Settings/Owner/My Documents/My Dropbox/dev/erl").
%% c(lispprac1).
-module(lispprac1).
-author("Brian E. Williams").
-compile([debug_info, export_all]).

palinedromep(List) when is_list(List) ->
   List == lists:reverse(List);
palinedromep(_) ->
    false.

presentp(Atom, Tuple) when is_atom(Atom), is_tuple(Tuple) ->
    presentp(Atom,tuple_to_list(Tuple));
presentp(Atom, [Head | Rest]) when is_atom(Atom), is_list(Head) ->
    presentp(Atom,Head) orelse presentp(Atom, Rest);
presentp(Atom, [Head | Rest]) when is_atom(Atom) ->
    Atom == Head orelse presentp(Atom, Rest);
presentp(_, _) -> false.

duplicate_entries(Thing) when not is_list(Thing) -> false;
duplicate_entries([]) -> false;
duplicate_entries([_]) -> false;
duplicate_entries([Head | Rest] ) -> 
    lists:member(Head, Rest) orelse duplicate_entries(Rest).
    
    
unit_test() ->
    true = palinedromep([]),
    true = palinedromep([a]),
    true = palinedromep([a, a]),
    true = palinedromep([a, b, a]),
    true = palinedromep([a, b, b, a]),
    false = palinedromep([a, b]),
    false = palinedromep([a, b, b]),
    false = palinedromep(a),
    false = palinedromep({a, b}),
    true = presentp(a, [a]),
    true = presentp(a, [b, a]),
    true = presentp(a, [a, b, b, c]),
    true = presentp(a, {a}),
    true = presentp(a, {b, a}),
    true = presentp(a, [b, [a]]),
    true = presentp(a, [b, [b, [b, a]]]),
    false = presentp(a, [b]),
    false = presentp(a, b),
    false = presentp(a, [b, [b, c]]),
    false = presentp(a, {b, b, c}),
    false = presentp(a, {b, {b, c}}),
    true = duplicate_entries([a, a]),
    true = duplicate_entries([a, b, a]),
    true = duplicate_entries([a, b, c, b]),
    true = duplicate_entries([[a], b, [a]]),
    true = duplicate_entries([[a, b], b, b, [a]]),
    true = duplicate_entries([[a, b], b, c, [a, b]]),
    false = duplicate_entries([a, b]),
    false = duplicate_entries([[a], b]),
    false = duplicate_entries(a),
    false = duplicate_entries({a}),
    false = duplicate_entries([a, b, c]),
    false = duplicate_entries([a]),
    tests_passed.
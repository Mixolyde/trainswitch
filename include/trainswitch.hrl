%%%----FILE trainswitch.hrl----

%%% train yard data structures
-record(yard, {track_list}).
-record(state, {yard_state}).

%%% problem structure
-record(problem, {yard = #yard{}, init_state = #state{}, goal_state = #state{}}).

%%% solution states
-record(solution_state, {state = #state{}, moves}).
-record(astar_solution_state, {solution_state = #solution_state{}, fvalue}).

%%% problems

-define(PROBLEM1,
    #problem{yard = [{t1, t2}, {t1, t3}, {t3, t5}, {t4, t5}, {t2, t6}, {t5, t6}],
        init_state = [{t1, [engine]}, {t2, [e]}, {t4, [b, c, a]}, {t6, [d]}],
        goal_state = [{t1, [engine, a, b, c, d, e]}] }).

-define(PROBLEM2,
    #problem{yard = [{t1, t2}, {t2, t3}, {t2, t4}, {t1, t5}],
        init_state = [{t1, [engine]}, {t2, [d]}, {t3, [b]}, {t4, [a, e]}, {t5, [c]}],
        goal_state = [{t1, [engine, a, b, c, d, e]}] }) .

-define(PROBLEM3,
    #problem{yard = [{t1, t2}, {t1, t3}],
        init_state = [{t1, [engine]}, {t2, [a]}, {t3, [b]}],
        goal_state = [{t1, [engine, a, b]}] } ).

-define(PROBLEM4,
    #problem{yard = [{t1, t2}, {t1, t3}, {t1, t4}],
        init_state = [{t1, [engine]}, {t2, [a]}, {t3, [b, c]}, {t4, [d]}],
        goal_state = [{t1, [engine, a, b, c, d]}] } ).

-define(PROBLEM5,
    #problem{yard = [{t1, t2}, {t1, t3}, {t1, t4}],
        init_state = [{t1, [engine]}, {t2, [a]}, {t3, [c, b]}, {t4, [d]}], %note c and b are out of order
        goal_state = [{t1, [engine, a, b, c, d]}] } ).


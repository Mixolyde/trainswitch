Trainswitch in Erlang
=====================

Erlang solution to a fun Artifical Intelligence homework assignment

The assignment can be found online [here](http://www.cis.udel.edu/~decker/courses/681s07/prog1.html).

Running
=======

To run from the Erlang shell:
 1. `cd("path/to/repo/directory/trainswitch").`
 2. `code:add_path("ebin").`
 3. `make:all([load]).`
 4. `ts_unit_test:unit_test().`
 5. `ts_benchmark:benchmark().`

Files
=====
Data structures and assignment problems are defined in 
`trainswitch.hrl`.

`trainswitch.erl` has some of the utility functions that build-up to the solution as part
of the homework assignment, and the iterative-deepening search that works well for
the first few small problems.

`ts_util.erl` contains some more utility methods for manipulating data structures.

`ts_astar.erl` has the A* heuristic solution which solves the big yards quite quickly,
using a combination of dijkstra's algorithm for calculating distances from tracks
and an out of order check that penalizes moves that would put cars on a track out of
order from the goal state.

`ts_unit_test.erl` contains a large list of unit test style assert statementss for just 
about every method in the system.

`ts_benchmark.erl` contains a few methods for running the searches and printing some timing
benchmark results.

`dls_proc.erl` was an attempt to solve the problem in a distributed fashion, but it hits the
maximum process count of the system pretty early, not a good idea. Plan to refactor
some day.

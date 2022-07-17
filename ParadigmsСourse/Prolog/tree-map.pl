map_build([], []) :- !.
map_build(ListMap, TreeMap) :- length(ListMap, Q), build(0, Q, ListMap, TreeMap).

map_get([(K, V), _, _], K, V) :- !.
map_get([], _, -1) :- !.
map_get([(K, V), Left, Right], Key, Value) :- K > Key, !, map_get(Left, Key, Value).
map_get([(K, V), Left, Right], Key, Value) :- K < Key, !, map_get(Right, Key, Value).


build(L, L, _, []) :- !.
build(L, R, ListMap, [M, [], []]) :- 1 is R - L, element(R, ListMap, M), !.
build(L, R, ListMap, [M, Left, Right]) :- Sum is L + R, Mid is div(Sum, 2), Temp is Mid + 1, element(Temp, ListMap, M),
																					build(L, Mid, ListMap, Left), build(Temp, R, ListMap, Right).

map_replace([], _, _, []) :- !.
map_replace([(Key, V), Left, Right], Key, Value, [(Key, Value), Left, Right]) :- !.
map_replace([(K, V), Left, Right], Key, Value, [(K, V), R, Right]) :- K > Key, !, map_replace(Left, Key, Value, R).
map_replace([(K, V), Left, Right], Key, Value, [(K, V), Left, R]) :- K < Key, !, map_replace(Right, Key, Value, R).
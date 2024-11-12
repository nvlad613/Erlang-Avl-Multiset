-module(main).

-export([run/0]).

run() ->
    Multiset = multiset:from_list([100, 10, 83, 1, 83, 10]),
    Filtered = multiset:filter(
        Multiset,
        fun({_, Count}) -> Count == 1 end
    ),
    multiset:to_list(Filtered).

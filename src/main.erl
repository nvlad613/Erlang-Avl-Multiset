-module(main).

-export([run/0]).

run() ->
    MSet = multiset:from_list([1, 243, 2453]),
    Neutral = multiset:new(),
    multiset:equals(multiset:merge(MSet, Neutral), MSet) andalso
        multiset:equals(multiset:merge(Neutral, MSet), MSet).

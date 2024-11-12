-module(multiset_unit_test_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    from_list_test/1,
    add_test/1,
    delete_test/1,
    fold_test/1,
    filter_test/1
]).

all() ->
    [
        from_list_test,
        add_test,
        delete_test,
        fold_test,
        filter_test
    ].

from_list_test(_) ->
    Multiset = multiset:from_list(["a", "b", "b", "c"]),
    ?assert(multiset:find(Multiset, "a") == 1),
    ?assert(multiset:find(Multiset, "b") == 2),
    ?assert(multiset:find(Multiset, "c") == 1),
    ?assert(multiset:find(Multiset, "g") == 0).

add_test(_) ->
    Multiset = multiset:from_list(["a", "b", "b", "c"]),
    ?assert(multiset:find(Multiset, "g") == 0),
    Multiset2 = multiset:add(Multiset, "g"),
    ?assert(multiset:find(Multiset2, "g") == 1),
    Multiset3 = multiset:add(Multiset, "a", 10),
    ?assert(multiset:find(Multiset3, "a") == 11).

delete_test(_) ->
    Multiset = multiset:from_list([
        "apple", "mango", "papaya", "pineapple", "pineapple", "pineapple"
    ]),
    ?assert(multiset:find(Multiset, "mango") == 1),
    Multiset2 = multiset:delete(Multiset, "mango"),
    ?assert(multiset:find(Multiset2, "mango") == 0),
    ?assert(multiset:find(Multiset, "pineapple") == 3),
    Multiset3 = multiset:delete(Multiset, "pineapple"),
    ?assert(multiset:find(Multiset3, "pineapple") == 2).

fold_test(_) ->
    Multiset = multiset:from_list([100, 10, -5, 1]),
    Sum = multiset:fold(Multiset, 0, fun(Acc, {X, _}) -> Acc + X end),
    Prod = multiset:fold(Multiset, 1, fun(Acc, {X, _}) -> Acc * X end),
    ?assert(Sum == 106),
    ?assert(Prod == -5000).

filter_test(_) ->
    Multiset = multiset:from_list([100, 10, 83, 1, 83, 10]),
    Filtered = multiset:filter(
        Multiset,
        fun({_, Count}) -> Count == 1 end
    ),
    WithoutDuplicates = multiset:from_list([100, 1]),
    ?assert(multiset:equals(WithoutDuplicates, Filtered)).

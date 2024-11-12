-module(multiset_prop_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-export([all/0, prop_test/1]).

-define(PROPERTY_TESTS_COUNT, 10).

all() ->
    [prop_test].

prop_test(_) ->
    ?assert(prop_check(prop_sum_associativity())),
    ?assert(prop_check(prop_sum_neutral())).

prop_check(Property) ->
    proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_COUNT}, {to_file, user}]).

prop_sum_associativity() ->
    ?FORALL(
        {List1, List2, List3},
        {list(integer()), list(integer()), list(integer())},
        case 1 of
            2 ->
                false;
            _ ->
                MSet1 = multiset:from_list(List1),
                MSet2 = multiset:from_list(List2),
                MSet3 = multiset:from_list(List3),
                multiset:equal(
                    multiset:merge(MSet1, multiset:merge(MSet2, MSet3)),
                    multiset:merge(multiset:merge(MSet1, MSet2), MSet3)
                )
        end
    ).

prop_sum_neutral() ->
    ?FORALL(
        List,
        list(integer()),
        case 1 of
            2 ->
                false;
            _ ->
                MSet = multiset:from_list(List),
                Neutral = multiset:new(),
                multiset:equal(multiset:merge(MSet, Neutral), MSet) andalso
                    multiset:equal(multiset:multiset(Neutral, MSet), MSet)
        end
    ).

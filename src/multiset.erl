-module(multiset).

-export([new/0, add/2, add/3, delete/2, find/2, size/1, sorted/1, equals/2, merge/2]).

-record(multiset, {
    tree :: avl:avlnode(),
    size = 0 :: integer()
}).

-type multiset() :: #multiset{}.

new() ->
    #multiset{tree = avl:new()}.

find(Self, Item) ->
    Found = avl:find(Self#multiset.tree, Item),
    case is_integer(Found) of
        true -> Found;
        false -> 0
    end.

add(Self, Item) ->
    add(Self, Item, 1).

add(Self, Item, Count) ->
    Found = find(Self, Item),
    Self#multiset{
        tree = avl:upsert(Self#multiset.tree, Item, Found + Count),
        size = Self#multiset.size + Count
    }.

delete(Self, Item) ->
    Found = find(Self, Item),
    Self#multiset{
        tree = avl:delete(Self#multiset.tree, Item),
        size = Self#multiset.size - Found
    }.

size(#multiset{size = Size}) ->
    Size.

sorted(#multiset{tree = Tree}) ->
    lists:map(
        fun({Key, Number}) -> lists:duplicate(Number, Key) end,
        avl:sorted(Tree)
    ).

equals(SetA, SetB) when not is_record(SetA, multiset); not is_record(SetB, multiset) ->
    false;
equals(#multiset{tree = TreeA}, #multiset{tree = TreeB}) ->
    avl:sorted(TreeA) =:= avl:sorted(TreeB).

merge(SetA, SetB) ->
    {AvlBUpdated, Leaf} = avl:pop_leaf(SetB#multiset.tree),
    SetBUpdated = SetB#multiset{
        tree = AvlBUpdated
    },
    merge(
        add(SetA, Leaf),
        SetBUpdated
    ).

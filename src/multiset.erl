-module(multiset).

-export([
    new/0,
    add/2, add/3,
    delete/2,
    find/2,
    size/1,
    sorted/1,
    equals/2,
    merge/2,
    fold/3,
    map/2,
    filter/2
]).

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

fold(#multiset{tree = Tree}, Acc, Func) ->
    avl:fold(Tree, Acc, Func).

merge(SetA, SetB) ->
    fold(
        SetB,
        SetA,
        fun(Self, {Key, Value}) -> add(Self, Key, Value) end
    ).

map(Self, Func) ->
    fold(
        Self,
        [],
        fun(Acc, Value) -> [Func(Value) | Acc] end
    ).

filter_builtin(Value, Acc, Func) ->
    case Func(Acc, Value) of
        true -> [Value | Acc];
        false -> Acc
    end.

filter(Self, Func) ->
    fold(
        Self,
        [],
        fun(Acc, Value) -> filter_builtin(Value, Acc, Func) end
    ).

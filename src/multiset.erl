-module(multiset).

-export([
    new/0,
    from_list/1,
    add/2, add/3,
    delete/2,
    find/2,
    size/1,
    equals/2,
    merge/2,
    fold/3,
    map/2,
    filter/2,
    to_list/1
]).

-record(multiset, {
    tree :: avl:avlnode(),
    size = 0 :: integer()
}).

% -type multiset() :: #multiset{}.

new() ->
    #multiset{tree = none}.

from_list(List) ->
    lists:foldl(
        fun(El, Acc) -> add(Acc, El) end,
        new(),
        List
    ).

find(#multiset{tree = none}, _) ->
    0;
find(Self, Item) ->
    Found = avl:find(Self#multiset.tree, Item),
    case is_integer(Found) of
        true -> Found;
        false -> 0
    end.

add(Self, Item) ->
    add(Self, Item, 1).

add(#multiset{tree = none}, Item, Count) ->
    #multiset{tree = avl:new(Item, Count), size = Count};
add(Self, Item, Count) ->
    Found = find(Self, Item),
    Self#multiset{
        tree = avl:upsert(Self#multiset.tree, Item, Found + Count),
        size = Self#multiset.size + Count
    }.

delete(#multiset{tree = none}, _) ->
    new();
delete(Self, Item) ->
    Found = find(Self, Item),
    case Found of
        0 ->
            Self;
        1 ->
            Self#multiset{
                tree = avl:delete(Self#multiset.tree, Item),
                size = Self#multiset.size - Found
            };
        _ ->
            add(Self, Item, -1)
    end.

size(#multiset{size = Size}) ->
    Size.

equals(#multiset{tree = none}, #multiset{tree = none}) ->
    true;
equals(#multiset{tree = none}, _) ->
    false;
equals(_, #multiset{tree = none}) ->
    false;
% equals(SetA, SetB) when not is_record(SetA, multiset); not is_record(SetB, multiset) ->
%     false;
equals(#multiset{tree = TreeA}, #multiset{tree = TreeB}) ->
    avl:sorted(TreeA) =:= avl:sorted(TreeB).

fold(#multiset{tree = none}, Acc, _) ->
    Acc;
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
        new(),
        fun(Acc, Value) -> add(Acc, Func(Value)) end
    ).

filter_builtin({Key, Count}, Acc, Func) ->
    case Func({Key, Count}) of
        true -> add(Acc, Key);
        false -> Acc
    end.

filter(Self, Func) ->
    fold(
        Self,
        new(),
        fun(Acc, Value) -> filter_builtin(Value, Acc, Func) end
    ).

to_list(Self) ->
    fold(
        Self,
        [],
        fun(Acc, Value) -> [Value | Acc] end
    ).

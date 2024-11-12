-module(avl).

-export_types(avlnode/0).
-export([new/2, upsert/3, delete/2, find/2, sorted/1, fold/3, pop_leaf/1]).

-record(node, {
    key :: any(),
    value :: any(),
    l_tree = none :: avlnode(),
    r_tree = none :: avlnode(),
    l_height = 1 :: integer(),
    r_height = 1 :: integer()
}).

-type avlnode() :: #node{} | none().

new(Key, Value) ->
    #node{key = Key, value = Value}.

height(#node{l_height = LHeight, r_height = RHeight}) ->
    max(LHeight, RHeight);
height(none) ->
    0.

factor(#node{l_height = LHeight, r_height = RHeight}) ->
    LHeight - RHeight.

set_left(Node, Child) ->
    Node#node{
        l_tree = Child,
        l_height = height(Child) + 1
    }.

set_right(Node, Child) ->
    Node#node{
        r_tree = Child,
        r_height = height(Child) + 1
    }.

upsert(Root, Key, Value) ->
    upsert(Root, new(Key, Value)).

upsert(Parent, Node) when Parent#node.key > Node#node.key ->
    append_left(Parent, Node);
upsert(Parent, Node) when Parent#node.key < Node#node.key ->
    append_right(Parent, Node);
upsert(Parent, Node) when Parent#node.key =:= Node#node.key ->
    Parent#node{value = Node#node.value}.

% Insert in left leaf
% No rotations needed
append_left(Parent, Node) when Parent#node.l_tree =:= none ->
    set_left(Parent, Node);
% Insert in left child recursive
% Rotate if not factor > 1
append_left(Parent, Node) ->
    Child = upsert(Parent#node.l_tree, Node),
    ParentUpdated = set_left(Parent, Child),
    case factor(ParentUpdated) > 1 of
        true -> make_LL_or_LR(ParentUpdated);
        false -> ParentUpdated
    end.

% Insert in right leaf
% No rotations needed
append_right(Parent, Node) when Parent#node.r_tree =:= none ->
    set_right(Parent, Node);
% Insert in right child recursive
% Rotate if not factor < -1
append_right(Parent, Node) ->
    Child = upsert(Parent#node.r_tree, Node),
    ParentUpdated = set_right(Parent, Child),
    case factor(ParentUpdated) < -1 of
        true -> make_RR_or_RL(ParentUpdated);
        false -> ParentUpdated
    end.

% depends on factor:
% LL -> rotate_right(ROOT)
% LR -> rotate_left(LEFT_CHILD), rotate_right(ROOT)
make_LL_or_LR(Root) ->
    case factor(Root#node.l_tree) > 0 of
        true ->
            rotate_right(Root);
        false ->
            rotate_right(
                set_left(Root, rotate_left(Root#node.l_tree))
            )
    end.

% depends on factor:
% RR -> rotate_left(ROOT)
% RL -> rotate_right(RIGHT_CHILD), rotate_left(ROOT)
make_RR_or_RL(Root) ->
    case factor(Root#node.r_tree) < 0 of
        true ->
            rotate_left(Root);
        false ->
            rotate_left(
                set_right(Root, rotate_right(Root#node.r_tree))
            )
    end.

rotate_right(Root) ->
    PivotR = set_left(Root, Root#node.l_tree#node.r_tree),
    set_right(Root#node.l_tree, PivotR).

rotate_left(Root) ->
    PivotL = set_right(Root, Root#node.r_tree#node.l_tree),
    set_left(Root#node.r_tree, PivotL).

% search left
delete(Node, Key) when Key < Node#node.key ->
    UpdatedChild = delete(Node#node.l_tree, Key),
    rebalance(Node#node{
        l_tree = UpdatedChild,
        l_height = height(UpdatedChild) + 1
    });
% search right
delete(Node, Key) when Key > Node#node.key ->
    UpdatedChild = delete(Node#node.r_tree, Key),
    rebalance(Node#node{
        r_tree = UpdatedChild,
        r_height = height(UpdatedChild) + 1
    });
% node to be deleted not exists
delete(none, _) ->
    none;
%
% we found node so we want to delete it:
% - case it has only 1 child
delete(Node, _) when Node#node.l_height =:= 1 ->
    Node#node.r_tree;
delete(Node, _) when Node#node.r_height =:= 1 ->
    Node#node.l_tree;
%
% - case it has child with one child
delete(Node, _) when Node#node.l_tree#node.r_height =:= 1 ->
    rebalance(Node#node.l_tree#node{
        r_tree = Node#node.r_tree,
        r_height = height(Node#node.r_tree) + 1
    });
delete(Node, _) when Node#node.r_tree#node.l_height =:= 1 ->
    rebalance(Node#node.r_tree#node{
        l_tree = Node#node.l_tree,
        l_height = height(Node#node.l_tree) + 1
    });
% common case for all other
delete(Node, _) ->
    Leaf = find_leaf(Node#node.l_tree#node.r_tree),
    (delete(Node, Leaf#node.key))#node{
        key = Leaf#node.key,
        value = Leaf#node.value
    }.

find_leaf(Node) when Node#node.l_tree =:= none, Node#node.r_tree =:= none ->
    Node;
find_leaf(Node) ->
    case Node#node.l_height < Node#node.r_height of
        true -> find_leaf(Node#node.l_tree);
        false -> find_leaf(Node#node.r_tree)
    end.

rebalance(Node) ->
    case factor(Node) of
        -2 -> make_RR_or_RL(Node);
        2 -> make_LL_or_LR(Node)
    end.

find(#node{l_tree = Child, key = NodeKey}, Key) when Key < NodeKey ->
    find(Child, Key);
find(#node{r_tree = Child, key = NodeKey}, Key) when Key > NodeKey ->
    find(Child, Key);
find(#node{value = Result}, _) ->
    Result;
find(none, _) ->
    none.

sorted(Root) when is_record(Root, node) ->
    sorted(Root, []).

sorted(#node{value = Value, key = Key, l_tree = LChild, r_tree = RChild}, List) ->
    Appended = [
        {Key, Value} | sorted(RChild, List)
    ],
    sorted(LChild, Appended);
sorted(_, List) ->
    List.

pop_leaf(none) ->
    none;
pop_leaf(Node) ->
    Leaf = find_leaf(Node),
    {delete(Node, Leaf), Leaf}.

fold(none, Acc, _) ->
    Acc;
fold(#node{l_tree = LTree, r_tree = RTree, value = Val, key = Key}, Acc, Func) ->
    AccLeft = fold(LTree, Acc, Func),
    AccRight = fold(RTree, AccLeft, Func),
    Func(AccRight, {Key, Val}).

# Objective  
Gain experience in constructing custom data types, polymorphism, recursive algorithms, and testing methods (unit testing, property-based testing).  

# Requirements  
The task is to implement a set based on a hash table.  

1. Functions:  
    - Adding and removing elements;  
    - Filtering;  
    - Mapping;  
    - Folding (left and right);  
    - The structure must be a [monoid](https://en.wikipedia.org/wiki/Monoid).  
2. Data structures must be immutable.  
3. The library must be tested using unit testing.  
4. The library must be tested using property-based testing (at least 3 properties, including monoid properties).  
5. The structure must be polymorphic.  
6. Use idiomatic programming style for the technology.  

# Implementation  
Under the hood, the functions in the `multiset` module rely on tree functions from the `avl` module.  

## Multiset  
Main functions.  

### find  
```erlang
find(#multiset{tree = none}, _) ->
    0;
find(Self, Item) ->
    Found = avl:find(Self#multiset.tree, Item),
    case is_integer(Found) of
        true -> Found;
        false -> 0
    end.
```  

### delete  
```erlang
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
```  

### add  
```erlang
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
```  

### fold  
```erlang
fold(#multiset{tree = none}, Acc, _) ->
    Acc;
fold(#multiset{tree = Tree}, Acc, Func) ->
    avl:fold(Tree, Acc, Func).
```  

### map  
```erlang
map(Self, Func) ->
    fold(
        Self,
        new(),
        fun(Acc, Value) -> add(Acc, Func(Value)) end
    ).
```  

### filter  
```erlang
filter(Self, Func) ->
    fold(
        Self,
        new(),
        fun(Acc, Value) -> filter_builtin(Value, Acc, Func) end
    ).
```  

## Avl  
Better to check the code directly because there’s quite a lot of it :)  

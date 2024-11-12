# Лабораторная работа 2
Ненов Владислав Александрович, P34082
Вариант: avl-bag
=====

# Цель
Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

# Требования
Необходимо реализовать множество на хэш-таблице.

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

# Реализация
Под копотом функций модуля multiset функции для работы с деревом из модуля avl.

## Multiset
Основные функции.

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

Лучше смотреть в коде, потому что там его довольно много)
-module(main).

-export([run/0]).

run() ->
    Root1 = avl:new(1, 1),
    Root2 = avl:upsert(Root1, 2, 2),
    Root3 = avl:upsert(Root2, -139, -139),
    Root4 = avl:upsert(Root3, 423, 423),
    Root5 = avl:upsert(Root4, -129, -129),
    Root6 = avl:upsert(Root5, 3, hdwedekd),
    Root7 = avl:upsert(Root6, -1000, hdwedekd),
    Root8 = avl:upsert(Root7, 2341, 2341),
    Root9 = avl:delete(Root8, 1),
    Root9.

-module(puzzle_maker).
-export([load/1,make_4_5/1]).

load(File) ->
    {ok, F} = file:read_file(File),
    load2(F, <<>>, []).
load2(<<>>, _, R) -> lists:reverse(R);
load2(<<"\n", B/binary>>, W, R) -> 
    load2(B, <<>>, [W|R]);
load2(<<C:8, B/binary>>, W, R) -> 
    load2(B, <<W/binary, C:8>>, R).

parse(_, _, []) -> [];
parse(Rows, Cols, [H|T]) ->
    HW = horizontal_words(H, Cols),
    VW = vertical_words(HW),
    X = {H, HW, VW},
    [X|parse(Rows, Cols, T)].

horizontal_words(<<>>, _) -> [];
horizontal_words(Y, Cols) -> 
    Z = Cols*8,
    <<X:Z, R/binary>> = Y,
    [<<X:(Cols*8)>>|
     horizontal_words(R, Cols)].

vertical_words([<<>>|_]) -> [];
vertical_words(WL) -> 
    FLS = lists:map(fun(<<C:8, _/binary>>) ->
                            <<C:8>>
                    end, WL),
    TLS = lists:map(fun(<<_:8, R/binary>>) ->
                            R
                    end, WL),
    [list_to_word(FLS, <<>>)|
     vertical_words(TLS)].
list_to_word([], R) -> R;
list_to_word([<<C:8>>|T], R) -> 
    list_to_word(T, <<R/binary, C:8>>).

to_defs([], _) -> [];
to_defs([{_Puzzle, HW, VW}|T], Defs) -> 
    [{hints(HW, Defs), hints(VW, Defs)}|
     to_defs(T, Defs)].

randint(X) ->
    round(math:ceil(X*rand:uniform())).
    

hints([], _) -> [];
hints([Word|T], Defs) -> 
    L0 = ets:match(Defs, {Word, '$1'}),
    L = lists:map(fun([X]) -> X end,
                  L0),
    LL = length(L),
    case LL of
        0 -> 
            io:fwrite("hints failure: "),
            io:fwrite(Word),
            io:fwrite("\n");
        _ -> ok
    end,
    X = randint(length(L)),
    [lists:nth(X, L)|
     hints(T, Defs)].
defs_ets(L) ->
    DB = ets:new(ok, [bag]),
    defs_ets2(L, DB).
defs_ets2([], DB) -> DB;
defs_ets2([H|T], DB) -> 
    DB2 = defs_ets3(H, DB),
    defs_ets2(T, DB2).
defs_ets3([], DB) -> DB;
defs_ets3([{Word, Def}|T], DB) -> 
    Entry = {Word, Def},
    ets:insert(DB, Entry),
    defs_ets3(T, DB).


make_4_5(D) ->
    Rows = 5,
    Cols = 4,
    Defs = defs_ets(D),%{Word, Def}
    L0 = load("4_5_puzzles.txt"),
    %would be nice if this was a persistent process, and Defs and L0 were maintained in between.
    X = randint(length(L0)),
    L = [lists:nth(X, L0)],
    [L2] = parse(Rows, Cols, L),
    [Puzzle] = to_defs([L2], Defs),
    [L2, Puzzle].
    




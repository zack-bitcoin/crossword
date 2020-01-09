-module(crossword).

-export([load_dict/0, 
         %ten_words/1,
         word_combos/1]).

load_dict() ->
    {ok, T} = file:read_file("20k.txt"),
    W = text_to_words(<<>>, [], T),%all words 3 letters or longer.
    W2 = lists:sort(W),%alphabetize

    {ok, D} = file:read_file("dictionary2.csv"),
    D3 = remove_quotes(<<>>, D),
    D4 = parse_dict([], D3),%[{word, part of speech, definition}, ...]
    D2 = lists:sort(fun({A, _, _}, {B, _, _}) ->
                           A < B
                   end, 
                    D4),
    D5 = define_words(W2, D2),
    words_by_length(D5, [], 3, [[]]).


prob_maker(D, N) ->
    P = dict:new(),
    P2 = probs(P, D, N),%{{<<"c">>, <<"a">>, '_'}, 8}
    P2.
    
word_combos(D) ->
    Board = {{'_','_','_','_'},
             {'_','_','_','_'},
             {'_','_','_','_'},
             {'_','_','_','_'}},
    Size = max(size(Board), size(element(1, Board)))-2,
    P = prob_maker(D, Size),
    _Defs = defs_ets(D),%{Word, Def}
    Words = words_ets(D), %{Word, Letter1, Letter2... }
    WL = lists:reverse(word_list_maker(Words, Size)),
    fill_board(Board, 1, size(Board), Words, P, WL).
stringify_board(B) when is_tuple(B) ->
    stringify_board(tuple_to_list(B));
stringify_board([]) -> <<>>;
stringify_board([A|B]) -> 
    <<(stringify_board(A))/binary,
      (stringify_board(B))/binary>>;
stringify_board('_') -> <<"_">>;
stringify_board(B) when is_binary(B) ->
    B.

word_list_maker(_, 0) -> [];
word_list_maker(Words, N) ->
    W1_DB = lists:nth(N, Words),
    Key = list_to_tuple(['$1'|empty_lookup(N+2)]),
    WL0 = ets:match(W1_DB, Key),
    WL1 = lists:map(fun([X]) -> X end, WL0),
    L = lists:sort(fun(X, Y) -> X < Y end, WL1),
    [L|word_list_maker(Words, N-1)].

fill_board(Board, N, Rows, _Words, _P, _) when (N == (Rows + 1))->
    io:fwrite(stringify_board(Board)),
    io:fwrite("\n"),
    [Board];
fill_board(Board, N, Rows, Words, P, WLM) ->
    W1 = size(element(N, Board)),
    WL = lists:nth(W1-2, WLM),
    fill_board2(WL, Board, N, Rows, Words, P, [], WLM).
fill_board2([], _, _, _, _, _, R, _) -> R;
fill_board2([Word|WT], Board, N, Rows, Words, P, R, WLM) ->
    Tup = word_to_tuple(Word),
    Board2 = setelement(N, Board, Tup),
    B = check_cols(Board2, P, size(Tup), 1),
    R1 = if
             B -> fill_board(Board2, N+1, Rows, Words, P, WLM);
             true -> []
         end, 
    fill_board2(WT, Board, N, Rows, Words, P, R++R1, WLM).
word_to_tuple(Word) -> wtt2(Word, []).
wtt2(<<>>, R) -> 
    list_to_tuple(lists:reverse(R));
wtt2(<<C:8, D/binary>>, R) -> 
    wtt2(D, [<<C:8>>|R]).
   
check_cols(_B, _P, Cols, N) when (N > Cols) -> 
    true;
check_cols(B, P, Cols, N) -> 
    L = tuple_to_list(B),
    T2 = lists:map(fun(X) -> element(N, X) end,
                   L),
    case dict:find(T2, P) of
        error -> false;
        _ -> check_cols(B, P, Cols, N+1)
    end.

probs(DB, [], _) -> DB;
probs(DB, _, 0) -> DB;
probs(DB, [[]|T], N) -> 
    io:fwrite("progress "),
    io:fwrite(integer_to_list(N)),
    io:fwrite("\n"),
    probs(DB, T, N-1);
probs(DB, [[{Word, _}|T1]|T2], N) -> 
    DB2 = insert_combos(Word, DB),
    probs(DB2, [T1|T2], N).

insert_combos(Word, DB) ->
    N = size(Word),
    insert_combos2(Word, N, {1}, DB).
insert_combos2(Word, N, T, DB0) ->
    Key = set_letters(tuple_to_list(T), 
                      Word,
                      1),
    DB = dict:store(Key, 1, DB0),
    F = finished(N, T),
    ST = size(T),
    if
        F and (ST == N)-> DB;
        true ->
            T2 = if
                     F -> tuple_ones(ST+1, 1);
                     true -> next_check(N, T)
                 end,
            insert_combos2(Word, N, T2, DB)
    end.
set_letters([], <<_:8, R/binary>>, N) ->
    ['_'|set_letters([], R, N+1)];
set_letters(_, <<>>, _) -> [];
set_letters([NH|NT], 
            <<W:8, WR/binary>>,
            N) -> 
    if
        (N == NH) -> 
            [<<W:8>>|set_letters(NT, WR, N+1)];
        true -> 
            ['_'|set_letters([NH|NT], WR, N+1)]
    end.

finished(N, T) ->
    %5, {3, 4, 5} %this is finished
    %5, {2, 4, 5} %this is not finished
    L = tuple_to_list(T),
    L2 = lists:reverse(L),
    finished2(N, L2).
finished2(_, []) -> true;
finished2(N, [N|T]) ->
    finished2(N-1, T);
finished2(_, _) -> false.
tuple_ones(N, X) ->
    list_to_tuple(tuple_ones2(N, X)).
tuple_ones2(0, _) -> [];
tuple_ones2(N, X) ->
    [X|tuple_ones2(N-1, X+1)].

next_check(N, T) ->
    %iterates the tuple through the possible set of characters that can be known
    %next_check(5, {1, 4, 5}) -> {2, 3, 4}
    %next_check(5, {1, 3, 5}) -> {1, 4, 5}
    L = tuple_to_list(T),
    L2 = lists:reverse(L),
    L3 = next_check2(N, L2, 0),
    L4 = lists:reverse(L3),
    list_to_tuple(L4).
next_check2(N, [N|T], P) ->
    next_check2(N-1, T, P+1);
next_check2(_, [M|T], P) ->
    next_check3(T, M+1, P).
next_check3(T, M, 0) -> [M|T];
next_check3(T, M, P) -> 
    next_check3([M|T], M+1, P-1).

empty_lookup(0) -> [];
empty_lookup(N) -> 
    ['_'|empty_lookup(N-1)].

words_ets([]) -> [];
words_ets([H|T]) -> 
    [words_ets2(H)|words_ets(T)].
words_ets2(H) ->
    DB = ets:new(ok, [set]),
    words_ets3(H, DB).
words_ets3([], DB) -> DB;
words_ets3([{Word, _}|T], DB) -> 
    Letters = word_to_letters(Word, []),
    Entry = list_to_tuple([Word] ++ Letters),
    ets:insert(DB, Entry),
    words_ets3(T, DB).

defs_ets([]) -> [];
defs_ets([H|T]) -> 
    [defs_ets2(H)|defs_ets(T)].
defs_ets2(H) ->
    DB = ets:new(ok, [bag]),
    defs_ets3(H, DB).
defs_ets3([], DB) -> DB;
defs_ets3([{Word, Def}|T], DB) -> 
    Entry = {Word, Def},
    ets:insert(DB, Entry),
    defs_ets3(T, DB).

word_to_letters(<<>>, L) -> lists:reverse(L);
word_to_letters(<<C:8, R/binary>>, L) -> 
    L2 = [<<C:8>>|L],
    word_to_letters(R, L2).

words_by_length([], [], _, L) -> lists:reverse(L);
words_by_length([], W, N, L) ->
    words_by_length(W, [], N+1, [[]|L]);
words_by_length([{Word, Def}|T], W2, N, [L|LT]) ->
    S = size(Word),
    if
        S == N -> 
            L2 = [[{Word, Def}|L]|LT],
            words_by_length(T, W2, N, L2);
        true ->
            W3 = [{Word, Def}|W2],
            words_by_length(T, W3, N, [L|LT])
    end.

remove_quotes(A, <<>>) -> A;
remove_quotes(A, <<"\"", R/binary>>) -> 
    remove_quotes(A, R);
remove_quotes(A, <<C:8, R/binary>>) -> 
    remove_quotes(<<A/binary, C:8>>, R).

define_words([], _) -> [];
define_words([<<>>|T], D) -> 
    define_words(T, D);
define_words(_, []) -> [];
define_words([W|T1], [{W, _, Def}|D1]) -> 
    [{W, Def}|define_words([W|T1], D1)];
define_words([W1|T1], [{W2, P2, Def2}|D1]) -> 
    if
        W1 > W2 -> define_words([W1|T1], D1);
        true -> define_words(T1, [{W2, P2, Def2}|D1])
    end.
            
uncapatilize(<<>>) -> 
    <<>>;
uncapatilize(<<F:8, R/binary>>) -> 
    if
        ((F<91) and (F>64)) ->
            <<(F+32):8, R/binary>>;
        true -> <<F:8, R/binary>>
    end.

parse_dict(L, <<>>) -> lists:reverse(L);
parse_dict(L, R) -> 
    {A, R2} = parse_dict_line_0(<<>>, R),
    parse_dict([A|L], R2).
parse_dict_line_0(Word, <<"\r\n", R/binary>>) ->
    parse_dict_line_0(Word, R);
parse_dict_line_0(Word, <<" (", R/binary>>) ->
    {Part, Def, R2} = parse_dict_line_1(<<>>, R),
    {{uncapatilize(Word), Part, Def}, R2};
parse_dict_line_0(Word, <<C:8, R/binary>>) ->
    parse_dict_line_0(<<Word/binary, C:8>>, R).

parse_dict_line_1(Part, <<") ", R/binary>>) ->
    {Def, R2} = parse_dict_line_2(<<>>, R),
    {Part, Def, R2};
parse_dict_line_1(Part, <<C:8, R/binary>>) ->
    parse_dict_line_1(<<Part/binary, C:8>>, R).

parse_dict_line_2(Def, <<"\r\n", R/binary>>) ->
    {Def, R};
parse_dict_line_2(Def, <<C:8, R/binary>>) ->
    parse_dict_line_2(<<Def/binary, C:8>>, R).

text_to_words(N, L, <<"\n", R/binary>>) ->
    S = size(N),
    L2 = if
             S>2 -> [N|L];
             true -> L
         end,
    text_to_words(<<>>, L2, R);
text_to_words(N, L, <<>>) -> [N|L];
text_to_words(N, L, <<A, R/binary>>) -> 
    text_to_words(<<N/binary, A>>, L, R).

    

-module(ten_words).
-export([doit/1]).
doit(S) ->
    <<A:40, B:40, C:40, D:40, E:40>> = S,
    F = [<<A:40>>, <<B:40>>, <<C:40>>, <<D:40>>, <<E:40>>],
    F2 = sideways(F),
    no_repeats(F ++ F2).
sideways([<<>>|_]) -> [];
sideways(WL) -> 
    FLS = lists:map(fun(<<C:8, _/binary>>) ->
                            <<C:8>>
                    end, WL),
    TLS = lists:map(fun(<<_:8, R/binary>>) ->
                            R
                    end, WL),
    [list_to_word(FLS, <<>>)|sideways(TLS)].
no_repeats([]) -> true;
no_repeats([H|T]) -> 
    not(is_in(H, T)) and no_repeats(T).
is_in(_, []) -> false;
is_in(H, [H|_]) -> true;
is_in(H, [_|T]) ->
    is_in(H, T).
list_to_word([], R) -> R;
list_to_word([<<C:8>>|T], R) -> 
    list_to_word(T, <<R/binary, C:8>>).

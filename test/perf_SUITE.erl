-module(perf_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([perf0/1, perf1/1, perf2/1]).

-define(TIMES, 999).

all() -> [perf0,perf1,perf2].

init_per_suite(Cfg) ->
    [] = [KV || KV={[A|B],N} <- get(),
                is_binary(A), is_binary(B), is_integer(N)
         ],
    Cfg.

end_per_suite(Cfg) ->
    [erase([A|B]) || {[A|B],N} <- get(),
                     is_binary(A), is_binary(B), is_integer(N)
    ],
    Cfg.

perf0([_|_]) ->
    case levenshtein:perftest(?TIMES, fun levenshtein:d0/2) of
        N when 110 < N, N < 230 -> ok
    end.

perf1([_|_]) ->
    case levenshtein:perftest(?TIMES, fun levenshtein:d1/2) of
        N when 110 < N, N < 230 -> ok
    end.

perf2([_|_]) ->
    case levenshtein:perftest(?TIMES, fun levenshtein:d2/2) of
        N when 60*1000 < N, N < 200*1000 -> ok
    end.

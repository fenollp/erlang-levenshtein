-module(levenshtein).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([perftest/2]).
-define(A, <<"7ab02d24-2d67-11e8-835d-0b1d27744c6d">>).
-define(B, <<"80393ca4-2d67-11e8-8f7f-c7a8488e4904">>).
-endif.

-export([d/2]).
-export([d0/2]).
-export([d1/2]).
-export([d2/2]).

-compile({inline, [d/2]}).
d(A, B) -> d2(A, B).

%% original

d0(<<Bin/binary>>, <<Bin2/binary>>) ->
    {Ed, _Cache} = d0(Bin, Bin2, dict:new()),
    Ed.

d0(<<>>, <<Bin/binary>>, Cache) ->
    {byte_size(Bin), dict:store({<<>>,Bin}, byte_size(Bin), Cache)};
d0(<<Bin/binary>>, <<>>, Cache) ->
    {byte_size(Bin), dict:store({Bin,<<>>}, byte_size(Bin), Cache)};
d0(<<B:8,B1/binary>>, <<B:8,B2/binary>>, Cache) ->
    d0(B1, B2, Cache);
d0(<<_:8,B1/binary>>=Bin1, <<_:8,B2/binary>>=Bin2, Cache) ->
    case dict:is_key({Bin1,Bin2}, Cache) of
        true -> {dict:fetch({Bin1,Bin2}, Cache), Cache};
        false ->
            {L1, C1} = d0(Bin1, B2, Cache),
            {L2, C2} = d0(B1, Bin2, C1),
            {L3, C3} = d0(B1, B2, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, dict:store({Bin1,Bin2}, L, C3)}
    end.

%% dict

d1(<<Bin/binary>>, <<Bin2/binary>>) ->
    {Ed, _Cache} = d1(Bin, Bin2, dict:new()),
    Ed.

-define(KEY(A,B), [A|B]).
d1(<<>>, <<Bin/binary>>, Cache) ->
    Size = byte_size(Bin),
    {Size, dict:store(?KEY(<<>>,Bin), Size, Cache)};
d1(<<Bin/binary>>, <<>>, Cache) ->
    Size = byte_size(Bin),
    {Size, dict:store(?KEY(Bin,<<>>), Size, Cache)};
d1(<<B:8,B1/binary>>, <<B:8,B2/binary>>, Cache) ->
    d1(B1, B2, Cache);
d1(<<_:8,B1/binary>>=Bin1, <<_:8,B2/binary>>=Bin2, Cache) ->
    Key = ?KEY(Bin1, Bin2),
    case dict:find(Key, Cache) of
        {ok,L} -> {L, Cache};
        error ->
            {L1, C1} = d1(Bin1, B2, Cache),
            {L2, C2} = d1(B1, Bin2, C1),
            {L3, C3} = d1(B1, B2, C2),
            L = 1 + erlang:min(L1, erlang:min(L2, L3)),
            {L, dict:store(Key, L, C3)}
    end.

%% pdict

d2(<<>>, <<Bin/binary>>) ->
    Size = byte_size(Bin),
    _ = erlang:put(?KEY(<<>>,Bin), Size),
    Size;
d2(<<Bin/binary>>, <<>>) ->
    Size = byte_size(Bin),
    _ = erlang:put(?KEY(Bin,<<>>), Size),
    Size;
d2(<<B:8,B1/binary>>, <<B:8,B2/binary>>) ->
    d2(B1, B2);
d2(<<_:8,B1/binary>>=Bin1, <<_:8,B2/binary>>=Bin2) ->
    Key = ?KEY(Bin1, Bin2),
    case erlang:get(Key) of
        undefined ->
            L1 = d2(Bin1, B2),
            L2 = d2(B1, Bin2),
            L3 = d2(B1, B2),
            L = 1 + erlang:min(L1, erlang:min(L2, L3)),
            _ = erlang:put(Key, L),
            L;
        L -> L
    end.

%% Tests

-ifdef(TEST).
-define(MATCH(Guard, Call), {timeout, 60, ?assertMatch(Guard, Call)}).

d_test() -> ?assertEqual(21, ?MODULE:d(?A, ?B)).

d0_test() ->
    ?MATCH(N when 250 < N andalso N < 300, perftest(1000, fun ?MODULE:d0/2)).

d1_test() ->
    ?MATCH(N when 300 < N andalso N < 400, perftest(1000, fun ?MODULE:d1/2)).

d2_test() ->
    ?MATCH(N when 300 < N andalso N < 400, perftest(1000, fun ?MODULE:d2/2)).

perftest(Iterations, Method) ->
    Start = os:system_time(),
    method_loop(Iterations, Method),
    Diff = os:system_time() - Start,
    R = (Iterations / Diff) * 1000000000,
    io:format(user, "\n~p: ~p ips\n", [Method,R]),
    R.

method_loop(0, _) -> ok;
method_loop(I, Method) ->
    _ = Method(?A, ?B),
    method_loop(I - 1, Method).
-endif.

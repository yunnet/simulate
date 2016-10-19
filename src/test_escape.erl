%%%-------------------------------------------------------------------
%%% @author <yunnet>
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2016 14:33
%%%-------------------------------------------------------------------
-module(test_escape).
-author("yunnet").

%% API
-export([run/1, unEscape808/1]).

run(T)->
  statistics(runtime),
  A = "7E0200001C01318000099900080000000000000003019410A106694C1300640028001E1610190953487D027E",
  B = to_bin(A),
  run(T, 10, B).

run(T, 0, _)->
  {_, Time} = statistics(runtime),
  io:format("(~p)running time: ~p ms ~n", [T, Time]),
  ok;

run(T, N, B)->
  if
    T == 1 -> C = binary:replace(B, <<16#7D,16#02>>, <<16#7E>>, [global]),
              D = binary:replace(C, <<16#7D,16#01>>, <<16#7D>>, [global]);
    true -> C = unEscape808(B)
  end,
  run(T, N - 1, B).

%% @spec to_bin(string()) -> binary()
%% @doc Convert a hexadecimal string to a binary.
to_bin(L) ->
  to_bin(L, []).

to_bin([], Acc) ->
  iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
  to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

%% @spec dehex(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 ->
  C - $0;
dehex(C) when C >= $a, C =< $f ->
  C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
  C - $A + 10.


%%808反转义 7D02 -> 7E 7D01 -> 7D
unEscape808(X) ->
  unEscape808(X, <<>>).
unEscape808(<<125, 1, T/binary>>, Acc) ->
  unEscape808(T, <<Acc/binary, 125>>);      %%7D01 -> 7D     125, 1 -> 125
unEscape808(<<125, 2, T/binary>>, Acc) ->
  unEscape808(T, <<Acc/binary, 126>>);      %%7D02 -> 7E     125, 2 -> 126
unEscape808(<<H, T/binary>>, Acc) ->        %%Other
  unEscape808(T, <<Acc/binary, H>>);
unEscape808(<<>>, Acc) ->
  Acc.



%%%-------------------------------------------------------------------
%%% @author Net
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2016 11:40
%%%-------------------------------------------------------------------
-module(test_crc).
-author("yunnet").

%% API
-compile(export_all).


a()->
CommNO = "013180000999",
Seq = 8,
A = 16#0,
B = 16#3,
Lat = 26480801,
Lon = 107564051,
Height = 100,
Speed = 40,
Head = 30,
%%Time = os:timestamp(),
Time =  lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [2016 rem 100, 10, 19, 09, 53, 48])),
DataBody = lists:flatten(io_lib:format("~8.16.0B~8.16.0B~8.16.0B~8.16.0B~4.16.0B~4.16.0B~4.16.0B~s", [A, B, Lat, Lon, Height, Speed, Head, Time]) ),
sendData(CommNO, 16#0200, Seq, DataBody).

sendData(CommNO, Cmd, Seq, DataBody) ->
  Bin = eca_code:encode(Cmd, CommNO, DataBody, Seq),
  error_logger:info_msg("send=> ~p~n", [eca_utils:to_hex_upper(Bin)]).
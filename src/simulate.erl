%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2016 by  <>
%%%-------------------------------------------------------------------
-module(simulate).

-behaviour(gen_server).

%% API
-export([start_link/4]).
-export([start/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 5000).

-record(state, {
       socket,
       seq = 0 :: non_neg_integer(),
       commno
}).

start(X, _Host, _Port, _CommNO)->
  lists:foreach(fun(Y) -> doStart(Y, _Host, _Port, _CommNO) end, lists:seq(1, X)).

doStart(Y, _Host, _Port, _CommNO)->
  RgName = list_to_atom("simulate_" ++ integer_to_list(Y)),
  CommNO = _CommNO + Y,
  RS = supervisor:start_child(simulate_sup, {RgName, {simulate, start_link, [RgName, _Host, _Port, CommNO]},
                                                   transient, 3000, worker, [simulate]}),

  case RS of
    {ok, Pid} ->
      {ok, Pid};
    {error, {already_started, Pid}} ->
      {ok, Pid};
    {error, already_present} ->
      ok = supervisor:delete_child(simulate_sup, RgName),
      start(Y, _Host, _Port, CommNO);
    {error, Error} ->
      error_logger:error_msg("~ts ~w ~ts Reason:~p", ["start the ", Y, "client error", Error]),
      {error, Error}
  end.

%%stop(X) ->
%%  lists:foreach(fun(Y) -> stop(Y) end, lists:seq(1, X)),
%%  supervisor:stop_child(simulate_sup).

start_link(RgName, _Host, _Port, _CommNO) ->
  gen_server:start_link({local, RgName}, ?MODULE, [RgName, _Host, _Port, _CommNO], []).

init([RgName, _Host, _Port, _CommNO]) ->
  case gen_tcp:connect(_Host, _Port, [binary, {packet, 0}]) of
    {ok, Socket} ->
      error_logger:info_msg("~p:~p connect is ok.", [_Host, _Port]),
      NState = #state{socket = Socket, commno = lists:concat(["0", _CommNO])},
      erlang:send_after(?INTERVAL, self(), {doTimer, 0});

    {error, Reason}->
      NState = #state{},
      error_logger:info_msg("~p connect failed. ~p~n", [RgName, Reason])
  end,
  {ok, NState}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({doTimer, N}, #state{socket = Socket, commno = CommNO, seq = Seq} = State) ->
  CmdSeq = (Seq + 1) band 16#FFFF,
  try
    send_0x0200(Socket, CommNO, CmdSeq)
  catch
      _:Reason -> error_logger:error_msg("0x0200 err: ~p~n", [Reason])
  end,

  erlang:send_after(?INTERVAL, self(), {doTimer, N + 1}),
  {noreply, State#state{seq = CmdSeq}};
handle_info({tcp, _Socket, Bin}, State)->
  error_logger:info_msg("recv: ~p~n", [Bin]),
  {noreply, State};
handle_info({tcp_close, _Socket}, State)->
  error_logger:error_msg("connect closed."),
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State)->
  error_logger:error_msg("tcp err:~p.", [Reason]),
  {stop, Reason, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_0x0200(Socket, CommNO, Seq)->
  A = 16#0,
  B = 16#3,
  Lat = 26480801,
  Lon = 107564051,
  Height = 100,
  Speed = 40,
  Head = 30,
  Time = os:timestamp(),

  DataBody = lists:flatten(io_lib:format("~8.16.0B~8.16.0B~8.16.0B~8.16.0B~4.16.0B~4.16.0B~4.16.0B~s", [A, B, Lat, Lon, Height, Speed, Head, eca_utils:time2GB(Time)]) ),
  sendData(Socket, CommNO, 16#0200, Seq, DataBody).

sendData(Socket, CommNO, Cmd, Seq, DataBody) ->
  Bin = eca_code:encode(Cmd, CommNO, DataBody, Seq),
%%  error_logger:info_msg("send=> ~p~n", [eca_utils:to_hex_upper(Bin)]),
  gen_tcp:send(Socket, Bin).


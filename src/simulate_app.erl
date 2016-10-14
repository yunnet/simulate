%%%-------------------------------------------------------------------
%% @doc simulate public API
%% @end
%%%-------------------------------------------------------------------

-module(simulate_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0, stop/0]).

%%====================================================================
%% API
%%====================================================================

start()->
    application:start(simulate).

stop()->
    application:stop(simulate).

start(_StartType, _StartArgs) ->
    Host = "127.0.0.1",
    Port = 6000,
    CommNO = 13180000000,
    {ok,SupPid} = simulate_sup:start_link(),
    simulate:start(10, Host, Port, CommNO),
    {ok, SupPid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

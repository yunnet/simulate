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
    [Host, Port] = get_server_config(simulate),
    [Commno, Num] = get_client_config(simulate),

    {ok,SupPid} = simulate_sup:start_link(),

    simulate:start(Num, Host, Port, Commno),
    {ok, SupPid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
get_server_config(App)->
    case application:get_env(App, server) of
        {ok, false} -> throw(undefined);
        {ok, Server_config} ->
            try
                {_, Host} = lists:keyfind(host, 1, Server_config),
                {_, Port} = lists:keyfind(port, 1, Server_config),
                [Host, Port]
            catch
                _:_ -> exit({bad_config, {simulate, {server, config_error}}})
            end;
        undefined -> throw(undefined)
    end.

get_client_config(App)->
    case application:get_env(App, client) of
        {ok, false} -> throw(undefined);
        {ok, Client_config} ->
            {_, Commno} = lists:keyfind(commno, 1, Client_config),
            {_, Num} = lists:keyfind(num, 1, Client_config),
            [Commno, Num];
        undefined -> throw(undefined)
    end.

%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the prp application.

-module(prp_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for prp.
start(_Type, _StartArgs) ->
    prp_sup:start_link().


%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for prp.
stop(_State) ->
    ok.

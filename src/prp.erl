%% @author author <willi@jups42.de>
%% @copyright 2012 Willi Müller.

%% @doc prp startup code

-module(prp).
-author('author <willi@jups42.de>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
	ensure_started(inets),
	ensure_started(crypto),
	ensure_started(mochiweb),
	ensure_started(mnesia),

	application:set_env(webmachine, webmachine_logger_module,
						webmachine_logger),
	ensure_started(webmachine),
	prp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the prp server.
start() ->
	ensure_started(inets),
	ensure_started(crypto),
	ensure_started(mochiweb),
	ensure_started(mnesia),

	application:set_env(webmachine, webmachine_logger_module,
						webmachine_logger),
	ensure_started(webmachine),
	application:start(prp).

%% @spec stop() -> ok
%% @doc Stop the prp server.
stop() ->
	Res = application:stop(prp),
	application:stop(webmachine),
	application:stop(mnesia),
	application:stop(mochiweb),
	application:stop(crypto),
	application:stop(inets),
	Res.

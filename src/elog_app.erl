-module(elog_app).

-behavior(application).

-export([start/2, stop/1]).

start(normal, _) ->
	ensure_ok(application:start(compiler)),
	ensure_ok(application:start(syntax_tools)),
	ensure_ok(application:start(lager)),
	elog_sup:start_link().

ensure_ok(ok) -> ok;
ensure_ok({error,{already_started,_}}) -> ok;
ensure_ok(Error) -> throw(Error).

stop(_) ->
	ok.


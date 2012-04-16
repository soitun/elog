-module(elog).

-include("elog.hrl").

-export([start/0]).

start() -> [application:start(App) || App <- [compiler, syntax_tools, lager]].


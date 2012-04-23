%%%----------------------------------------------------------------------
%%% File    : elog_sup.erl
%%% Author  : Ery Lee
%%% Purpose : 
%%% Created : 17 Apri. 2012
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2012, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(elog_sup).

-author("ery.lee@gmail.com").

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 100},
      [{elog, {elog, start_link, []},
        permanent, 5000, worker, [elog]}]}}.


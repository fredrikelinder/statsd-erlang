-module(statsd_sup).

-behaviour(supervisor).

-define(CHILD(Name, Module, Type, Args),
        {Name, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

-export([init/1]).

%% supervisor callbacks

init([PoolSize, Host]) ->
    PoolSpec = lists:map(
        fun (I) ->
                ?CHILD(I, statsd_worker, worker, [Host])
        end, lists:seq(1, PoolSize)),
    {ok, {{one_for_one, 10, 10}, PoolSpec}}.

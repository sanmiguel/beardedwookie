-module(bw_app).
-author('<michael.coles@gmail.com>').

-behaviour(application).

-export([start/2, stop/1]).

% Application callbacks

-spec start(StartType, StartArgs) ->
    {ok, pid()}
    | {ok, pid(), State}
    | {error, Reason :: term()}
      when
      StartType :: normal | {takeover, Node} | {failover, Node},
      StartArgs :: term(),
      State :: any().
start(_StartType, _StartArgs) ->
    SizeArgs = [{size, 10},
                {max_overflow, 20}],

    WorkerArgs = [{hostname, "localhost"},
                  {port, 8099},
                  {ping_every, 50000},
                  {options, [{auto_reconnect, true}]}],

    PoolName = application:get_env(beardedwookie, riak_poolname, riak_pool),

    riakc_poolboy:start_pool(PoolName, SizeArgs, WorkerArgs),

    {ok, Sup} = bw_sup:start_link(),
    {ok, Sup, [PoolName]}.

-spec stop(State :: any()) -> ok.
stop([PoolName]) ->
    riakc_poolboy:stop_pool(PoolName).

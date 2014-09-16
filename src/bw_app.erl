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

    PoolName = application:get_env(beardedwookie, riak_poolname, bw_riak_pool),

    SizeArgs = [{size, 5}, {max_overflow, 10}],
    PoolboyOpts = [{auto_reconnect, true}],
    WorkerArgs = [{hostname, "localhost"}, {port, 8087},
                  {options, PoolboyOpts}, {ping_every, 50000}],

    {ok, _Pid} = riakc_poolboy:start_pool(PoolName, SizeArgs, WorkerArgs),

    {ok, Sup} = bw_sup:start_link(),
    {ok, Sup, [PoolName]}.

-spec stop(State :: any()) -> ok.
stop([PoolName]) ->
    riakc_poolboy:stop_pool(PoolName).

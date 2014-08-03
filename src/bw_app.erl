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
    bw_sup:start_link().

-spec stop(State :: any()) -> ok.
stop(_State) ->
    ok.

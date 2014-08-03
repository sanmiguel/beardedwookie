-module(bw_sup).
-author('<michael.coles@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER(Mod,Args), {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
-define(SUPERVISOR(Mod,Args), {Mod, {Mod, start_link, Args}, permanent, 5000, supervisor, [Mod]}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%% Supervisor callbacks
-spec init(Args :: list()) -> {ok, {SupFlags :: tuple(), [ChildSpec :: list()]}} |
                   ignore |
                   {error, term()}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Dispatch = cowboy_router:compile(
      [{'_',
        [
         {"/tasks/[:task_id]", bw_task_handler, []}
        ]}]),

    RanchOptions = [{port, application:get_env(beardedwookie, port, 7799)}, {nodelay, true}],

    CowboyEnv = [{env, [{dispatch, Dispatch},
                        {max_keepalive, 100},
                        {compress, false},
                        {max_empty_lines, 5},
                        {max_header_name_length, 64},
                        {max_header_value_length, 4096},
                        {max_headers, 100},
                        {max_request_line_length, 4096},
                        {timeout, 30000}]}],

    Children =
    [
     ranch:child_spec(
       bw_handler, % Name
       50, % Acceptor count
       ranch_tcp, % transport method
       RanchOptions, % ranch options
       cowboy_protocol, % behaviour
       CowboyEnv % cowboy options
      )
    ],


    {ok, {SupFlags, Children}}.

%%% Internal functions



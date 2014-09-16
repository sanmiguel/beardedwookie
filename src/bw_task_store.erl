-module(bw_task_store).

-export([init/1]).

-export([insert/2]).
-export([create/1]).
-export([read/1]).
-export([update/2]).
-export([delete/1]).

-define(TIMEOUT, 5000).

-define(TASKPOOL, bw_riak_pool).
-define(TASKBUCKET, <<"task_bucket">>).

-type id() :: binary().
-type task() :: tuple().

-spec init(Args :: list(term())) ->
    {ok, pid()}
    | {error, Reason :: term()}.
init(_Args) ->
    PoolName = ?TASKPOOL,
    SizeArgs = [{size, 5}, {max_overflow, 10}],
    PoolboyOpts = [{auto_reconnect, true}],
    WorkerArgs = [{hostname, "localhost"}, {port, 8087},
                  {options, PoolboyOpts}, {ping_every, 50000}],
    {ok, _Pid} = riakc_poolboy:start_pool(PoolName, SizeArgs, WorkerArgs).

-spec create(task()) ->
    {ok, id()}
    | {error, Reason :: term()}.
create(Task) ->
    RawUUID = uuid:get_v4(),
    ID = uuid:uuid_to_string(RawUUID, binary_standard),
    case insert(ID, Task) of
        ok -> {ok, ID};
        {error,_}=E -> E
    end.

-spec insert(id(), task()) ->
    ok
    | {error, Reason :: term()}.
insert(ID, TaskProps) when is_binary(ID) ->
    Task = jsx:encode(TaskProps),
    Obj = riakc_obj:new(?TASKBUCKET, ID, Task),
    riakc_poolboy:put(?TASKPOOL, Obj, ?TIMEOUT).

-spec read(id()) ->
    {ok, id(), task()}
    | {error, Reason :: term()}.
read(ID) ->
    case riakc_poolboy:get(?TASKPOOL, ?TASKBUCKET, ID, ?TIMEOUT) of
        {ok, O} ->
            Docs = riakc_obj:get_values(O),
            %% TODO Something something sibling something
            [ JSON ] = [ jsx:decode(D) || D <- Docs ],
            {ok, JSON};
        {error,_}=Err ->
            Err
    end.

-spec update(id(), task()) ->
    {ok, id(), task()}
    | {error, Reason :: term()}.
update(ID, Task) ->
    {error, unimplemented}.

-spec delete(id()) ->
    ok
    | {error, Reason :: term()}.
delete(ID) ->
    riakc_poolboy:delete(?TASKPOOL, ?TASKBUCKET, ID).

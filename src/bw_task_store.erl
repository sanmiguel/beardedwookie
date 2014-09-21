-module(bw_task_store).

-export([insert/2]).
-export([list/0]).
-export([create/1]).
-export([read/1]).
-export([update/2]).
-export([delete/1]).

-define(TIMEOUT, 5000).

%% TODO This shouldn't be in a macro like this
-define(TASKPOOL, application:get_env(beadedwookie, riak_poolname, bw_riak_pool)).
-define(TASKBUCKET, <<"task_bucket">>).

-spec list() ->
    {ok, list(bw_task:id())}
    | {error, Reason :: term()}.
list() ->
    riakc_poolboy:list_keys(?TASKPOOL, ?TASKBUCKET, ?TIMEOUT).

-spec create(bw_task:props()) ->
    {ok, bw_task:id()}
    | {error, Reason :: term()}.
create(Task) ->
    ID = bw_task:id(),
    case insert(ID, Task) of
        ok -> {ok, ID};
        {error,_}=E -> E
    end.

-spec insert(bw_task:id(), bw_task:props()) ->
    ok
    | {error, Reason :: term()}.
insert(ID, TaskProps) when is_binary(ID) ->
    Task = jsx:encode(TaskProps),
    Obj = riakc_obj:new(?TASKBUCKET, ID, Task),
    riakc_poolboy:put(?TASKPOOL, Obj, ?TIMEOUT).

-spec read(bw_task:id()) ->
    {ok, bw_task:id(), bw_task:props()}
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

-spec update(bw_task:id(), bw_task:props()) ->
    {ok, bw_task:id(), bw_task:props()}
    | {error, Reason :: term()}.
update(ID, Task) ->
    {error, unimplemented}.

-spec delete(bw_task:id()) ->
    ok
    | {error, Reason :: term()}.
delete(ID) ->
    riakc_poolboy:delete(?TASKPOOL, ?TASKBUCKET, ID).

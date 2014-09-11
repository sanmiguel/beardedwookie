-module(bw_task_store).

-export([create/1]).
-export([read/1]).
-export([update/2]).
-export([delete/1]).

-define(TASKBUCKET, <<"task_bucket">>).

-type id() :: binary().
-type task() :: tuple().

-spec create(task()) ->
    {ok, id(), task()}
    | {error, Reason :: term()}.
create(Task) ->
    {error, unimplemented}.

-spec read(id()) ->
    {ok, id(), task()}
    | {error, Reason :: term()}.
read(ID) ->
    {error, unimplemented}.

-spec update(id(), task()) ->
    {ok, id(), task()}
    | {error, Reason :: term()}.
update(ID, Task) ->
    {error, unimplemented}.

-spec delete(id()) ->
    ok
    | {error, Reason :: term()}.
delete(ID) ->
    {error, unimplemented}.


-module(bw_task_handler).

-export([init/3]).
-export([rest_init/2]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
-export([allow_missing_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([display/2]).
-export([store/2]).

-record(context,
        {
         task :: bw_task:task()
        }).

-type context() :: #context{}.

%% Cowboy REST callbacks
-spec init(atom(), cowboy_req:req(), Paths :: list()) ->
    {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Args) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), list()) ->
    {ok, cowboy_req:req(), context()}.
rest_init(Req, _Opts) ->
    {ok, Req, #context{}}.

-spec known_methods(cowboy_req:req(), context()) ->
    {list(binary()), cowboy_req:req(), context()}.
known_methods(Req, Context) ->

    {[<<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PATCH">>, <<"POST">>], Req, Context}.

-spec allowed_methods(cowboy_req:req(), context()) ->
    {list(binary()), cowboy_req:req(), context()}.
allowed_methods(Req, Context) ->
    {[<<"DELETE">>, <<"GET">>, <<"PUT">>, <<"PATCH">>, <<"POST">>], Req, Context}.

-spec content_types_provided(cowboy_req:req(), context()) ->
    {list(), cowboy_req:req(), context()}.
content_types_provided(Req, Context) ->
    {Method, Req1} = cowboy_req:method(Req),
    CTA = case Method of
              <<"GET">>    -> [{{<<"application">>, <<"json">>, '*'}, display}];
              <<"DELETE">> -> [{{<<"application">>, <<"json">>, '*'}, undef}];
              <<"POST">>   -> [{{<<"application">>, <<"json">>, '*'}, undef}];
              <<"PATCH">>  -> [{{<<"application">>, <<"json">>, '*'}, undef}];
              <<"PUT">>    -> [{{<<"application">>, <<"json">>, '*'}, undef}];
			  _ -> []
          end,
    {CTA, Req1, Context}.

-spec content_types_accepted(cowboy_req:req(), context()) ->
    {list(), cowboy_req:req(), context()}.
content_types_accepted(Req, Context) ->
    {Method, Req1} = cowboy_req:method(Req),
    CTA = case Method of
              <<"PUT">>   -> [{{<<"application">>, <<"json">>, '*'}, store}];
              <<"PATCH">> -> [{{<<"application">>, <<"json">>, '*'}, store}];
              <<"POST">>  -> [{{<<"application">>, <<"json">>, '*'}, store}];
			  _ -> []
          end,
    {CTA, Req1, Context}.

-spec resource_exists(cowboy_req:req(), context()) ->
    {true | false, cowboy_req:req(), context()}.
resource_exists(Req0, #context{}=Context) ->
    {TaskID, Req1} = cowboy_req:binding(task_id, Req0),
    case bw_task_store:read(TaskID) of
        {ok, Task} -> {true, Req1, Context#context{task=Task}};
        _ -> {false, Req1, Context}
    end.

-spec allow_missing_post(cowboy_req:req(), context()) ->
    {boolean(), cowboy_req:req(), context()}.
allow_missing_post(Req, Context) -> {false, Req, Context}.

-spec delete_resource(cowboy_req:req(), context()) ->
    {true | false, cowboy_req:req(), context()}.
delete_resource(Req0, #context{}=Context0) ->
    {TaskID, Req1} = cowboy_req:binding(task_id, Req0),
    bw_task_store:delete(TaskID),
    {true, Req1, Context0}.

-spec display(cowboy_req:req(), context()) ->
    {Body :: binary(), cowboy_req:req(), context()}.
display(Req0, #context{}=Context) ->

    {TaskID, Req1} = cowboy_req:binding(task_id, Req0),
    Task = bw_task_store:read(TaskID),

    JSON = jsx:encode(Task),
    {JSON, Req1, Context}.

-spec store(cowboy_req:req(), context()) ->
    {Body :: binary(), cowboy_req:req(), context()}.
store(Req0, #context{}=Context) ->
    {TaskID, Req1} = cowboy_req:binding(task_id, Req0),

    {ok, Body, Req2} = cowboy_req:body(Req1),
    TaskProps = jsx:decode(Body),

    bw_task_store:insert(TaskID, TaskProps),
    {true, Req2, Context}.

-module(bw_task).

-include("bw_task.hrl").

-export([id/0]).
-export([props/1]).
-export([task/1]).

-define(version, <<"version">>).
-define(title, <<"title">>).
-define(status, <<"status">>).
-define(notes, <<"notes">>).
-define(misc, <<"misc">>).

-spec id() -> id().
id() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

-spec props(#task{}) -> props().
props(#task{
         version=V,
         title=T,
         status=S,
         notes=Ns,
         misc=Ms
        }) ->
    [
     {?version, V},
     {?title, T},
     {?status, S},
     {?notes, Ns},
     {?misc, Ms}
    ].

-spec task(props()) -> task().
task(Props) ->
    lists:foldl(fun val/2, #task{}, Props).

val({?version, V}, #task{}=T) -> T#task{version=V};
val({?title, T}, #task{}=T)   -> T#task{title=T};
val({?status, S}, #task{}=T)  -> T#task{status=S};
val({?notes, Ns}, #task{}=T)  -> T#task{notes=Ns};
val({?misc, Ms}, #task{}=T)   -> T#task{misc=Ms};
val(_, T) -> T.

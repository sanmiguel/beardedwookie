-define(TASKVERSION, 0).

-record(task,
        {
         version = ?TASKVERSION :: integer(),
         title :: binary(),
         status :: binary(), %% TODO Enum
         notes :: list(binary()),
         misc :: list(binary())
        }).

%% TODO Upgrade to uuid vWhatever to get uuid() type
-type id() :: binary().
-type task()  :: #task{}.
-type props() :: proplists:proplist().

-export_type([id/0, task/0, props/0]).

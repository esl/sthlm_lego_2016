-module(erlv3_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    case erlv3_sup:start_link(StartArgs) of
        {ok, SupervisorPid} ->
            {ok, SupervisorPid};
        SomeError ->
            {error, SomeError}
    end.

stop(_State) ->
    ok.

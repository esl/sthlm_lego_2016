-module(erlv3_sup).
-behaviour(supervisor).
-export([start_link/2, init/1]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, init, StartArgs).

init(Args) ->
    RestartStrategy = {one_for_one, 2, 5},
    ChildSpec = [],
    {ok, {RestartStrategy, ChildSpec}}.

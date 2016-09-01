-module(erlv3_bump_sensor).
-behaviour(gen_server).
-export([start_link/1, get_bump_state/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).

-define(BUMP_SENSOR_NAME, ?MODULE).

%% APIs
start_link(Args) ->
    gen_server:start_link({local, ?BUMP_SENSOR_NAME}, ?MODULE,
                          Args, []).

get_bump_state() ->
    gen_server:call(?BUMP_SENSOR_NAME, get_bump_state).

stop() ->
    gen_server:terminatestop(?BUMP_SENSOR_NAME).

%% Callbacks
init(_Args) ->
    State = [],
    {ok, State}.

handle_call(get_bump_state, _From, State) ->
    BumpState = read_bump_state(),
    {reply, BumpState, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions
read_bump_state() ->
    BumpFileLocation = erlv3_utils:get_bump_dir() ++ "/value0",
    {ok, StringContent} = file:read_file(BumpFileLocation),
    Content = re:replace(StringContent, "\n", "",
                         [{return, list}]),
    list_to_integer(Content).

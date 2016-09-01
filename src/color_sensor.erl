-module(color_sensor).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).
-export([start_link/1, get_color_mode/0, set_color_mode/1, get_color_values/0,
         stop/0]).

-record(state, {color_mode}).
-define(MODE_FILE_NAME, "/mode").
-define(DEFAULT_COLOR_MODE, col_reflect).
-define(COLOR_SENSOR_NAME, ?MODULE).


%%% API functions
start_link(Args) ->
    gen_server:start_link({local, ?COLOR_SENSOR_NAME}, ?MODULE, Args, []).

get_color_mode() ->
    gen_server:call(?COLOR_SENSOR_NAME, get_color_mode).

-spec set_color_mode(rgb_raw | col_reflect) -> ok | {error, _}.
set_color_mode(NewColorMode) ->
    gen_server:call(?COLOR_SENSOR_NAME, {set_color_mode, NewColorMode}).

get_color_values() ->
    gen_server:call(?COLOR_SENSOR_NAME, get_color_values).

stop() ->
    gen_server:stop(?COLOR_SENSOR_NAME).

%%% Calback functions
init(_Args) ->
    set_color_mode(?DEFAULT_COLOR_MODE),
    State = #state{color_mode = ?DEFAULT_COLOR_MODE},
    {ok, State}.

handle_call(get_color_mode, _From, State) ->
    {reply, State#state.color_mode, State};
handle_call({set_color_mode, NewColorMode}, _From, State) ->
    ReturnValue = set_lego_color_mode(NewColorMode),
    NewState = State#state{color_mode = NewColorMode},
    {reply, ReturnValue, NewState};
handle_call(get_color_values, _From, #state{color_mode = ColorMode} = _State) ->
    ColorSensorDirLocation = erlv3_utils:get_color_sensor(),
    ColorValues = case ColorMode of
                      rgb_raw ->
                          RedValue = get_color_code_value(
                                       ColorSensorDirLocation ++ "/value0"),
                          GreenValue = get_color_code_value(
                                         ColorSensorDirLocation ++ "/value1"),
                          BlueValue = get_color_code_value(
                                        ColorSensorDirLocation ++ "/value2"),
                          {RedValue, BlueValue, GreenValue};
                      col_reflect ->
                          ColorValue = get_color_code_value(
                                         ColorSensorDirLocation ++ "/value0"),
                          ColorValue
                  end,
    ColorValues.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% Internal functions
set_lego_color_mode(ColorMode) ->
    ColorSensorDir = erlv3_utils:get_color_sensor(),
    PathToColorModeFile = ColorSensorDir ++ ?MODE_FILE_NAME,
    case ColorMode of
        rgb_raw ->
            file:write_file(PathToColorModeFile, "RGB-RAW", [write]);
        col_reflect ->
            file:write_file(PathToColorModeFile, "COL-REFLECT", [write]);
        _InvalidColorMode ->
            {error, invalid_color_mode}
    end.

get_color_code_value(FileLocation) ->
    {ok, StringContent} = file:read_file(FileLocation),
    Content = string:sub_string(binary_to_list(StringContent), 1,
                      string:len(binary_to_list(StringContent))-1),
    list_to_integer(Content).

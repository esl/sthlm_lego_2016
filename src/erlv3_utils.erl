-module(erlv3_utils).

-define(CLASS_PATH, "/sys/class").

-export([find_tacho_motor/1,
         find_color_sensor/0]).

find_tacho_motor(Side) ->
  {ok, Port} = application:get_env(erlv3, {motor, Side}),
  find_dir(Port, find_dirs("tacho_motor")).

find_color_sensor() ->
  {ok, Port} = application:get_env(erlv3, color_sensor),
  find_dir(Port, find_dirs("lego-sensor")).

find_dirs(TopDir) ->
  filelib:wildcard(filename:join([?CLASS_PATH, TopDir, "*"])).

find_dir(_Port, []) ->
  {error, not_found};
find_dir(Port, [Dir | T]) ->
  case file:read_file(filename:join([Dir, "address"])) of
    {ok, Data} ->
      case binary_to_atom(Data, utf8) of
        Port ->
          Dir;
        _ ->
          find_dir(Port, T)
      end;
    {error, _} ->
      find_dir(Port, T)
  end.

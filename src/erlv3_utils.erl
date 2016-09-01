-module(erlv3_utils).

-define(CLASS_PATH, "/sys/class").

-export([find_tacho_motor/1,
         find_color_sensor/0]).

find_tacho_motor(Side) ->
  {ok, Envs} = application:get_env(erlv3, motor),
  Port = proplists:get_value(Side, Envs),
  find_dir(Port, "address", find_dirs("tacho-motor")).

find_color_sensor() ->
  find_dir('lego-ev3-color', "driver_name", find_dirs("lego-sensor")).

find_bump_sensor() ->
  find_dir('lego-ev3-touch', "driver_name", find_dirs("lego-sensor")).

find_dirs(TopDir) ->
  filelib:wildcard(filename:join([?CLASS_PATH, TopDir, "*"])).

find_dir(_Value, _File, []) ->
  {error, not_found};
find_dir(Value, File, [Dir | T]) ->
  case file:read_file(filename:join([Dir, File])) of
    {ok, Data} ->
      ValueBin = atom_to_binary(Value, utf8),
      case binary:match(Data, ValueBin) of
        {0, _} -> Dir;
        _ -> find_dir(Value, File, T)
      end;
    {error, _} ->
      find_dir(Value, File, T)
  end.

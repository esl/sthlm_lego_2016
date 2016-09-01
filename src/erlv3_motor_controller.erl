-module(erlv3_motor_controller).

-export([start_link/1,
	 duty_cycle_sp/1,set_duty_cycle_sp/2,
	 run/1,coast/1,brake/1,hold/1]).

-export([init/1,terminate/2,handle_call/3,
	 handle_cast/2,handle_info/2,code_change/3]).

-behaviour(gen_server).

-record(motor, {side,
		id_path="",
		type=tacho,
		command_path="",
		stop_action_path="",
		duty_cycle_path=""
	       }).

start_link(Side) ->
    gen_server:start_link(?MODULE, Side, []).

%% User API.

duty_cycle_sp(Motor) -> gen_server:call(Motor, duty_cycle_sp).

set_duty_cycle_sp(Motor, Pct) ->
    gen_server:call(Motor, {set_duty_cycle_sp,Pct}).

run(Motor) -> gen_server:call(Motor, run). 

coast(Motor) -> gen_server:call(Motor, coast).

brake(Motor) -> gen_server:call(Motor, brake).

hold(Motor) -> gen_server:call(Motor, hold).

%% Callbacks.

init(Side) ->
    {ok,IdPath} = erlv3_utils:find_tacho_motor(Side),
    St0 = #motor{side=Side, id_path=IdPath, type=tacho},
    St1 = set_paths(St0),
    {ok,St1}.

terminate(_R, _St) ->
    ok.

handle_call(duty_cycle_sp, _From, St) ->
    {reply,file:read_file(St#motor.duty_cycle_path),St};
handle_call({set_duty_cycle_sp,Pct}, _From, St) ->
    {reply,file:write_file(St#motor.duty_cycle_path, integer_to_list(Pct)),St};
handle_call(run, _From, St) ->
    {reply,file:write_file(St#motor.command_path, "run-forever"),St};
handle_call(coast, _From, St) ->
    file:write_file(St#motor.stop_action_path, "coast"),
    file:write_file(St#motor.command_path, "stop"),
    {reply,ok,St};
handle_call(brake, _From, St) ->
    file:write_file(St#motor.stop_action_path, "brake"),
    file:write_file(St#motor.command_path, "stop"),
    {reply,ok,St};
handle_call(hold, _From, St) ->
    file:write_file(St#motor.stop_action_path, "hold"),
    file:write_file(St#motor.command_path, "stop"),
    {reply,ok,St}.

%% Set the paths.
set_paths(#motor{id_path=IdPath}=St) ->
    ComPath = IdPath ++ "/" ++ "command",
    SAPath = IdPath ++ "/" ++ "stop_action",
    DCPath = IdPath ++ "/" ++ "duty_cycle_sp",
    St#motor{command_path=ComPath,
	     stop_action_path=SAPath,
	     duty_cycle_path=DCPath}.

%% Unused callbacks.

handle_cast(_M, St) -> {ok,St}.

handle_info(_I, St) -> {ok,St}.

code_change(_Old, St, _E) -> {ok,St}.

%% #!/bin/bash
%% # Motor script for Lego EV3 / ev3dev firmware
%% # Usage: motor 0 -- will control motor0
%% DIR="/sys/class/tacho-motor/motor$1"
%% echo "100" > ${DIR}/duty_cycle_sp
%% #echo "5" > ${DIR}/speed_sp
%% echo "0" > ${DIR}/position
%% echo "90" > ${DIR}/position_sp
%% echo "brake" > ${DIR}/stop_command
%% echo "run-to-rel-pos" > ${DIR}/command
%% #echo "run-forever" > ${DIR}/command
%% #sleep 1
%% #echo "stop" > ${DIR}/command

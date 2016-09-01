-module(erlv3_drive).

-export([start_link/0,forward/0,backward/0,coast/0,brake/0,hold/0,
	run/0,set_speeds/2]).

-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).

-behaviour(gen_server).

-record(motor_controller, {left,right,lspeed=0,rspeed=0}).

-define(DEFAULT_SPEED, 50).

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

forward() -> forward(?DEFAULT_SPEED).

forward(Speed) -> gen_server:call(?MODULE, {forward,Speed}).

backward() -> backward(?DEFAULT_SPEED).

backward(Speed) -> gen_server:call(?MODULE, {backward,Speed}).

coast() -> gen_server:call(?MODULE, coast).

brake() -> gen_server:call(?MODULE, brake).

hold() -> gen_server:call(?MODULE, hold).

run() -> gen_server:call(?MODULE, run).

set_speeds(Lspeed, Rspeed) ->
    gen_server:call(?MODULE, {set_speeds,Lspeed,Rspeed}).

%% Callbacks.

init(_) ->
    {ok,Left} = erlv3_motor_controller:start_link(left),
    {ok,Right} = erlv3_motor_controller:start_link(right),
    {ok,#motor_controller{left=Left,right=Right}}.

terminate(_, St) ->
    erlv3_motor_controller:stop(St#motor_controller.left),
    erlv3_motor_controller:stop(St#motor_controller.right).

handle_call({forward,S}, _From, #motor_controller{left=L,right=R}=St) ->
    set_duty_cycle_sp(L, S, R, S),
    run(L, R),
    {reply,ok,St};
handle_call({backward,S}, _From, #motor_controller{left=L,right=R}=St) ->
    set_duty_cycle_sp(L, -S, R, -S),
    run(L, R),
    {reply,ok,St};
handle_call({set_speeds,Ls,Rs}, _From, #motor_controller{left=L,right=R}=St) ->
    set_duty_cycle_sp(L, Ls, R, Rs),
    {reply,ok,St};
handle_call(run, _From, #motor_controller{left=L,right=R}=St) ->
    run(L, R),
    {reply,ok,St}.

set_duty_cycle_sp(L, Lv, R, Rv) ->
    erlv3_motor_controller:set_duty_cycle_sp(L, Lv),
    erlv3_motor_controller:set_duty_cycle_sp(R, Rv).

run(L, R) ->
    erlv3_motor_controller:run(L),
    erlv3_motor_controller:run(R).

%% Unused callbacks.

handle_cast(_M, St) -> {ok,St}.

handle_info(_I, St) -> {ok,St}.

code_change(_Old, St, _E) -> {ok,St}.

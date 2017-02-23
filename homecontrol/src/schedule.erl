-module(schedule).
-behavior(gen_server).

-export([start_link/0, init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2, code_change/3, timer/5, timer_repeated/5]).


%%%%%%%
% Public interface

start_link() -> gen_server:start_link(?MODULE, [], []).

timer(Schedule, Callback, Context, Hour, Minute) ->
    gen_server:call(Schedule, {timer, Callback, Context, Hour, Minute}).

timer_repeated(Schedule, Callback, Context, Hour, Minute) ->
    gen_server:call(Schedule, {timer_repeated, Callback, Context, Hour, Minute}).

%%%%%%%
% Callback interface

init([]) -> {ok, []}.

% TODO: Remove old timers
handle_cast(Request, State) ->
    case Request of
        {times_up_repeat, Callback, Context, Hour, Minute} ->
            % TODO:
            io:fwrite("Repeated timer triggered~n"),
            Callback ! Context,
            timer_repeated(self(), Callback, Context, Hour, Minute),
            {noreply, State};
        {times_up, Callback, Context} ->
            % TODO:
            io:fwrite("Timer triggered~n"),
            Callback ! Context,
            {noreply, State}
    end.

handle_call(Request, _From, State) ->
    case Request of
        {timer_repeat, Callback, Context, Hour, Minute} ->
            io:fwrite("Repeating timer for ~p:~p will trigger in ~p~n", [Hour, Minute, get_seconds_to(Hour,Minute)*1000]),
            Ref = make_ref(),
            % TODO: Modify to support gen_server
            NewTimer = timer:send_after(
                         get_seconds_to(Hour,Minute)*1000,
                         {times_up_repeat, Callback, Context, Hour, Minute}
                        ),
            Timer = {Ref, NewTimer},
            {reply, {ok, timer_repeat, Timer}, [Timer | State]};
        {timer, Callback, Context, Hour, Minute} ->
            io:fwrite("Timer for ~p:~p will trigger in ~p~n", [Hour, Minute, get_seconds_to(Hour,Minute)*1000]),
            Ref = make_ref(),
            % TODO: Modify to support gen_server
            NewTimer = timer:send_after(
                         get_seconds_to(Hour,Minute)*1000,
                         {times_up, Callback, Context}
                        ),
            Timer = {Ref, NewTimer},
            {reply, {ok, timer, Timer}, [Timer | State]}
    end.

terminate(_Reason, _State) -> none. % TODO: Clean out schedule

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, _State, _Extra) -> {error, "Not implemented"}.

%%%%%%%
% Private functions

get_seconds_to(THour, TMinute) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    LastDay = calendar:last_day_of_the_month(Year, Month),
    NewDay =
    case (Hour >= THour) orelse ((Hour =:= THour) andalso (Minute >= TMinute)) of
        true ->
            io:fwrite("Time has already passed today~n", []),
            Day + 1;
        false -> Day
    end,
    WakeTime =
    case LastDay < NewDay of
        true ->
            io:fwrite("Tomorrow is a new month~n", []),
            {{Year, Month+1, 1}, {THour, TMinute,0}};
        false -> {{Year, Month, NewDay}, {THour, TMinute,0}}
    end,
    TimeToWait =
        calendar:datetime_to_gregorian_seconds(WakeTime) -
        calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
    io:fwrite("Hours to wait ~p~n", [TimeToWait/3600]),
    TimeToWait.


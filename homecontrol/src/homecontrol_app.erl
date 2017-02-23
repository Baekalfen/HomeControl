%%%-------------------------------------------------------------------
%% @doc homecontrol public API
%% @end
%%%-------------------------------------------------------------------

-module(homecontrol_app).
% -behavior(supervisor).

% -behaviour(application).

%% Application callbacks
-export([start/0, init/0]).

-define(HOMECONTROLVM, '[redacted]').
-define(PI, '[redacted]').
-define(MACBOOK, '[redacted]').

start() -> spawn(?MODULE, init, []).

init() ->
    io:fwrite("Starting Home Control~n"),
    {ok, UniversalRemote} = universal_remote:start_link(),
    {ok, Lights} = light:start_link(),
    {ok, Music} = music:start_link(?PI,UniversalRemote),
    loop({
      Music,
      UniversalRemote,
      Lights,
      schedule:start_link()
     }),
    io:fwrite("Home Control Exitting!~n").

loop(State = {Music, UniversalRemote, Lights, Schedule}) ->
    io:fwrite("Starting loop~n"),
    receive
        % Action = {wake_up, light, radio, p3}
        {set_alarm, Action, Hour, Minute} ->
            % TODO: Clear previous alarm
            schedule:timer_repeated(Schedule, self(), Action, Hour, Minute);
        {wake_up, light, radio, RadioChannel} ->
            music:turn_on(Music,RadioChannel),
            light:sequence(Lights, wake_up);
        {wake_up, light, tv_delayed} ->
            light:sequence(Lights, wake_up),
            timer:send_after(
                600000, % 10 minutes in milliseconds
                {tv, on}
            );
        {music, on, radio, Channel} ->
            start_stereo(UniversalRemote, 43, aux2),
            music:turn_on(Music, radio, Channel);
        {music, on, youtube, Link} ->
            start_stereo(UniversalRemote, 43, aux2),
            music:turn_on(Music, youtube, Link);
        {music, off} ->
            music:turn_off(Music);
        {light, scene, Name} ->
            light:scene(Lights, Name);
        {light, sequence, Name} ->
            light:sequence(Lights, Name);
        {tv, on} ->
            start_stereo(UniversalRemote, 40, opt2),
            universal_remote:send_command(UniversalRemote, {hdmi, 2}),
            universal_remote:send_command(UniversalRemote, {tv_tuner, toggle_power}),
            universal_remote:send_command(UniversalRemote, {projector, on}),
            universal_remote:send_command(UniversalRemote, {screen, on});
        {tv, off} ->
            universal_remote:send_command(UniversalRemote, {stereo, off}),
            universal_remote:send_command(UniversalRemote, {tv_tuner, toggle_power}),
            universal_remote:send_command(UniversalRemote, {projector, off}),
            universal_remote:send_command(UniversalRemote, {screen, off});
        {remote, Device, Code} ->
            universal_remote:send_command(UniversalRemote, {Device, Code});
        {remote, Device, Code, Value} ->
            universal_remote:send_command(UniversalRemote, {Device, Code, Value});
        Otherwise ->
            io:fwrite("Got message in main: ~p~n", [Otherwise])
    end,
    loop(State).


%%%%%%%
% Private functions

start_stereo(UniversalRemote, Volume, Channel) ->
    universal_remote:send_command(UniversalRemote, {stereo, on}),
    universal_remote:send_command(UniversalRemote, {stereo, channel, Channel}),
    universal_remote:send_command(UniversalRemote, {stereo, volume, Volume}).

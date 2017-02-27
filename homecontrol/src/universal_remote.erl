-module(universal_remote).
-behavior(gen_server).

-export([send_command/2, start_link/0, init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2, code_change/3]).

-include("nodes.hrl").

%%%%%%%
% Public interface
start_link() -> gen_server:start_link(?MODULE, [], []).

send_command(UniversalRemote, Request) -> gen_server:cast(UniversalRemote, Request).

%%%%%%%
% Callback interface

init([]) ->
    % TODO: Detect which serial device is which
    try {rs232:start(?HOMECONTROLVM, "/dev/ttyUSB1", 9600),
    rs232:start(?HOMECONTROLVM, "/dev/ttyUSB0", 9600),
    rs232:start(?PI, "/dev/ttyUSB0", 115200)} of
        Devices -> {ok,Devices}
    catch
        Error -> {error, Error}
    end.

% TODO: Remove old timers
handle_cast(Request, State = {Arduino, Projector, Radio}) ->
    Self = self(),
    case Request of
        {hdmi, Code} ->
            Cmd = case Code of
                1 -> "$a1!";
                2 -> "$a2!";
                3 -> "$a3!";
                4 -> "$a4!";
                '2ch' -> "$a5!";
                power -> "$a6!"
            end,
            Arduino ! {send, Cmd, Self};
        {tv_tuner, Code} ->
            Cmd = case Code of
                toggle_power -> "$b1!";
                1 -> "$b2!";
                2 -> "$b3!";
                3 -> "$b4!";
                4 -> "$b5!";
                5 -> "$b6!";
                6 -> "$b7!";
                7 -> "$b8!";
                8 -> "$b9!";
                9 -> "$b:!";
                0 -> "$b;!";
                rec -> "$b>!"
            end,
            Arduino ! {send, Cmd, Self};
        {apple_tv, Code} ->
            Cmd = case Code of
                ok -> "$c1!";
                play -> "$c2!";
                stop -> "$c3!"
            end,
            Arduino ! {send, Cmd, Self};
        {projector, Code} ->
            Cmd = case Code of
                on -> "~0000 1\r";
                off -> "~0000 0\r"
            end,
            Projector ! {send, Cmd, Self};
        {screen, Code} ->
            Cmd = case Code of
                off -> "$e0!";
                on -> "$e1!"
            end,
            Arduino ! {send, Cmd, Self};
        {stereo, display, From} ->
            Radio ! {send, "get_display!", From};
        {stereo, Code, Value} ->
            Cmd = case Code of
                volume -> "volume_" ++ integer_to_list(Value) ++ "!";
                channel -> atom_to_list(Value) ++ "!"
            end,
            Radio ! {send, Cmd, Self};
        {stereo, Code} ->
            Cmd = case Code of
                on -> "power_on!";
                off -> "power_off!"
            end,
            Radio ! {send, Cmd, Self}
    end,
    {noreply, State}.

terminate(_Reason, _State) -> none. % TODO: Clean out schedule

handle_call(_Request, _From, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, _State, _Extra) -> {error, "Not implemented"}.


-module(music).
-behavior(gen_server).

-export([start_link/1, start_link/2, init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2, code_change/3, turn_on/3, turn_off/1]).


%%%%%%%
% Public interface


start_link(UniversalRemote) -> gen_server:start_link(?MODULE, [UniversalRemote], []).
start_link(Node, UniversalRemote) -> gen_server:start_link({global,Node}, ?MODULE, [UniversalRemote], []).

init(UniversalRemote) -> {ok, {UniversalRemote, false, fun() -> ok end}}.

turn_off(Music) ->
    gen_server:cast(Music, {turn_off}).

turn_on(Music, radio, p3) ->
    gen_server:cast(Music, {start_radio, p3});
turn_on(Music, youtube, Link) ->
    gen_server:cast(Music, {start_radio, youtube, Link}).

%%%%%%%
% Callback functions

handle_cast(Request, {UniversalRemote, _AutoTurnOff, StopMusic}) ->
    case Request of
        {start_radio, p3} ->
            io:fwrite("Starting P3~n"),
            StopMusic(),
            io:fwrite("Init Music~n"),
            os:cmd("mpc stop"),
            os:cmd("mpc play"),
            os:cmd("mpc volume 100"),
            {noreply, {UniversalRemote, true, fun() -> os:cmd("mpc stop") end}};
        {start_radio, youtube, Link} ->
            io:fwrite("Starting YouTube~n"),
            StopMusic(),
            Pid = cmd_async("~/playYouTube.sh " ++ Link),
            {noreply, {UniversalRemote, true, fun() -> exit(Pid, kill) end}};
        {stop_radio} ->
            io:fwrite("Stoping radio~n"),
            StopMusic(),
            {noreply, {UniversalRemote, false, fun() -> ok end}};
        {turn_off} ->
            io:fwrite("Turning off radio~n"),
            StopMusic(),
            % UniversalRemote ! {stereo, off},
            {noreply, {UniversalRemote, false, fun() -> ok end}}

    % TODO: Should be inserted as a schedule instead
    % after
    %     ?CHECK_DELAY ->
    %         case AutoTurnOff of
    %             true ->
    %                 io:fwrite("Checking if radio is still on~n"),
    %                 case radio_status(UniversalRemote) of
    %                     on -> loop(State);
    %                     off ->
    %                         io:fwrite("Turning off radio~n"),
    %                         turn_off(self()),
    %                         loop({UniversalRemote, false, StopMusic})
    %                 end;
    %             false -> loop(State)
    %         end
    end.

handle_call(_Request, _From, State) -> {noreply, State}.

terminate(_Reason, _State) -> none.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, _State, _Extra) -> {error, "Not implemented"}.

%%%%%%%
% Private functions

radio_status(UniversalRemote) ->
    From = self(),
    case UniversalRemote ! {stereo, display, From} of
        ok -> on;
        no_answer -> off
    end.

cmd_async(Command) ->
    spawn(fun() -> os:cmd(Command) end).


-module(light).
-behavior(gen_server).

-export([start_link/0, init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2, code_change/3, sequence/2, scene/2]).

-define (HUE_ADDR, "http://[redacted]/api/").
-define (USERNAME, "[redacted]").
-define (LIGHT_DELAY, 10000).

get([], ValOrMap) -> ValOrMap;
get([H|T], ValOrMap) -> get(T, maps:get(H, ValOrMap, not_found)).


%%%%%%%
% Public interface

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, []}.

sequence(Lights, Name) -> gen_server:cast(Lights, {sequence, Name}).

scene(Lights, Name) -> gen_server:cast(Lights, {scene, Name}).


%%%%%%%
% Callback interface

% TODO: Support cancelling a sequence by spawning it to a process
handle_cast(Request, State) ->
    case Request of
        {sequence, Name} ->
            run_sequence(Name);
        {scene, Name} ->
            set_scene(Name);
        Otherwise ->
            io:fwrite("Got message in light: ~p~n", [Otherwise])
    end,
    {noreply, State}.

terminate(_Reason, _State) -> none.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

% TODO: Support inserting new scenes and sequences in runtime
code_change(_OldVsn, _State, _Extra) -> {error, "Not implemented"}.

%%%%%%%
% Private functions

% Light module
set_attribute(Lamp, Attributes) ->
    hackney:start(), % todo: move to a better place?
    Method = put,
    Url = ?HUE_ADDR ++ ?USERNAME ++"/lights/" ++ Lamp ++ "/state",
    Headers = [],
    Payload = jiffy:encode(Attributes),
    Options = [],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {ok, 200, _respheaders, Clientref} ->
            {ok, _Body} = hackney:body(Clientref);
        Error ->
            {error, Error}
    end.

get_state() ->
    hackney:start(), % todo: move to a better place?
    Method = get,
    Url = ?HUE_ADDR ++ ?USERNAME ++"/lights",
    Headers = [],
    Payload = <<>>,
    Options = [],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {ok, 200, _respheaders, Clientref} ->
            {ok, Body} = hackney:body(Clientref),
            {ok, jiffy:decode(Body, [return_maps])};
        Error -> {error, Error}
    end.

lights_on() ->
    io:fwrite("lights_on called~n"),
    case get_state() of
        {ok, Lights} ->
            case get([<<"1">>, <<"state">>, <<"on">>], Lights) and
                 get([<<"2">>, <<"state">>, <<"on">>], Lights) of
                true -> already_on;
                false -> proceed
            end;
        Error -> Error
    end.



set_light(all, Temperature, Brigthness) ->
    set_light("1", Temperature, Brigthness),
    set_light("2", Temperature, Brigthness);
set_light(Lamp, Temperature, Brigthness) ->
    set_attribute(Lamp,
                  #{
                    <<"on">> => true,
                    <<"ct">> => trunc(Temperature),
                    <<"bri">> => trunc(Brigthness)
                   }).

set_scene(on) ->
    set_attribute(1, #{<<"on">> => true}),
    set_attribute(2, #{<<"on">> => true});
set_scene(off) ->
    set_attribute(1, #{<<"on">> => false}),
    set_attribute(2, #{<<"on">> => false}).

run_sequence(wake_up) ->
    case lights_on() of
        already_on -> skip;
        proceed ->
            LTemp = float(454), % Starting temperature (warmest possible)
            LBri = float(1), % Starting brightness
            set_light(all, LTemp, LBri),
            TTemp = 286, % Target temperature (white light)
            TBri = 254,

            Iterations = trunc((60 * 10)/(?LIGHT_DELAY/1000)), % 10 minutes
            StepTemp = ((TTemp-LTemp)/Iterations),
            StepBri = (TBri-LBri)/Iterations,
            run_sequence(wake_up, LTemp, LBri, StepTemp, StepBri, Iterations, 0);
        {error, Error} -> io:fwrite("Error: ~p~n", [Error])
    end.

run_sequence(wake_up, _, _, _, _, 0, Counter) ->
    io:fwrite("Wake up took ~p iterations~n", [Counter]),
    ok;

run_sequence(wake_up, LTemp, LBri, StepTemp, StepBri, Iterations, Counter) ->
    io:fwrite("Iterations left: ~p~n", [Iterations]),
    timer:sleep(?LIGHT_DELAY), % Wait from init
    % TODO: Check if lights has changed, to detect user interruption
    io:fwrite("LTemp is ~p. Subtracting ~p~n", [LTemp, StepTemp]),
    NewTemp = LTemp + StepTemp,
    NewBri = LBri + StepBri,
    io:fwrite("T: ~p, B: ~p~n", [NewTemp, NewBri]),
    set_light(all, NewTemp, NewBri),
    run_sequence(wake_up, NewTemp, NewBri, StepTemp, StepBri, Iterations - 1, Counter + 1).

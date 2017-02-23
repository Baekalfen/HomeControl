-module(rs232).

-export([start/2, start/3, init/2]).

start(Device, BaudRate) -> spawn(?MODULE, init, [Device, BaudRate]).
start(Node, Device, BaudRate) -> spawn(Node, ?MODULE, init, [Device, BaudRate]).

init(Device, BaudRate) ->
    loop(serial:start([{open, Device}, {speed, BaudRate}])).

loop(Serial) ->
    receive
        {send, Command, From} ->
            Serial ! {send, list_to_binary(Command)},
            From ! ok;
        {send, Command, From, Ref} ->
            Serial ! {send, list_to_binary(Command)},
            From ! ok
    end,
    loop(Serial).

% receive_data(Serial, Command, Data) ->
%     Serial ! {send, list_to_binary(Command)},
%     receive
%        {data, Bytes} ->
%            receive_data(Serial, Command, erlang:iolist_to_binary([Data, Bytes]))
%     after
%        250 -> Data
%     end.


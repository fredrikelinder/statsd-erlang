-module(statsd).

-behaviour(application).

%% API
-export([start/0, stop/0]).
-export([increment/1,
         increment/2,
         decrement/1,
         decrement/2,
         count/2,
         count/3,
         gauge/2,
         timing/2,
         timing/3]).

%% application behaviour
-export([start/2, stop/1]).

%% API

start() -> application:start(?MODULE).
stop()  -> application:stop(?MODULE).

%% Public: increments a counter by 1
%%
%% returns ok or {error, Reason}
increment(Key, Samplerate) ->
    count(Key, 1, Samplerate).
increment(Key) ->
    count(Key, 1).

%% Public: decrements a counter by 1
%%
%% returns ok or {error, Reason}
decrement(Key, Samplerate) ->
    count(Key, -1, Samplerate).
decrement(Key) ->
    count(Key, -1).

%% Public: increments a counter by an arbitrary integer value
%%
%% returns: ok or {error, Reason}
count(Key, Value) ->
    send({message, Key, Value, c}).
count(Key, Value, Samplerate) ->
    send({message, Key, Value, c, Samplerate}, Samplerate).

%% Public: sends an arbitrary gauge value
%%
%% returns: ok or {error, Reason}
gauge(Key, Value) ->
    send({message, Key, Value, g}).

%% Public: sends a timing in ms
%%
%% returns: ok or {error, Reason}
timing(Key, Value) ->
    send({message, Key, Value, ms}).
timing(Key, Value, Samplerate) ->
    send({message, Key, Value, ms, Samplerate}, Samplerate).


%% application callbacks

start(_Type, _Args) ->
    {ok, PoolSize} = application:get_env(statsd, pool_size),
    {ok, Host} = application:get_env(statsd, host),
    {ok, _Pid} = supervisor:start_link({local, statsd_sup}, statsd_sup, [PoolSize, Host]),
    ballermann:balance(statsd_sup, statsd_pool).

stop(_State) ->
    ok.


%% internal functions

%% Internal: prepares and sends the messages
%%
%% returns: ok or {error, Reason}
send(Message, Samplerate) when Samplerate =:= 1 ->
    send(Message);

send(Message, Samplerate) ->
    case random:uniform() =< Samplerate of
        true -> send(Message);
        _ -> ok
    end.

send(Message) ->
    gen_server:cast(ballermann:pid(statsd_pool),
        {send_message, build_message(Message)}).

%% Internal: builds the message string to be sent
%%
%% returns: a String
build_message({message, Key, Value, Type}) ->
    lists:concat([Key, ":", io_lib:format("~w", [Value]), "|", Type]);
build_message({message, Key, Value, Type, Samplerate}) ->
    lists:concat([build_message({message, Key, Value, Type}) | ["@", io_lib:format("~.2f", [1.0 / Samplerate])]]).

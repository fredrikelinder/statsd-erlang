-module(statsd_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% poolboy_worker behaviour
-export([start_link/1]).

%% holds all the relevant state that is used internally to pass the socket around
-record(state, {port, host, socket}).


%% API

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).


%% gen_server callbacks

%% Internal: used by gen_server and called on connection init
%%
%% returns: {ok, State}
init([{Host, Port}]) ->
    {ok, Socket} = gen_udp:open(0),
    State = #state{port = Port, host = Host, socket = Socket},
    {ok, State}.

%% Internal: used by gen_server and called on connection termination
%%
%% returns: {ok}
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    {ok, State}.

%% Internal: handles gen_server:call calls
%%
%% returns: {reply, ok|error, State}
handle_cast({send_message, Message}, State) ->
    gen_udp:send(State#state.socket, State#state.host, State#state.port, Message),
    {noreply, State}.
handle_call({send_message, Message}, _From, State) ->
    gen_udp:send(State#state.socket, State#state.host, State#state.port, Message),
    {ok, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
handle_info(_Info, State) -> {ok, State}.

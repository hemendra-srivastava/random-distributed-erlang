-module(calc).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(self(), stop).
 
init([]) ->
    add_sup:start_link(),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({get_sum, ReplyTo, Type, {Ele, Block}}, State) ->
    %%error_logger:info_msg("Received, ~p~n", [{Ele, Block}]),
    {ok, Pid} = add_sup:start_child({ReplyTo, Type, Ele, Block}),
    gen_server:cast(Pid, get_value),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, shutdown, State};
handle_cast(Msg, State) ->
    %%error_logger:info_msg("~p, ~p", [Msg, "why"]),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    

-module(adder).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

-record(state, {reply_to, type, element, cur_sum, blk}).

start_link(Args) ->
    error_logger:info_msg("~p ~n", [Args]),
    gen_server:start_link(?MODULE, [Args], []).

init([{ReplyTo, Type, Ele, Block}]) ->
    {ok, #state{reply_to=ReplyTo, type=Type, element=Ele, cur_sum=0, blk=Block}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(get_value, #state{blk={First, Last}, reply_to=ReplyTo} = State) when First >= Last ->
    gen_server:cast(ReplyTo, {State#state.type, State#state.cur_sum}),
    {stop, normal, State};
handle_cast(get_value, #state{element=Ele, blk={First, Last}, cur_sum=Sum} = State) ->
    gen_server:cast(self(), get_value),
    {noreply, State#state{blk={First+Ele, Last}, cur_sum=Sum+First}}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    








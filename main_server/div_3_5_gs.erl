-module(div_3_5_gs).
-behaviour(gen_server).

-export([start_link/0, find_sum/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(ADDER, calc).
-define(L1, [3,5]).
-define(L2, [15]).
-define(NUMBER, 100000000).
-define(NBlocks, 20).

-record(state, {current_sum=0, results_left}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(blocklist, _From, State) ->
    {ok, Blocklist} = list_of_blocks({0, ?NUMBER, ?NBlocks, ?NUMBER div ?NBlocks}, []),
    Len = length(Blocklist)*(length(?L1) + length(?L2)),
    {reply, {ok, Blocklist}, State#state{results_left=Len}}.


handle_cast({Type, Sum}, #state{current_sum = CurSum, results_left=1}) ->
    case Type of
	positive ->
	    FinalSum = CurSum + Sum;
	negative ->
	    FinalSum = CurSum - Sum
    end,
    {stop, normal, #state{current_sum = FinalSum, results_left=0}};

handle_cast({Type, Sum}, #state{current_sum=CurSum, results_left=ResLeft}) ->
    case Type of
	positive ->
	    FinalSum = CurSum + Sum;
	negative ->
	    FinalSum = CurSum - Sum
    end,
    {noreply, #state{current_sum=FinalSum, results_left=ResLeft-1}}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    error_logger:info_msg("Final Answer ~p ~n",[State#state.current_sum]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_sum() ->
    {ok, Blocklist} = gen_server:call(?SERVER, blocklist),
    lists:foreach(fun calculate_sum/1, Blocklist).

calculate_sum(Block) ->
    {ok, Ct} = get_sum(Block, ?L1, ?L2, get_nodelist(), 0).
    
get_first_no(N, K) when N div K == N/K ->
    N;
get_first_no(N, K) ->
    get_first_no(N+1, K).

get_sum(_Block, [], [], _Node, Ct) ->
    {ok, Ct};
get_sum(Block, L1, L2, [], Ct) ->
    get_sum(Block, L1, L2, get_nodelist(), Ct);
get_sum({First, Last} = Block, [], [Ele | Rest], [Node | Nodes], Ct) ->
    %%error_logger:info_msg("~p ~p ~p~n", [{?ADDER, Node}, Ele, neg]),
    gen_server:cast({?ADDER, Node}, {get_sum, {?SERVER, node()}, negative, {Ele, {get_first_no(First, Ele), Last}}}),
    get_sum(Block, [], Rest, Nodes, Ct+1); 
get_sum({First, Last} = Block, [Ele | Rest], L2, [Node | Nodes], Ct) ->
    %%error_logger:info_msg("~p ~p ~p~n", [{?ADDER, Node}, Ele, pos]),
    gen_server:cast({?ADDER, Node}, {get_sum, {?SERVER, node()}, positive, {Ele, {get_first_no(First, Ele), Last}}}),
    get_sum(Block, Rest, L2, Nodes, Ct+1).

get_nodelist() ->
    [node() | nodes()].

%%%%%%%%% Creates list of blocks based on K
 
list_of_blocks({StartNo, EndNo, 0, _Diff}, List) when EndNo > StartNo ->
    {ok, [{StartNo, EndNo} | List]};
list_of_blocks({StartNo, EndNo, 0, _Diff}, List) ->
    {ok, List};
list_of_blocks({StartNo, EndNo, K, Diff}, List) when K > 0->
    NewEnd = EndNo - Diff,
    list_of_blocks({StartNo, NewEnd, K-1, Diff}, [{NewEnd, EndNo} | List]);
list_of_blocks(_, _) ->
    {error, listofblocks}.



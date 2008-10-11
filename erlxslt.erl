%%%-------------------------------------------------------------------
%%% File    : erlxslt.erl
%%% Author  :  <>
%%% Description : 
%%%
%%% Created : 21 Jul 2008 by  <>
%%%-------------------------------------------------------------------
-module(erlxslt).

-behaviour(gen_server).

%% API
-export([start_link/0, register_function/4, process/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port, functions = []}).

%%====================================================================
%% API
%%====================================================================

register_function(X, Xmlns, Name, Fun) ->
    gen_server:call(X, {register_function, Xmlns, Name, Fun}).

process(X, Xslt, Xml) ->
    {ok, Data} = gen_server:call(X, {process, Xslt, Xml}),
    Data.

stop(X) ->
    gen_server:cast(X, {stop}).    

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Port = open_port({spawn, "./erlxslt"}, [{packet, 4}, binary]),
    {ok, #state{port=Port}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({process, Xslt, Xml}, _From, State = #state{port = Port}) ->
    port_command(Port, [1 | Xslt]),
    port_command(Port, [2 | Xml]),
    port_command(Port, [3]),
    Result = wait_result(State),
    {reply, {ok, Result}, State};

handle_call({register_function, Xmlns, Name, Fun}, _From,
	    State = #state{functions = Functions,
			   port = Port}) ->
    port_command(Port, lists:flatten([4, Xmlns, 0, Name])),
    {reply, ok, State#state{functions = [{Xmlns, Name, Fun} | Functions]}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

wait_result(#state{functions = Functions} = State) ->
    io:format("wait_result ~p~n",[Functions]),
    receive
	{_, {data, <<0, Result/binary>>}} ->
	    binary_to_list(Result);
	{Port, {data, <<1, Call/binary>>}} ->
	    io:format("call ~p in ~p~n",
		      [binary_to_term(Call),
		       Functions]),
	    Retval = call_function(binary_to_term(Call),
				   Functions),
	    io:format("retval: ~p~n",[term_to_binary(Retval)]),
	    port_command(Port, term_to_binary(Retval)),
	    wait_result(State)
    end.

call_function([Xmlns, Name | Args] = Call, [{Xmlns, Name, Fun} | Functions]) ->
    ArgsLen = length(Args),
    case erlang:fun_info(Fun, arity) of
	{arity, ArgsLen} ->
	    Args2 = lists:reverse(Args),
	    try apply(Fun, Args2) of
		R -> R
	    catch _:Reason ->
			  error_logger:error_msg("Error calling XSLT ext function with arguments:~n~p ~p:~nReason: ~p~n",
						 [Fun, Args2, Reason]),
			  ""
		  end;
	_ ->
	    call_function(Call, Functions)
    end;
call_function(Call, [_ | Functions]) ->
    call_function(Call, Functions);
call_function([Xmlns, Name | Args], []) ->
    error_logger:error_msg("Unable to find registered function {~s}~s with arity ~B~n",
			   [Xmlns, Name, length(Args)]),
    "".


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
-export([start_link/0, set_xslt/3, set_xml/3, set_params/2, process/1, exit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port, requestor = none}).

-define(CMD_SET_XSLT, 1).
-define(CMD_SET_XML, 2).
-define(CMD_PROCESS, 3).
-define(CMD_SET_PARAMS, 6).
-define(RPL_RESULT, 4).
-define(RPL_ERROR, 5).

%%====================================================================
%% API
%%====================================================================

set_xslt(X, Uri, Xslt) ->
    gen_server:cast(X, {set_xslt, Uri, Xslt}).

set_xml(X, Uri, Xml) ->
    gen_server:cast(X, {set_xml, Uri, Xml}).

set_params(X, Params) ->
    gen_server:cast(X, {set_params, Params}).

process(X) ->
    gen_server:call(X, {process}).

exit(X) ->
    gen_server:cast(X, {exit}).    

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
    process_flag(trap_exit, true),
    %Port = open_port({spawn, "valgrind -v --leak-check=full ./erlxslt"}, [{packet, 4}]),
    Port = open_port({spawn, "./erlxslt"}, [{packet, 4}]),
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
handle_call({process}, From, State = #state{port = Port,
					    requestor = none}) ->
    port_command(Port, [?CMD_PROCESS]),
    {noreply, State#state{requestor = From}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({set_xslt, Uri, Xslt}, State = #state{port = Port}) ->
    Buf = lists:append([[?CMD_SET_XSLT | Uri], [0], Xslt]),
    port_command(Port, Buf),
    {noreply, State};
handle_cast({set_xml, Uri, Xml}, State = #state{port = Port}) ->
    Buf = lists:append([[?CMD_SET_XML | Uri], [0], Xml]),
    port_command(Port, Buf),
    {noreply, State};
handle_cast({set_params, Params}, State = #state{port = Port}) ->
    Buf = lists:flatten(
	    lists:map(fun({K, V}) ->
			      lists:append([K, [0], V, [0]])
		      end, Params)),
    port_command(Port, [?CMD_SET_PARAMS | Buf]),
    {noreply, State};
handle_cast({exit}, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, State = #state{port = Port,
						   requestor = Requestor}) ->
    case Data of
	[?RPL_RESULT | Result] ->
	    gen_server:reply(Requestor, {ok, Result});
	[?RPL_ERROR | Error] ->
	    gen_server:reply(Requestor, {error, Error})
    end,
    {noreply, State#state{requestor = none}};
handle_info({'EXIT', Port, Reason}, State = #state{port = Port,
						    requestor = Requestor})
  when Requestor =/= none ->
    gen_server:reply(Requestor, {error, Reason}),
    {stop, Reason, State};
handle_info(_Info, State) ->
    io:format("handle_info(~p, ~p)~n", [_Info, State]),
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

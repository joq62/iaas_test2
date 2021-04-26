%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm  
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(orch). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("src/compute.hrl").
%% --------------------------------------------------------------------

%-define(UpdateInterval,10*1000).
-define(UpdateInterval,3*1000).
-define(GitDeployment,"glurk ").
-define(GitCatalog,"glurk ").


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{deployment,
	       catalog}).

%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


%% server interface

%%-- Handle applications
-export([update/0,
	 ping/0	 
	]).




-export([start/0,
	 stop/0
	 ]).
%% internal 
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

       
%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------
ping()->
    gen_server:call(?MODULE,{ping},infinity).

%%___________________________________________________________________

update()->
    gen_server:cast(?MODULE,{update}).

%%-----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,Catalog}=rpc:call(node(),orch_lib,get_catalog,[],10*1000),
    {ok,Deployment}=rpc:call(node(),orch_lib,get_deployment,[],10*1000),
    S=self(),
    spawn(fun()->local_update(S) end),  
    {ok, #state{deployment=Deployment,
		catalog=Catalog}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------


handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,?LINE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({update}, State) ->
    S=self(),
    spawn(fun()->local_update(S) end),  
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info({update,Catalog,Deployment}, State) ->
    io:format("Catalog ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Catalog}]),
    io:format("Deployment ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Deployment}]),
    {noreply, State#state{deployment=Deployment,
			  catalog=Catalog}};

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
local_update(Pid)->
    {ok,Catalog}=rpc:call(node(),orch_lib,get_catalog,[],10*1000),
    {ok,Deployment}=rpc:call(node(),orch_lib,get_deployment,[],10*1000),
 
%    io:format("local_update(Pid) ~p~n",[{?MODULE,?LINE,Pid}]),
    Pid!{update,Catalog,Deployment},
    timer:sleep(?UpdateInterval),
    ?MODULE:update().



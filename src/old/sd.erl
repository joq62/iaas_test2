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
-module(sd). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("src/compute.hrl").
%% --------------------------------------------------------------------
-define(UpdateInterval,10*1000).
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{running}).

%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


%% server interface

%%-- Handle applications
-export([get/1,
	 update/0,
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
get(Application)->
    gen_server:call(?MODULE,{get,Application},infinity).    
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
    AllRunningApps=[{Node,rpc:call(Node,application,which_applications,[])}||Node<-nodes()],   
    S=self(),
    spawn(fun()->local_update(S) end),  
    {ok, #state{running=[AllRunningApps]}}.

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
handle_call({get,all}, _From, State) ->
    Reply=State#state.running,
    {reply, Reply, State};

handle_call({get,Application}, _From, State) ->
    Reply=[Node||{Node,AppList}<-State#state.running,
		 true==lists:keymember(Application,1,AppList)],
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    mnesia:stop(),
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
 %   Reply=[Node||{Node,AppList}<-State#state.running,
%		 true==lists:keymember(common,1,AppList)],
 %   io:format("update  ~p~n",[{?MODULE,?LINE,Reply}]),
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

handle_info({all_running_apps,AllRunningApps}, State) ->
    {noreply, State#state{running=AllRunningApps}};
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
    AllRunningApps=[{Node,rpc:call(Node,application,which_applications,[])}||Node<-nodes()],   
%    io:format("local_update(Pid) ~p~n",[{?MODULE,?LINE,Pid}]),
    Pid!{all_running_apps,AllRunningApps},
    timer:sleep(?UpdateInterval),
    sd:update().

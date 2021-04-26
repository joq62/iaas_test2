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
-module(iaas). 
 
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(check_started_extra_node_time_out,3000).
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{nodes,
	       status}).

%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


%% server interface

%%-- Handle applications
-export([
	 install/0
	 %start_master/2,
	 %start_slave/2
	]).

-export([ping/0	 
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



%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%start_master(Ip,SshPort,Uid,Pw,VmId)->
%    gen_server:call(?MODULE,{start_master,Ip,SshPort,Uid,Pw,VmId},infinity).
%delete_vm(Vm)->
%    gen_server:call(?MODULE,{delete_vm,Vm},infinity).


install()->
    gen_server:call(?MODULE,{install},infinity).
 
ping()->
    gen_server:call(?MODULE,{ping},infinity).

%%___________________________________________________________________

check_started_extra_node()->
    gen_server:cast(?MODULE,{check_started_extra_node}).

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
%    {ok,FileName}=application:get_env(config_file),
%    {ok,[Nodes]}=file:consult(FileName),
%    ExternalNodes=lists:delete(node(),Nodes),
%    [net_adm:ping(Node)||Node<-ExternalNodes],
%    ok=application:start(gen_mnesia),
   % io:format("~p~n",[ExternalNodes]),
    {ok, #state{}}.

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
handle_call({create_vm}, _From, State) ->
    Reply=rpc:call(node(),compute_lib,create_vm,[]),
    {reply, Reply, State};

handle_call({delete_vm,Vm}, _From, State) ->
    Reply=rpc:call(node(),compute_lib,delete_vm,[Vm]),
    {reply, Reply, State};



handle_call({install}, _From, State) ->
    Reply=rpc:call(node(),iaas_lib,install,[]),
    {reply, Reply, State};


handle_call({start_vm}, _From, State) ->
    Reply=rpc:call(node(),compute_lib,start_vm,[]),
    {reply, Reply, State};


handle_call({read_status}, _From, State) ->
    Reply=State#state.status,
    {reply, Reply, State};

handle_call({set_status,Status}, _From, State) ->
    NewState=State#state{status=Status},
    spawn(fun()->local_check_started_extra_node(State#state.nodes) end),
    Reply=ok,
    {reply, Reply, NewState};


handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};


handle_call({sys_info}, _From, State) ->
    Reply=mnesia:system_info(),
    {reply, Reply, State};

handle_call({create_table,Table,Args}, _From, State) ->
    Reply=rpc:call(node(),gen_mnesia_lib,create_table,[Table,Args]),
    {reply, Reply, State};

handle_call({add_table,Vm,Table,StorageType}, _From, State) ->
    Reply=rpc:call(node(),gen_mnesia_lib,add_table,[Vm,Table,StorageType]),
    {reply, Reply, State};


handle_call({delete_node,Vm}, _From, State) ->
    Reply=rpc:call(Vm,application,stop,[gen_mnesia]),
    {reply, Reply, State};

handle_call({add_node,Vm}, _From, State) ->
    Reply=case net_adm:ping(Vm) of
	      pong->
		  rpc:call(Vm,application,stop,[gen_mnesia]),
		  ok=rpc:call(Vm,mnesia,delete_schema,[[Vm]]),
		  case rpc:call(Vm,application,start,[gen_mnesia]) of
		      ok->
			  case mnesia:change_config(extra_db_nodes, [Vm]) of
			      {ok,[Vm]}->
				  ok;
			      Err->
				  {error,[Err,Vm,?MODULE,?FUNCTION_NAME,?LINE]}
			  end;
		      Err->
			  {error,[Err,Vm,?MODULE,?FUNCTION_NAME,?LINE]}
		  end;
	      pang ->
		  {error,[not_running,Vm,?MODULE,?FUNCTION_NAME,?LINE]}
	   end,  
    {reply, Reply, State};

handle_call({init_table_info,Info}, _From, State) ->
    Reply=gen_mnesia_lib:create_table(Info),
    {reply, Reply, State};

handle_call({delete_schema_file}, _From, State) ->
    Reply=os:cmd("rm -rf Mne*"),
    {reply, Reply, State};

handle_call({load_textfile,FileName}, _From, State) ->
    Reply=mnesia:load_textfile(FileName),
 %   file:delete(Filename),
    {reply, Reply, State};

handle_call({load_textfile,Filename,Bin}, _From, State) ->
    file:delete(Filename),
    ok=file:write_file(Filename,Bin),
    Reply=mnesia:load_textfile(Filename),
 %   file:delete(Filename),
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

handle_cast({check_started_extra_node}, State) ->
    spawn(fun()->local_check_started_extra_node(State#state.nodes) end),  
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
local_check_started_extra_node(ExtraNodes)->
    timer:sleep(?check_started_extra_node_time_out),
    Locked=db_lock:is_open(?MODULE),
 %   io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Locked}]),
    case Locked of
	false->
	    do_nothing;
	true->
	    rpc:call(node(),compute_lib,start_restarted_nodes,[ExtraNodes])
    end,
    compute:check_started_extra_node().

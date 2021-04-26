%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(orch_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("src/db_lock.hrl").
-include("src/compute.hrl").
%% --------------------------------------------------------------------
-define(Catalog,"apps.catalog").
-define(Deployment,"deployment.config").

%% External exports
-export([get_catalog/0,
	 get_deployment/0,
	 install/1,
	 start_restarted_nodes/1,
	 create_vm/0,
	 delete_vm/1,
	 git_load_start_app/5,
	 stop_unload_app/4
	 
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================
get_catalog()->
    {ok,Catalog}=file:consult(?Catalog),
    {ok,Catalog}.
get_deployment()->
    {ok,Deployment}=file:consult(?Deployment),
    {ok,Deployment}.

check()->
    % git clone deployment.config [{Node,app}]
    {ok,Deployment}=file:consult("deployment.config"),
    {ok,Catalog}=file:consult("apps.catalog"),
   % io:format("Deployment = ~p~n",[Deployment]),
 
    AllRunningApps=sd:get(all),  % All=[{Node,[{app,"app_info",Vsn}]
    check(AllRunningApps,Catalog,Deployment,[],[]).

check([],_,_,StartApps,StopApps)->
    {StartApps,StopApps};
check([{Node,AppList}|T],Catalog,Deployment,StartApps,StopApps)->
   % io:format("Node,AppList = ~p~n",[{Node,AppList}]),
  %  io:format("Deployment = ~p~n",[Deployment]),
    FilteredAppListOnNode=[{Node,App}||{App,_,_}<-AppList,
				       true==lists:keymember(App,1,Catalog)],

   % io:format("FilteredAppListOnNode = ~p~n",[FilteredAppListOnNode]),
    AppsToDeployOnNode=[{XNode,XApp}||{XNode,XApp}<-Deployment,
		     XNode==Node],
   % io:format("AppsToDeployOnNode = ~p~n",[AppsToDeployOnNode]),
    Start=[{ZNode,ZApp}||{ZNode,ZApp}<-AppsToDeployOnNode,
			 false==lists:member({ZNode,ZApp},FilteredAppListOnNode)],

    Stop=[{ZNode,ZApp}||{ZNode,ZApp}<-FilteredAppListOnNode,
			 false==lists:member({ZNode,ZApp},AppsToDeployOnNode)],
    
   % Stop=[{YNode,YApp}||{YNode,YApp}<-Deployment,
%				 false==lists:member({YNode,YApp},FilteredAppList)],
 %   Stop=[],
 %   io:format("Start = ~p~n",[Start]),
%    io:format("StopApps = ~p~n",[StopApps]),
    check(T,Catalog,Deployment,
	  lists:append(Start,StartApps),
	  lists:append(Stop,StopApps)).


git_load_start_app(Slave,AppId,GitCmd,DestDir,PathList)->
    rpc:call(Slave,os,cmd,["rm -rf "++DestDir]),
    rpc:call(Slave,os,cmd,[GitCmd]),
    %% Add path to vm and start the application 
    {ok,DirParent}=rpc:call(Slave,file,get_cwd,[]),
    FullNamePathList=[filename:join([DirParent,DestDir,Path])||Path<-PathList],
    [rpc:call(Slave,code,add_patha,[FullNamePath])||FullNamePath<-FullNamePathList],
    ok=rpc:call(Slave,application,start,[list_to_atom(AppId)]),
    App=list_to_atom(AppId),
    {pong,Slave,App}=rpc:call(Slave,list_to_atom(AppId),ping,[]),
    ok.
  
stop_unload_app(Slave,AppId,DestDir,PathList)->
    ok=rpc:call(Slave,application,stop,[list_to_atom(AppId)]),
    {ok,DirParent}=rpc:call(Slave,file,get_cwd,[]),
    FullNamePathList=[filename:join([DirParent,DestDir,Path])||Path<-PathList],
    [rpc:call(Slave,code,del_path,[FullNamePath])||FullNamePath<-FullNamePathList],
    {badrpc,_}=rpc:call(Slave,list_to_atom(AppId),ping,[]),
    ok=rpc:call(Slave,application,unload,[list_to_atom(AppId)]),
    rpc:call(Slave,os,cmd,["rm -rf "++DestDir]),
    AppFile=AppId++".app",
    {error,{"no such file or directory",AppFile}}=rpc:call(Slave,application,start,[list_to_atom(AppId)]),
    ok.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create_vm()->
    NodeNameStr="worker_"++erlang:integer_to_list(erlang:system_time(millisecond)),
    ok=file:make_dir(NodeNameStr),
    {ok,Host}=inet:gethostname(),
    NodeName=list_to_atom(NodeNameStr),   
    {ok,Node}=slave:start(Host,NodeName,"-setcookie "++?Cookie),
    {ok,Node}.

delete_vm(Vm)->
    VmId=atom_to_list(Vm), % 'worker_xxxx@Host"
    io:format("~p~n",[{VmId,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("~p~n",[{string:tokens(VmId,"@"),?MODULE,?FUNCTION_NAME,?LINE}]),
    [NodeNameStr,Host]=string:tokens(VmId,"@"),
    io:format("~p~n",[{NodeNameStr,Host,?MODULE,?FUNCTION_NAME,?LINE}]),
    os:cmd("rm -rf "++NodeNameStr), % del_dir_r OTP 23!!!!
    slave:stop(Vm),
    ok.
    

load_start_service(ServiceId,Source,DestinationDir,WorkerVm)->
    glurk.
    
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

start_restarted_nodes(ExtraNodes)->
    start_restarted_nodes(ExtraNodes,[]).  
start_restarted_nodes([],R)->
    R;
start_restarted_nodes([Node|T],Acc) ->
    R=case rpc:call(Node,compute,read_status,[]) of
	  started->
	      case gen_mnesia:add_node(Node) of
		  ok->
		      case rpc:call(Node,compute,set_status,[running]) of
			  ok->
			      ok;
			  Err ->
			      {error,[Err,Node,compute,set_status,[running],?MODULE,?FUNCTION_NAME,?LINE]}
		      end;
		  Err->
		      {error,[Err,Node,add_node,?MODULE,?FUNCTION_NAME,?LINE]}
	      end;
	  running->
	      ok;
	  Err ->
	      
	      {error,[Err,Node,compute,read_status,[],?MODULE,?FUNCTION_NAME,?LINE]}
      end,
    start_restarted_nodes(T,[R|Acc]).
			      


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

install(ExternalNodes)->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    Table=lock,
    % init local
    ok=db_lock:create_table(),
    {atomic,ok}=db_lock:create(?DBASE_LEADER), 
    add_extra_nodes(ExternalNodes,Table,[]).
    
    % Add extra nodes
add_extra_nodes([],_Table,Result)->
    Result;
add_extra_nodes([Node|T],Table,Acc)->
    R=case net_adm:ping(Node) of
	  pang->
	      {error,[pang,Node,?MODULE,?FUNCTION_NAME,?LINE]};
	  pong->    %add Nodes
    	      case gen_mnesia:add_node(Node) of
		  ok->
		      StorageType=ram_copies,    
		      case gen_mnesia:add_table(Node,Table,StorageType) of
			  ok->
			      case rpc:call(Node,compute,set_status,[running]) of
				  ok->
				      ok;
				  Err->
				      {error,[Err,Node,compute,set_status,[running],?MODULE,?FUNCTION_NAME,?LINE]}
			      end;
			  Err->
			      {error,[Err,add_table,Node,?MODULE,?FUNCTION_NAME,?LINE]}
		      end;
		  Err->
		      {error,[Err,add_node,Node,?MODULE,?FUNCTION_NAME,?LINE]}
	      end
      end,
    add_extra_nodes(T,Table,[R|Acc]).

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

check_stopped_db_nodes()->
    case get_stopped_nodes() of
	[]->
	    ok;
	StoppedExtraDbNodes->
	    add_started_nodes(StoppedExtraDbNodes)
    end,
    get_stopped_nodes().


add_started_nodes([])->
    ok;
add_started_nodes([Vm|T])->
    initiate_added_node(Vm),
    timer:sleep(100),
    add_started_nodes(T).
	    
get_stopped_nodes()->
    ExtraDbNodes=mnesia:system_info(extra_db_nodes),
    RunningExtraDbNodes=lists:delete(node(),mnesia:system_info(running_db_nodes)),
    StoppedExtraDbNodes=[Node||Node<-ExtraDbNodes,
			     false==lists:member(Node,RunningExtraDbNodes)],
    StoppedExtraDbNodes.


create_table(Table,Args)->
    {atomic,ok}=mnesia:create_table(Table,Args),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
initiate_added_node(Vm)->
    Result=case net_adm:ping(Vm) of
	       pong->
		   stopped=rpc:call(Vm,mnesia,stop,[]),
		   ok=mnesia:delete_schema([Vm]),
		   ok=rpc:call(Vm,mnesia,start,[]),
		   {ok,[Vm]}=mnesia:change_config(extra_db_nodes, [Vm]);
	       pang ->
		   {error,[not_running,Vm]}
	   end,    
    Result.


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

add_table(Vm,Table,StorageType)->
    mnesia:add_table_copy(Table,Vm,StorageType),
  %  [{Table,Args,_}]=db_gen_mnesia:read(Table),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------


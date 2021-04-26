%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc : represent a logical vm   
%%% 
%%% Supports the system with standard erlang vm functionality, load and start
%%% of an erlang application (downloaded from git hub) and "dns" support 
%%% 
%%% Make and start the board start SW.
%%%  boot_service initiates tcp_server and l0isten on port
%%%  Then it's standby and waits for controller to detect the board and start to load applications
%%% 
%%%     
%%% -------------------------------------------------------------------
-module(install_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
-define(MaxRandNum,5).

-define(IP,"192.168.1.50").
-define(Port,22).
-define(Uid,"joq62").
-define(Pw,"festum01").
-define(HostCheckTO,5000).
-define(HostConfigPath,"https://github.com/joq62/host_config.git").
-define(Cookie,"abc").
%% --------------------------------------------------------------------
-export([test/0
	]).

%% ====================================================================
%% External functions
%% ====================================================================

test()->
    % Some test clean up
    rpc:call('master@joq62-X550CA',init,stop,[]),
    ok=application:start(iaas),
    ok=iaas:install(),
    ok=mnesia_start_test(),
    
    ok.


init_dbase_test()->

    ok.    
    % Create iaas table
    
    
mnesia_start_test()->
    % Create locks
    [iaas]=db_lock:read_all(),
    true=db_lock:is_open(iaas),
    false=db_lock:is_open(iaas),
    
    
    % Create hosts_config table
    ok=db_hosts_config:create_table(),
    FileName="host.config",
    ConfigDir="configs",
    File=filename:join(ConfigDir,FileName),
    {atomic,ok}=db_hosts_config:create(FileName,?HostConfigPath,ConfigDir),
    [{FileName,GitPath,ConfigDir}]=db_hosts_config:read(FileName),
    case filelib:is_dir(ConfigDir) of
	true->
	    os:cmd("rm -rf "++File);
	false->
	    ok=file:make_dir(ConfigDir)
    end,
    % and Load from github and store mnesia 
    os:cmd("rm -rf "++ConfigDir),
    ok=db_hosts:create_table(),
    [{FileName,GitPath,ConfigDir}]=db_hosts_config:read(FileName),
    os:cmd("git clone "++GitPath++" "++ConfigDir),
    {ok,HostInfo}=file:consult(File),
    [db_hosts:create(Host,Ip,Port,?Uid,?Pw,SlaveVmIdList)||{{id,Host},
							    {ip,Ip},
							    {port,Port},
							    {slaves,SlaveVmIdList}}<-HostInfo],
    

    [{{id,'joq62-X550CA'},{ip,"192.168.1.50"},{port,22},
      {slaves,["slave0","slave1","slave2","slave3","slave4"]}},
     {{id,c0},{ip,"192.168.1.200"},{port,22},
      {slaves,["slave0","slave1","slave2","slave3","slave4"]}},
     {{id,c1},{ip,"192.168.1.201"},{port,22},
      {slaves,["slave0","slave1","slave2","slave3","slave4"]}},
     {{id,c2},{ip,"192.168.1.202"},{port,22},
      {slaves,["slave0","slave1","slave2","slave3","slave4"]}}]=HostInfo,
  
    %Create table and load
  
    % Test this part 

    [{"192.168.1.50",22,"joq62","festum01",
     ["slave0","slave1","slave2","slave3","slave4"]}]=db_hosts:read('joq62-X550CA'),
    [{"192.168.1.200",22,"joq62","festum01",
     ["slave0","slave1","slave2","slave3","slave4"]}]=db_hosts:read(c0),
    [{"192.168.1.201",22,"joq62","festum01",
     ["slave0","slave1","slave2","slave3","slave4"]}]=db_hosts:read(c1),
    [{"192.168.1.202",22,"joq62","festum01",
     ["slave0","slave1","slave2","slave3","slave4"]}]=db_hosts:read(c2),

    [{c2,"192.168.1.202",22,"joq62","festum01",
      ["slave0","slave1","slave2","slave3","slave4"]},
     {c1,"192.168.1.201",22,"joq62","festum01",
      ["slave0","slave1","slave2","slave3","slave4"]},
     {'joq62-X550CA',"192.168.1.50",22,"joq62","festum01",
      ["slave0","slave1","slave2","slave3","slave4"]},
     {c0,"192.168.1.200",22,"joq62","festum01",
      ["slave0","slave1","slave2","slave3","slave4"]}]=db_hosts:read_all(),
    

    %% Create Wanted state and check nodes to Restart and Stop
    AllHostInfo=db_hosts:read_all(),
    AllWanted=[[list_to_atom("master@"++atom_to_list(Host))|wanted_slaves(Host,Slaves)]||
		    {Host,_Ip,_Port,_Uid,_Pw,Slaves}<-AllHostInfo],
    WantedState=lists:merge(AllWanted),
    %Start master node
    ssh:start(),
    ErlCmd="erl -sname master -detached -setcookie "++?Cookie,
    AllHosts=db_hosts:read_all(),
    StartMasterResult=[start_master(XHostInfo,ErlCmd)||XHostInfo<-AllHosts],
     io:format("StartMasterResult ~p~n",[{StartMasterResult,?MODULE,?FUNCTION_NAME,?LINE}]),
    % Start slaves on running nodes 
    ErlSlave="-setcookie "++?Cookie,
    
    StartSlaveResults=[start_slave(Host,Vm,ErlSlave)||{pong,Host,Vm}<-StartMasterResult],
    StartSlaveResult=lists:merge(StartSlaveResults),
    timer:sleep(2000),
    % Check if restart a node
    AllNodes=[node()|nodes()],
    RestartNodes=[Node||Node<-WantedState,
			false==lists:member(Node,AllNodes)],
    io:format("RestartNodes ~p~n",[{RestartNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    % Check if stop 
    StopNodes=[Node||Node<-AllNodes,
			false==lists:member(Node,WantedState)],
    io:format("StopNodes ~p~n",[{StopNodes,?MODULE,?FUNCTION_NAME,?LINE}]),   

    ['master@c0','master@c1','master@c2','master@joq62-X550CA',
     'slave0@c0','slave0@c1','slave0@c2','slave0@joq62-X550CA',
     'slave1@c0','slave1@c1','slave1@c2','slave1@joq62-X550CA',
     'slave2@c0','slave2@c1','slave2@c2','slave2@joq62-X550CA',
     'slave3@c0','slave3@c1','slave3@c2','slave3@joq62-X550CA',
     'slave4@c0','slave4@c1','slave4@c2','slave4@joq62-X550CA']=WantedState,

    [{ok,'slave0@joq62-X550CA'},
     {ok,'slave1@joq62-X550CA'},
     {ok,'slave2@joq62-X550CA'},
     {ok,'slave3@joq62-X550CA'},
     {ok,'slave4@joq62-X550CA'}]=StartSlaveResult,
    
    ok.


wanted_slaves(Host,Slaves)->
    [list_to_atom(SlaveId++"@"++atom_to_list(Host))||SlaveId<-Slaves].


start_master({Host,Ip,Port,Uid,Pw,_},ErlCmd)->
 %   io:format("~p~n",[{Host,?MODULE,?FUNCTION_NAME,?LINE}]),
    TimeOut=2000,
    my_ssh:ssh_send(Ip,Port,Uid,Pw,ErlCmd,TimeOut),
    timer:sleep(100),
    Vm=list_to_atom("master@"++atom_to_list(Host)),
    R=net_adm:ping(Vm),
    io:format("~p~n",[{Vm,Host,R,?MODULE,?FUNCTION_NAME,?LINE}]),
    {R,Host,Vm}.


start_slave(Host,Vm,ErlSlave)->
    io:format("~p~n",[{Vm,Host,?MODULE,?FUNCTION_NAME,?LINE}]),
     [{_Ip,_Port,_Uid,_Pw,
       SlaveIds}]=db_hosts:read(Host),
    SlaveStartResult=[rpc:call(Vm,slave,start,[Host,SlaveId,ErlSlave],5000)||SlaveId<-SlaveIds],
    SlaveStartResult.

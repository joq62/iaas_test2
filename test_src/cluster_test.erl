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
-module(cluster_test). 

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
    ok=cluster:start(),
    ok=check_config(),
    ok=check_master_nods(),
  %  ok=check_dist_mnesia(),
    


    ok.

check_master_nods()->
    
    
    ok.

check_config()->
 	[{"host.config",
	  "https://github.com/joq62/host_config.git",
	  "configs"
	 }]=db_hosts_config:read_all(),
    ok.


init_dbase_test()->

    ok.    
    % Create iaas table
    
cluster_start_test()->

    

    ok.

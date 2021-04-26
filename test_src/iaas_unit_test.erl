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
-module(iaas_unit_test). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-define(WAIT_FOR_TABLES,10000).	  
-define(MaxRandNum,5).
%% --------------------------------------------------------------------
-export([test/0

	]).

%% ====================================================================
%% External functions
%% ====================================================================
node_name(Name)->
    {ok,Host}=inet:gethostname(),
    Node=list_to_atom(Name++"@"++Host),    
    Node.
host()->
    {ok,Host}=inet:gethostname(),
    Host.

vm_id(Vm)->
    [VmId,Host]=string:tokens(atom_to_list(Vm),"@"),
    VmId.

a_sysinfo()->
    {ok,Host}=inet:gethostname(),
    NodeA=list_to_atom("a@"++Host),
    rpc:call(NodeA,mnesia,system_info,[]).


%% --------------------------------------------------------------------
%% 
%% 
%% --------------------------------------------------------------------
test()->
    io:format("Start test ~p~n",[?MODULE]),
    io:format("install_test() ~n"),
    ok=install_test:test(),
%    io:format("iaas_test() ~n"),
 %    ok=iaas_test:start(),
%    ok=defines_test(),
 %   io:format("clean_start_test() ~n"),
 %   ok=clean_start_test(),
 %   io:format("connect_boards_test() ~n"),
%    ok=connect_boards_test(),
 %   io:format("start_slaves_test() ~n"),
 %   ok=start_slaves_test(),
    
     io:format("Successfully Stop test ~p~n",[?MODULE]),
    
    ok.
% Test defines



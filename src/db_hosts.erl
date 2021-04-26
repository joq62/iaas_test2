-module(db_hosts).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_hosts.hrl").

 

-define(TABLE,hosts).
-define(RECORD,hosts).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).


create(Id,Ip,Port,Uid,Pw,Slaves) ->
    F = fun() ->
		Record=#?RECORD{hostid=Id,ip=Ip,port=Port,uid=Uid,pw=Pw,slaves=Slaves},		
		mnesia:write(Record) end,
    mnesia:transaction(F).



read_all_info() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Id,Ip,Port,Uid,Pw,Slaves}||{?RECORD,Id,Ip,Port,Uid,Pw,Slaves}<-Z].

read_all() ->
   read_all_info().

read(Id) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.hostid==Id])),
    [{Ip,Port,Uid,Pw,Slaves}||{?RECORD,_Id,Ip,Port,Uid,Pw,Slaves}<-Z].

      
delete(Id) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.hostid==Id],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------

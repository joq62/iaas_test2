-module(db_hosts_config).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_hosts_config.hrl").

 

-define(TABLE,hosts_config).
-define(RECORD,hosts_config).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).


create(FileName,GitPath,ConfigDir) ->
    F = fun() ->
		Record=#?RECORD{file_name=FileName,git_path=GitPath,config_dir=ConfigDir},		
		mnesia:write(Record) end,
    mnesia:transaction(F).



read_all_info() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{FileName,GitPath,ConfigDir}||{?RECORD,FileName,GitPath,ConfigDir}<-Z].

read_all() ->
   read_all_info().

read(WantedFileName) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.file_name==WantedFileName])),
    [{FileName,GitPath,ConfigDir}||{?RECORD,FileName,GitPath,ConfigDir}<-Z].

      
delete(WantedFileName) ->
    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,WantedFileName}),
			    X#?RECORD.file_name==WantedFileName],
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

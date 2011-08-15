-module(common.db.filemaker).
-include("debug.hrl").

-import(lists).
-import(io_lib).
-import(proplists).
-import(mnesia).
-import(erlydb).
-import(erlydb_odbc).
-import(compile).
-import(string).
-import(filename).
-import(filelib).
-import(beam_lib).
-import(code).
-import(file).
-import(io).
-import(odbc).
-import(application).
%%-import(common.utils.lists, [errors/1]).
-import(common.utils.packages).
-import(common.db.db).

-define(driver, "DataDirect 32-BIT SequeLink 5.5").

-export([create_table/2, connect_string/1, connect_string/2, code_gen/2, 
	 describe_table/2]).
-export([options/1, options/3, encode/2, decode/2, tables/2]).
-export([test/1]).
%%-type(boolean() :: true | false).

%%delete_table(Name, Opts)
%%clear_table(Name, Opts) 
create_table(Name, Opts) when is_atom(Name) ->
    ErlyDBOpts = proplists:get_value(erlydb_opts, Opts, []),
    Options = proplists:delete(erlydb_opts, Opts) ++ ErlyDBOpts,
    erlydb_odbc:create_table(Name, Options).
    
-type(reason() :: term()). 
-type(proplist() :: [atom() | {atom(), term()}]).

-spec(code_gen/2 :: (
		     Tables :: [atom()], 
		     Options :: proplist()) -> true | {error, reason()}).
code_gen(Tables, Options) ->
    %% FIXME this is a HACK
    case erlydb_odbc:status() of
	{ok, not_running, _Name} -> erlydb:start(odbc, Options);
	_Else -> ok
    end,	
    case erlydb:code_gen(Tables, odbc, Options) of
	{error, _Reason} = Error ->
	    ?debug("ERLYDB_Error = ~p.",[Error],code_gen),
	    Error;
	OK -> OK
    end.
    
options(Options) ->
    [{erlydb_opts, 
      [{connection_fun, fun 'common.db.filemaker':connect_string/2},
       {decode_fun,     fun 'common.db.filemaker':decode/2},
       {encode_fun,     fun 'common.db.filemaker':encode/2},
       {describe_table_fun, fun 'common.db.filemaker':describe_table/2},
       {callback_module, 'common.db.filemaker'}]}] ++ Options.

describe_table(Table, _Options) -> db:describe_table(filemaker, Table).
    

tables(Driver, PoolId) -> db:tables(filemaker, []).			       
    
options(Driver, PoolId, Options) -> Options.   
    
    
-record(common_cm_data,
	{id :: integer(),
	 app :: atom(),
	 db_key :: term(),
	 default :: term(),
	 is_defined :: bool(),
	 notif :: [], %% if atom() we will send the message to 
	 %% process with such name??	 
	 value :: term(),
	 created_on :: term() %% this field required for erlydb.
	}).

connect_string(Options) ->
    connect_string(?driver, Options).
connect_string("DataDirect 32-BIT SequeLink 5.5" = Driver, Options) ->
    Port = proplists:get_value(port, Options),
    Hostname = proplists:get_value(hostname, Options),
    Username = proplists:get_value(username, Options),
    Password = proplists:get_value(password, Options),
    Connect = lists:flatten(
		io_lib:format("DRIVER={~s};HST=~s;PRT=~p;UID=~s;PWD=~s", 
			      [Driver, Hostname, Port, Username, Password])),
    Connect.

%% FileMaker supports only following types
%% varchar decimal date time timestamp binary
test(string) -> 
    Fun = fun 'common.db.filemaker':connect_string/2,
    Fun("DataDirect 32-BIT SequeLink 5.5", []);
test(create) ->
    Properties = 
	[
         %%{Field, {Type, Modifier}, Null, Key, Default, Extra, MnesiaType}
	 {id ,{decimal, undefined},false,primary,undefined,identity,integer},
	 {app,{varchar, undefined},false,undefined,undefined,undefined,atom},
	 {db_key, {binary, undefined},false,undefined,undefined,undefined,binary},
	 {is_defined,{varchar,undefined},false,undefined,undefined,undefined,
	  atom}
	],

    create_table(common_cm_data, 
 		 [
 		  {attributes, 
 		   record_info(fields, common_cm_data)},
		  {user_properties, Properties}
 		 ]).
%%    create_table(odbc, odbc_table, Opts).

%% from erlydb type to FileMaker type for TABLE CREATE query
encode(field_type, {Type, _}) -> encode(field_type, Type);
encode(field_type, varchar)   -> 'VARCHAR';
encode(field_type, decimal)   -> 'DECIMAL';
encode(field_type, date)      -> 'DATE';
encode(field_type, time)      -> 'TIME';
encode(field_type, timestamp) -> 'TIMESTAMP';
encode(field_type, datetime) -> 'TIMESTAMP';
encode(field_type, binary)    -> 'BLOB';
encode(field_type, Type)      -> {error, {unsupported_type, Type}}.
    
decode(field_type, 'SQL_VARCHAR')        -> varchar;
decode(field_type, 'SQL_DOUBLE')         -> decimal;
decode(field_type, 'SQL_DATE')           -> date;
decode(field_type, 'SQL_TIME')           -> time;
%decode(field_type, 'SQL_TYPE_TIMESTAMP') -> timestamp;
decode(field_type, 'SQL_TYPE_TIMESTAMP') -> datetime;
decode(field_type, 'SQL_LONGVARBINARY')  -> binary;
decode(field_type, Type)                 -> {error, {unsupported_type, Type}}.
     
tables(Options) -> maybe_todo.
    

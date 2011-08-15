-module(common.db.db).
-include("debug.hrl").

-import(lists).
-import(proplists).
-import(mnesia).
-import(erlydb).
-import(code).
-import(file).
-import(io).
-import(application).
-import(common.utils.lists, [errors/1]).
-import(common.node.code, [get_modules_with_fun/2]).
-import(common.node.init).
-import(common.node.node).
-import(common.utils.packages).
%%-import(common.node.code).

-export([create_table/3, create_tables/2, test/0, start/1, load_table/2, load_tables/2,
	get_modules/0, local_tables/1]).
-export([tables/2, describe_table/2, options/3]).
-export([test/1]).
-type(boolean() :: true | false).

%%{Field, {Type, Modifier}, Null, Key, Default, Extra, DBType}
-record(field, {field :: atom(),
		type :: {Type :: atom(), Modifier :: term()},
		null :: boolean(),
		key :: primary | undefined,
		default :: undefined,
		extra :: identity | undefined,
		db_type :: atom()}).

-record(common_db, {table :: {Table :: atom(), Backend :: atom()},
			   fields :: [#field{}], options = []}).
-define(match_pattern_backend(Backend), 
	#common_db{table = {'_', Backend}, fields = '_', options = '_'}).
register_table(Type, Name, Options) ->
    Fields = proplists:get_value(attributes, Options),
    Properties = case proplists:get_value(user_properties, Options) of
		     undefined -> [field(Field) || Field <- Fields];
		     List -> List
		 end,
    ErlyDBOpts = proplists:get_value(erlydb_opts, Options, []),
    Trans = fun() -> 
		    mnesia:write(#common_db{table = {Name, Type},
						   fields = Properties, 
						   options = ErlyDBOpts})
	    end,
    ?assert({atomic, ok} =:= mnesia:transaction(Trans)),
    proplists:delete(erlydb_opts, Options).

describe_table(BackEnd, Table) ->
    Trans = fun() -> 
		    mnesia:read(common_db, {Table, BackEnd})
	    end,
    case mnesia:transaction(Trans) of
	{atomic, []} -> 
%%	    ?debug("",[], describe_table),
	    {error, {unknown_table, Table}};
	{atomic, [Description]} -> 
	    %%mnesia:transaction(Trans),
	    {ok, [new_field(F) || F <- Description#common_db.fields]};
	{aborted, Reason} = Error -> {error, {cannot_describe_table, Error}}
    end.    
	    
%% TODO move this to erlydb_odbc
field({Field, Type, Null, Key, Default, Extra, DBType}) ->
    field({Field, Type, Null, Key, Default, Extra, DBType, []});
field({Field, Type, Null, Key, Default, Extra, DBType, Attributes}) ->
    #field{field = Field, type = Type, null = Null, key = Key, 
	   default = Default, extra = Extra, db_type = DBType};
field(Name) ->
    #field{field = Name, type = {binary, undefined}, 
	   null = false, db_type = binary}.
new_field(#field{field = Field, type = Type, null = Null, key = Key, 
		 default = Default, extra = Extra, db_type = DBType}) -> 
    {Field, Type, Null, Key, Default, Extra, DBType};
new_field({Field, Type, Null, Key, Default, Extra, DBType}) -> 
new_field({Field, Type, Null, Key, Default, Extra, DBType, []});
new_field({Field, Type, Null, Key, Default, Extra, DBType, Attributes}) -> 
    {Field, Type, Null, Key, Default, Extra, DBType, Attributes}.
    
create_table(Type, Name, Opts) ->
    case backend(Type) of
	{error, _Reason} = Error -> Error;
	Module -> 
	    case create_module(Name) of
		{module, Name} -> 
%%		    Options = options(Module:options(Opts)),
		    Options = options(Module, Opts),
		    case Module:create_table(Name, Options) of
			{atomic, ok} -> 
			    register_table(Type, Name, Options),
			    {atomic, ok};
			Create_Error -> 
			    ?debug("Cannot create table, reason ~p.",
				   [Create_Error], create_table),
			    Create_Error
		    end;
		Module_Error -> 
		    ?debug("Cannot create module, reason ~p.",
			   [Module_Error], create_table),
		    Module_Error
	    end
    end.

backend(mnesia) -> common.db.mnesia;
backend(filemaker) -> common.db.filemaker;
backend(BackEnd) -> {error, {unsupported_backend, BackEnd}}.
    
local_tables(Type) -> 
    case backend(Type) of
	{error, _Reason} = Error -> Error;
	BackEnd -> BackEnd:local_tables()
    end.

%% TODO filemaker hardcoded for now
start(_Args) -> 
    ?assert(ok =:= erlydb:start(mnesia)),
    Mnesia_Tables = tables(mnesia), %% init starts from here if necessary
    ?debug("Found tables ~p.",[Mnesia_Tables],start),
    case Mnesia_Tables of 
	Mnesia_Tables when is_list(Mnesia_Tables) -> 	    
	    case errors(load_tables(mnesia, Mnesia_Tables)) of
		[] -> 
		    SQL_Tables = tables(filemaker),
		    case SQL_Tables of 
			SQL_Tables when is_list(SQL_Tables) ->   
			    case errors(load_tables(filemaker, SQL_Tables)) of
				[] -> {atomic, ok};
				Load_SQL_Errors -> 
				    ?debug("Cannot load sql tables, reason ~p.",
					   [Load_SQL_Errors], start),
				    Load_SQL_Errors
			    end;
			SQL_Error -> 
			    ?debug("Cannot determine list of tables, reason ~p.",
				   [SQL_Error],start),
			    SQL_Error
		    end;
		Load_Mnesia_Errors -> 
		    ?debug("Cannot load mnesia tables, reason ~p.",
			   [Load_Mnesia_Errors],start),
		    Load_Mnesia_Errors
	    end;
	Mnesia_Error -> Mnesia_Error
    end.
    

tables(Backend) -> tables(Backend, undefined).    

%% here we miss ram_copy tables
tables(mnesia, _) ->
    ?debug("~p, ~p", [mnesia:system_info(directory), local_tables(mnesia)],
tables),
    case {mnesia:system_info(extra_db_nodes), local_tables(mnesia)} of
	{[], [schema]} -> %% not initiated server
	    init(node()),
	    local_tables(mnesia);
	{[], List} -> List; %% ready server
	{Nodes, _} -> %% diskless node
	    [mnesia:add_table_copy(schema, N, ram_copies) || N <- Nodes],
	    ?debug("TABLES:", [mnesia:system_info(tables)], tables),
	    mnesia:system_info(tables)
    end;

tables(BackEnd, Options) ->
    Trans = fun() -> 
		    mnesia:match_object(?match_pattern_backend(BackEnd))
	    end,
    case mnesia:transaction(Trans) of
	{atomic, Records} ->
	    ?debug("Records = ~p",[Records],tables),
	    [Table || #common_db{table = {Table, BackEnd}} <- Records];
	_Any -> []
    end.

load_tables(Backend, List) ->
    load_tables(Backend, List, []).
load_tables(Backend, [], Res) -> Res;
load_tables(Backend, [Name | Rest], Res)->
    load_tables(Backend, Rest, [{Name, load_table(Backend, Name)}] ++ Res).
    
load_table(mnesia, schema) -> true; %% skip schema
load_table(mnesia, counter) -> true; %% skip counter special table for erlydb
load_table(mnesia, common_db) -> true; %% skip special table for self()
load_table(Backend, Name) ->
     case code:which(Name) of
 	false -> true;
 	non_existing -> 
 	    case create_module(Name) of
 		{module, Name} -> 
		    Options = options(Backend, Name, []),
 		    case code_gen(Backend, [Name], Options) of
 			ok -> 
 			    true;
 			ERLYDB_Error -> {error, ERLYDB_Error}
 		    end;
 		Module_Error -> false
 	    end;
 	Path -> 
	     %% FIX ME 
	     code:purge(Name),
	     code:delete(Name),
	     Options = options(Backend, Name, []),
	     case code_gen(Backend, [Name], Options) of
		 ok -> true;
		 ERLYDB_Error -> {error, ERLYDB_Error}
	     end
    end.

test() -> todo.
       
%%-spec(create_module/1 :: 
%%      (Name :: atom()) -> {module, Name :: atom()} | Error :: term()).
%% Name cannot contains dots.
create_module(Name) ->
    %% It is not working because code:which points to the fake file
    %%     Forms = [{attribute, 1, file, {atom_to_list(Name) ++ ".erl", 0}},
    %% 	     {attribute, 1, module, Name},{eof, 1}
    %% 	    ],
    %%     case compile:forms(Forms, [debug_info]) of
    %% 	{ok, Name, Binary} -> load(Name, Binary);
    %% 	{ok, Name, Binary, _Warnings} -> load(Name, Binary);
    %% 	Error -> Error
    %%     end.  
    %% FIXME
%%    Path = "local/" ++ atom_to_list(Name) ++ ".erl",
    Path = src_dir() ++ atom_to_list(Name) ++ ".erl",
    {ok, FDO} = file:open(Path, [write]),
    out(FDO, "-module(~s).~n", [Name]),
    file:close(FDO),
    {module, Name}.
    
out(FD, F) ->
    io:fwrite(FD, F, []).
out(FD, F, A) ->
    io:fwrite(FD, F, A).

%% load(Name, Binary) ->
%%     code:load_binary(Name, atom_to_list(Name), Binary).

init(Node) ->
    ?debug("About to init node ~p.",[Node],init),
    Nodes = [Node],
    %% TODO can be dangerous but it's ok because it must be invoked only on clean
    mnesia:stop(),
    ?assert(ok =:= mnesia:create_schema(Nodes)),
    ?assert(ok =:= erlydb:start(mnesia)),
    case mnesia:create_table(common_db, 
			     [
			      {attributes, 
			       record_info(fields, common_db)},
			      {disc_copies, Nodes}
			     ]) of
	{atomic,ok} -> 
	    case mnesia:create_table(counter, [{disc_copies, Nodes}, 
					       {attributes, [key,counter]}]) of
		{atomic,ok} -> 
		    init:init(Nodes);
		DB_Counter_Error ->
		    ?error("Cannot create counter table (for self),reason ~p.",
			   [DB_Counter_Error],init),
		    {error, {cannot_create_table, {counter, DB_Counter_Error}}}
	    end;
	DB_Error ->
	    ?error("Cannot create common_db table (for self),reason ~p.",
		   [DB_Error],init),
	    {error, {cannot_create_table, {common_db, DB_Error}}}
    end.

create_tables(Nodes, Modules) ->
    lists:foldl(
      fun(Table, Res) ->
	      case Table:create_tables(Nodes) of
		  ok -> [{Table, true}] ++ Res;
		  {atomic, ok} -> [{Table, true}] ++ Res;
		  Error -> [{Table, Error}] ++ Res
	      end
      end, [], Modules).


get_modules() -> get_modules_with_fun(create_tables, 1).

%all_keys(Table) ->
-type(reason() :: term()). 
-type(proplist() :: [atom() | {atom(), term()}]).

-spec(code_gen/3 :: (Backend_Type :: atom(), 
		     Tables :: [atom()], 
		     Options :: proplist()) -> true | {error, reason()}).
code_gen(Type, Tables, Options) ->
    case backend(Type) of
	{error, _Reason} = Error -> Error;
	BackEnd -> 
	    ?debug("About to ~p:code_gen(~p, ~p).",
		   [BackEnd, Tables, Options],code_gen),
	    BackEnd:code_gen(Tables, Options)
    end.

-spec(options/3 :: (BackEnd :: atom(), 
		    Table :: atom(), 
		    Options :: proplist()) -> proplist() | {error, reason()}).
options(BackEnd, Table, Options) ->
    Trans = fun() -> 
		    mnesia:read(common_db, {Table, BackEnd})
	    end,
    case mnesia:transaction(Trans) of
	{atomic, [Description]} -> 
	    ?debug("Table definition is ~p", [Description], options),
	    Description#common_db.options;
	_Any -> 
	    ?debug("Unexpected table definition ~p", [_Any], options),
	    []
    end.

options(Module, Options) ->
    Custom = Module:options(Options),
    ErlyDBOpts = proplists:append_values(erlydb_opts, Custom),
    Local = src_dir(),
    proplists:delete(erlydb_opts, Options) ++ 
	[{erlydb_opts, ErlyDBOpts ++ [debug, {src_dir, Local},
				      {skip_fk_checks, true}]}].

src_dir() -> 
    ?debug("local_dir: ~p",[node:local_dir()],src_dir),
    node:local_dir() ++ "/erlydb-0.7.2/src/erlydb/". %% FIXME   

test(_) -> todo.
    

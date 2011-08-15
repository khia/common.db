-module(common.db.mnesia).
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
-import(common.utils.packages).

-export([create_table/2, local_tables/0]).
-export([tables/1, options/1, code_gen/2]).
-export([test/1]).

%%TODO
%%delete_table(Name, Opts)
%%clear_table(Name, Opts) 
create_table(Name, Opts) ->
    Options = proplists:delete(erlydb_opts, Opts),
    ?debug("About to create_table(~p, ~p).", [Name, Options], create_table),
    mnesia:create_table(Name, Options).
    
local_tables() -> mnesia:system_info(local_tables).

-type(reason() :: term()). 
-type(proplist() :: [atom() | {atom(), term()}]).

-spec(code_gen/2 :: (
		     Tables :: [atom()], 
		     Options :: proplist()) -> true | {error, reason()}).
code_gen(Tables, Options) ->
    case erlydb:code_gen(Tables, mnesia, Options) of
	{error, _Reason} = Error ->
	    ?debug("ERLYDB_Error = ~p.",[Error],code_gen),
	    Error;
	OK -> OK
    end.

tables(_Options) -> local_tables().
    
options(Options) -> 
%%    ?debug("Request options ~p.",[Options], options),
    Opts = [],
    [{erlydb_opts, Opts}] ++ Options.

test(_) -> todo.

%% TODO
encode(field_type, Type)      -> {error, {unsupported_type, Type}}.
decode(field_type, Type)                -> {error, {unsupported_type, Type}}.  

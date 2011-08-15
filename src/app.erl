-module(common.db.app).
-include("debug.hrl").
-behaviour(application). 

-import(erlydb).
-import(proplists).
-import(erlang).
-import(init).
-import(application).
-import(common.node.node).
-import(common.utils.string).


-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

-spec(start/2 :: (
	      Type :: normal | {takeover, node()} | {failover, node()}, 
	      Args :: term()) -> 
	     {ok, Pid :: pid()} 
		 | {ok, Pid :: pid(), State :: term()} 
		 | {error, Reason :: term()}).
start(_Type, Args) -> 
    ?debug("About to start application with arguments: ~p",[Args],start),
%%    Drivers = proplists:get_value(drivers, Args, []),
%%    application:set_env(common.db, drivers, Drivers);
%%    application:set_env(mnesia,dir,Dir),
    case node:role() of
	boot_client -> 
	    application:set_env(mnesia, extra_db_nodes, [node:boot_server()]);
	_ -> ok
    end,
%%     ?assert([] =/= Drivers),
%%     Res =  [{Driver, erlydb:start(Driver, scan(Opts))} 
%% 	    || {Driver, Opts} <- Drivers],
%%     ?debug("erlydb was started, ~p", [Res], start),
     case db:start(Args) of
 	{atomic, ok} -> 
	     ?debug("Application common.db was successfully started", [], start),
	     {ok, self()};
 	Error -> 
	     ?debug("Cannot start db, reason ~p.", [Error], start),
	     Error
%%     end,
%%    {ok, self()}.
     end.

%% %%    process_args(),
%%     case sup:start_link(Args) of
%% 	{ok, Pid} -> 
%% 	    ?debug("Application ~p was successfully started.", [?MODULE], start),
%% 	    {ok, Pid};
%% 	Error -> 
%% 	    ?debug("Cannot start application, reason ~p.", [Error], start),
%% 	    Error
%%     end.

-type(ignored() :: term()).
-spec(stop/1 :: (State :: term()) -> ignored()). 
stop(_State) -> 
    ok.

scan(Opts0) ->
    ?debug("Opts0 = ~p",[Opts0],scan),
    Opts1 = scan(debug, Opts0),
    scan(connect, Opts1).

scan(connect, Opts) ->
    case proplists:get_value(connection_fun, Opts) of
	undefined -> Opts;
	Fun ->
	    ?debug("~p",[Fun],scan),
	    case string:to_ref(Fun) of
		{error, Reason} ->
		    ?error("Cannot parse function reference ~p, reason ~p.",
			   [Fun, Reason], scan),
		    exit(1);
		Ref ->
		    [{connection_fun, string:to_ref(Fun)}] ++ 
			proplists:delete(connection_fun, Opts)
	    end
    end;

scan(debug, Opts) ->
%%    Debug = proplists:get_value(debug, Opts, error),
    Filtered = proplists:delete(debug, Opts),
%%     case Debug of
%% 	none -> {{logfun, fun log/4}}
    Filtered.%% ++ [{logfun, fun log/4}].
    
%% log(Module, Line, Level, FormatFun) ->
%%     case Level of
%% 	debug ->
%% 	    ok;
%% 	_ ->
%% 	    Func = case Level of
%% 		       info ->
%% 			   info_msg;
%% 		       normal ->
%% 			   info_msg;
%% 		       error ->
%% 			   error_msg;
%% 		       warn ->
%% 			   warning_msg
%% 		   end,
%% 	    {Format, Params} = FormatFun(),
%% 	    error_logger:Func("~w:~b: "++ Format,
%% 			      [Module, Line | Params])
%%     end.



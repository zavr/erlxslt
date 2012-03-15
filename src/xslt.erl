-module(xslt).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

%% ----------------------------------------------------------------------------
%% Defines
%% ----------------------------------------------------------------------------

-record(state,
    {   connections,
        xsl_pool,
        tasks,
        reconnection_time,
        necessary,
        xslt_adapter_path
    }).

%% ----------------------------------------------------------------------------
%% External exports
%% ----------------------------------------------------------------------------
-export([start/0, start_link/0, start_link/1, apply/2, test/0]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start()->
    start_link().

start_link()->
    start_link(?LIBXSLT_FULL_ADAPTER_PATH).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Xslt_adapter_path)->
    gen_server:start_link({local, ?MODULE},
        ?MODULE, [Xslt_adapter_path], [{spawn_opt,[{min_heap_size,200000}]}]).


apply(Xsl_url, Xml) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {con_request, self()}),
            receive
                {xslconnection, Con} ->
                    try
                        case xslt_adapter:apply_xsl2(Con, Xsl_url, Xml) of
                            {ok, HTML} -> 
                                gen_server:cast(Pid, {con_free, Con}),
                                HTML;
                            Other ->
                                ?ERROR(?FMT("apply_xsl error: ~p~n", [Other])),
                                gen_server:cast(Pid, {con_error, Con}),
                                ""
                        end
                    catch
                        E:R ->
                            gen_server:cast(Pid, {con_error, Con}), 
                            {error, {E, R}}
                    end
            end;
        _ ->
            {error, {not_started}}
    end.


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Xslt_adapter_path]) ->
    ?INFO(?FMT("XSL PROC POOL STARTING...~n~n~n", [])),
    process_flag(trap_exit, true),
    {ok, #state{
        connections=[],
        xsl_pool = [],
        necessary=?XSLT_NECESSARY,
        reconnection_time=get_now(),
        tasks=[],
        xslt_adapter_path=Xslt_adapter_path
    }, 0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(Request, _From, State) ->
    ?ERROR(?FMT("~p:~p unexpected call: ~p~n", [?MODULE, ?LINE, Request])),
    ?INFO(?FMT("~p ~p~n", [?MODULE,{unexpected_call,Request}])),
    {NState, Timeout} = check_reconnection(State),
    Reply = {error, unexpected_call}, 
    {reply, Reply, NState, Timeout}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({con_request, Pid}, State=#state{connections=Cons,
        tasks=Tasks, xsl_pool=_CPool, necessary=_N}) ->
    {RestTasks, Rest_cons} = exec_tasks(Tasks++[Pid], Cons),
    {NState, Timeout} = check_reconnection(
        State#state{connections=Rest_cons, tasks=RestTasks}),
    {noreply, NState, Timeout};

handle_cast({con_error, Con}, State=#state{
        tasks=_Tasks, connections=Cons, necessary=N, xsl_pool=CPool}) ->
    case lists:member(Con, CPool) of
        true ->
            ?INFO(?FMT("so.. reconnect ~p~n",[Con])),
            NewNecessary = N+1;
        false ->
            ?INFO(?FMT("illegal con ~p in ~p~n",[Con, CPool])),
            NewNecessary = N
    end,

    StateStage1 = State#state{necessary=NewNecessary, xsl_pool=CPool--[Con],
        connections=Cons--[Con]}, % 100% no Con in pools
    {NState, Timeout} = check_reconnection(StateStage1),
    {noreply, NState, Timeout};

handle_cast({con_free, Con}, State=#state{tasks=Tasks,
        connections=Cons, necessary=_N, xsl_pool=_CPool}) ->
    {RestTasks, Rest_cons} = exec_tasks(Tasks, [Con|Cons]),
    {NState, Timeout} =
        check_reconnection(State#state{connections=Rest_cons, tasks=RestTasks}),
    {noreply, NState, Timeout};

handle_cast(Msg, State) ->
    ?ERROR(?FMT("~p:~p unexpected cast: ~p~n", [?MODULE, ?LINE, Msg])),
    {NState, Timeout} = check_reconnection(State),
    {noreply, NState, Timeout}.


%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State=#state{tasks=Tasks, necessary=N}) ->
    ?INFO(?FMT("XSL RECONNECT TIMER ~p ... tasks: ~p~n", [N, length(Tasks)])),
    {NState, Timeout} = check_reconnection(State),
    {noreply, NState, Timeout};

handle_info(Info, State) ->
    ?INFO(?FMT("~p ~p~n", [?MODULE, {unexpected_info, Info}])),
    {NState, Timeout} = check_reconnection(State),
    {noreply, NState, Timeout}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    ?INFO(?FMT("~p:~p terminated, reason: ~p~n", [?MODULE, ?LINE, Reason])),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------

init_xsl_proc_pool(0, Ret, _Xslt_adapter_path) ->
    Ret;
init_xsl_proc_pool(N, Ret, Xslt_adapter_path) ->
    case catch xslt_adapter:start(Xslt_adapter_path) of
        {ok, Xsl_new} ->
            ?INFO(?FMT("new xsl processor ~p~n",[Xsl_new])),
            init_xsl_proc_pool(N-1, [Xsl_new|Ret], Xslt_adapter_path);
        Error ->
            ?INFO(?FMT("init xsl processor error: ~p~n", [Error])),
            Ret
    end.

init_xsl_proc_pool(N, Xslt_adapter_path) ->
    init_xsl_proc_pool(N, [], Xslt_adapter_path).


exec_tasks(Tasks, Cons=[Con|RC]) ->
    {Pid, Tasks2} = get_next_task(Tasks),
    if
        Pid =:= none ->
            {[], Cons};
        true ->
            Pid ! {xslconnection, Con},
            exec_tasks(Tasks2, RC)
    end;
exec_tasks(RT, RC) ->
    {RT, RC}.
check_reconnection(State=#state{necessary=0}) ->
    {State#state{reconnection_time=infinity}, infinity};
check_reconnection(State=#state{reconnection_time=infinity}) ->
    {State#state{reconnection_time=get_now() + ?XSLT_RECONNECT_TIMEOUT*1000},
        ?XSLT_RECONNECT_TIMEOUT};

check_reconnection(State=#state{reconnection_time=RT, necessary=N,
        connections=Cons, xsl_pool=CPool, tasks=Tasks,
            xslt_adapter_path=Xslt_adapter_path}) ->
    Now = get_now(),
    if  
        Now < RT ->
            NState = State,
            Timeout = trunc((RT - Now)/1000);
        true -> 
            NewCons = init_xsl_proc_pool(N, Xslt_adapter_path),
            {RestTasks, Rest_cons} = exec_tasks(Tasks, Cons++NewCons),
            ?INFO(?FMT( "XSL adapter reconnecting: necessary "
                        "- ~p, new - ~p, rest: ~p~n",
                [N, length(NewCons), length(Rest_cons)])),
            NState = State#state{
                necessary=N-length(NewCons),
                reconnection_time=infinity,
                connections=Rest_cons,
                tasks=RestTasks,
                xsl_pool=CPool++NewCons},
            Timeout = infinity
    end,
    {NState, Timeout}.

get_next_task([Pid|T]) ->
    case lists:member(Pid, processes()) of
        true ->
            {Pid, T};
        false ->
            ?INFO(?FMT("XSL DROP INVALID PID ~p~n", [Pid])),
            get_next_task(T)
    end;
get_next_task([]) ->
    {none, []}.

get_now() ->
    {_M,S,Mi} = now(),
    S*?XSLT_TIME_OFFSET+Mi.


-include_lib("eunit/include/eunit.hrl").

test() ->
    Xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<catalog>"
    "<cd>"
        "<title>Empire Burlesque</title>"
        "<artist>Bob Dylan</artist>"
        "<country>USA</country>"
        "<company>Columbia</company>"
        "<price>10.90</price>"
        "<year>1985</year>"
        "</cd>"
    "</catalog>",
    Etalon = <<"<?xml version=\"1.0\"?>\n<html><body><h2>My CD Collection</h2><table border=\"1\"><tr bgcolor=\"#9acd32\"><th>Title</th><th>Artist</th></tr><tr><td>Empire Burlesque</td><td>Bob Dylan</td></tr></table></body></html>\n">>,

    start(),
    Result = ?MODULE:apply(string:join([?LIBXSLT_ROOT_PATH,
        "priv/example/test.xsl"], "/"), Xml),

    ?assertEqual(Etalon, Result),

    ok.




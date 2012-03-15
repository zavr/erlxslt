-module(xslt_adapter).
-behaviour(gen_server).

-include("../include/common.hrl").

-define(VERSION_TOKEN,   "v").
-define(APPLY_XSL_TOKEN, "a").

%%%%%%%%%%%%%%%%%%

-export([start/1, start_link/1]).

%%%%%%%%%%%%%%%%%%

-export([stop/1, version/1, info/1, apply_xsl2/3]).

%%%%%%%%%%%%%%%%%%

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%

start(Xslt_adapter_path) ->
    gen_server:start(?MODULE, Xslt_adapter_path, []).

start_link(Xslt_adapter_path) ->
    gen_server:start_link(?MODULE, Xslt_adapter_path, []).

init(Xslt_adapter_path) ->
    {ok, open_port({spawn, Xslt_adapter_path},
        [binary, use_stdio, {packet,4}])}.

terminate(_Reason, Port) ->
    ?ERROR("TERMINATE!! ~n"),
    port_close(Port),
    ok.

%%%%%%%%%%%%%%%

stop(Pid) ->
    gen_server:call(Pid, die, ?XSLT_SERVICE_TIMEOUT).

version(Pid) ->
    gen_server:call(Pid, version, ?XSLT_SERVICE_TIMEOUT).

info(Pid) ->
    gen_server:call(Pid, info, ?XSLT_SERVICE_TIMEOUT).

apply_xsl2(Pid, Xsl_file_name, Xml_str) ->
    gen_server:call(Pid, {apply_xsl2, Xsl_file_name, Xml_str}, ?XSLT_TIMEOUT).


%%%%%%%%%%%%%%%%

handle_call(die, _From, State) ->
    ?INFO("STOP!! ~n"),
    {stop, normal, ok, State};

handle_call(version, _From, Port) ->
    xslt_port_util:port_command(Port, list_to_binary(?VERSION_TOKEN)),
    {reply, xslt_port_util:cond_result_list(Port, ?PORT_TIMEOUT), Port};

handle_call({apply_xsl2, Xsl_file_name, Xml_str}, _From, Port) ->
    xslt_port_util:port_commands(Port, [list_to_binary(?APPLY_XSL_TOKEN),
        Xsl_file_name, Xml_str]),
    Z = xslt_port_util:cond_result(Port, ?PORT_TIMEOUT),
    {reply, Z, Port};


handle_call(info, _From, State) ->
    {reply, {ok, State}, State}.

%%%%%%%%%%%%%%%%

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(_Request, State) -> 
    {noreply, State}.

code_change(_,Port,_) -> 
    {ok,Port}.
    

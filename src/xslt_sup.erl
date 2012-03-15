-module(xslt_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("../include/common.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Logging = {
        xml, {xml, start_link, []},
        permanent, ?XML_SUP_TIMEOUT, worker,
        [xml]
    },

    Xslt_processor = {
        xslt, {xslt, start_link, []},
        permanent, ?XSLT_SUP_TIMEOUT, worker,
        [xslt]
    },


    Processes = [
        Logging,
        Xslt_processor
    ],

    {ok, {{one_for_one, 10, 10}, Processes}}.

% -----------------------------------------------------------------------------
%% common defines
% -----------------------------------------------------------------------------

-define( FMT(F,P), lists:flatten(io_lib:format(F,P)) ).

%%
%% if has flog.erl and clog.erl
%%

% -define( INFO(P),  flog:info(P) ).
% -define( ERROR(P), flog:error(P) ).
% -define( DEBUG(P), flog:debug(P) ).

%%
%% else
%%

-define( INFO(P),  io:format("~pINFO:  ~s", [self(), P])).
-define( ERROR(P), io:format("~pERROR: ~s", [self(), P])).
-define( DEBUG(P), io:format("~pDEBUG: ~s", [self(), P])).

% -----------------------------------------------------------------------------
%% timeout defines
% -----------------------------------------------------------------------------

-define(XML_SUP_TIMEOUT,            10000).
-define(XML_TIMEOUT,                10000).

-define(XSLT_NECESSARY,             10).
-define(XSLT_TIME_OFFSET,           1000000).
-define(XSLT_SUP_TIMEOUT,           10000).
-define(XSLT_TIMEOUT,               10000).
-define(XSLT_RECONNECT_TIMEOUT,     5000).
-define(XSLT_SERVICE_TIMEOUT,       5000).
-define(PORT_TIMEOUT,               10000).

% -----------------------------------------------------------------------------
%% path defines
% -----------------------------------------------------------------------------

-define(LIBXSLT_ROOT_PATH,
    filename:dirname(filename:dirname(code:which(?MODULE)))).
-define(LIBXSLT_ADAPTER_PATH, "priv/cbin/libxslt_adapter").
-define(LIBXSLT_FULL_ADAPTER_PATH,
    string:join([?LIBXSLT_ROOT_PATH, ?LIBXSLT_ADAPTER_PATH], "/")).

% -----------------------------------------------------------------------------
%% xml defines
% -----------------------------------------------------------------------------

-define(XML_TOP, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-define(ITEM_B, "<item>").
-define(ITEM_E, "</item>").
-define(OPEN_B, "<").
-define(OPEN_E, ">").
-define(CLOSE_B, "</").
-define(CLOSE_E, ">").

-define(DELIM, []).
-define(SPACE, " ").

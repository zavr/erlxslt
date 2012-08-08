%% Feel free to use, reuse and abuse the code in this file.

-module(xslt_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(xslt).

start(_Type, _Args) ->
    xslt_sup:start_link().

stop(_State) ->
    ok.

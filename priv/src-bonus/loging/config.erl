%%-----------------------------------------------------------------------------
%% Created: 17.03.2009
%% Description: Config store
%%-----------------------------------------------------------------------------
-module(config).

-define(STORE, app_cfg).

-export([init/0, deinit/0,
         put/2, get/2, store/1,
         rewrite/3]).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
init() ->
    ?STORE = utils:make_ets(?STORE),
    ok.

put(Key,Val) ->
    true = ets:insert(?STORE, {Key,Val}),
    ok.

rewrite(Key,SubKey,Val) ->
    case config:get(Key, undefined) of
  L when is_list(L) ->
      L2 = proplists:delete(SubKey,L),
      config:put(Key, [{SubKey,Val}|L2]);
  _ -> config:put(Key,[{SubKey,Val}])   %% force
    end.

store(Args) when is_list(Args) ->
    true = ets:insert(?STORE, Args).

get(Key, Default) ->
    case catch(ets:lookup(?STORE, Key)) of
  {'EXIT',_} -> Default;
  [{Key,Val}] -> Val;
  _  -> Default
    end.


deinit() ->
    utils:drop_ets(?STORE),
    ok.

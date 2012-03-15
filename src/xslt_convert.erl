%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% ФУНКЦИИ КОНВЕРТАЦИИ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(xslt_convert).
-export([
            to_plain/1,
            from_plain/1,
            to_integer/1,
            to_atom/1,
            to_list/1,
            to_float/1,
            string_to_term/1,
            to_binary/1,
            test/0
        ]
).

% ---------------------------------------------------------------------------

to_plain(Val) ->
    lists:flatten(io_lib:format("~p", [Val])).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%%   to_string != to_plain (см ниже [test()] почему ),
%%      выбирайте иные пути, или используйте sformat.
%% 
%% 1> io_lib:format("foo ~p bar~n", [42]).
%% [102,111,111,32,"42",32,98,97,114,"\n"]
%% 2> erlang:iolist_to_binary(v(1)).
%% <<"foo 42 bar\n">>
%% 3> lists:flatten(v(1)).
%% "foo 42 bar\n"
%% 4> lists:concat([foo," ", 42, " ", bar, "\n"]).
%% "foo 42 bar\n"
%%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% ---------------------------------------------------------------------------


to_integer(Val) when erlang:is_integer(Val) -> Val;

to_integer(Val) when erlang:is_float(Val) -> trunc(Val);

to_integer(Val) when erlang:is_atom(Val) -> Val;

to_integer(Val) when erlang:is_list(Val) ->
    case catch(erlang:list_to_integer(Val)) of
        {'EXIT', _} -> erlang:throw({error, {to_integer, bad_arg, Val}});
        Int -> Int
    end;

to_integer(Val) when erlang:is_binary(Val) ->
    case catch(erlang:list_to_integer(erlang:binary_to_list(Val))) of
        {'EXIT', _} -> erlang:throw({error, {to_integer, bad_arg, Val}});
        Int -> Int
    end.


% ---------------------------------------------------------------------------

to_atom(Val) when erlang:is_atom(Val) -> Val;

to_atom(Val) when erlang:is_integer(Val) ->
    to_atom(erlang:integer_to_list(Val));

to_atom(Val) when erlang:is_list(Val) ->
    case catch(erlang:list_to_existing_atom(Val)) of
        {'EXIT', _} -> erlang:list_to_atom(Val);
        Atom -> Atom
    end;

to_atom(Val) when erlang:is_binary(Val) ->
    erlang:to_atom(erlang:binary_to_list(Val)).

% ---------------------------------------------------------------------------

to_list(Val) when is_list(Val) -> Val;
to_list(Val) when is_atom(Val) ->
    atom_to_list(Val);
to_list(Val) when is_integer(Val) ->
    integer_to_list(Val);
to_list(Val) when is_float(Val) ->
    float_to_list(Val);
to_list(Val) when is_binary(Val) ->
    binary_to_list(Val).

% ---------------------------------------------------------------------------

to_float(Val) when is_list(Val) ->
    case catch(erlang:list_to_float(Val)) of
        {'EXIT', _} ->
            case catch(erlang:list_to_integer(Val)) of
                {'EXIT', _} -> throw({error, {to_float, bad_arg, Val}});
                Int -> Int
            end;
        Float -> Float
    end;

to_float(Val) when is_float(Val); is_integer(Val) -> Val.


%% ============================================================================
%%
%% eredis: 'knut.nesheim@wooga.com'
%%

-define(NL, "\r\n").

to_binary(X) when is_binary(X)  ->
    X;
to_binary(X) when is_list(X)    ->
    erlang:list_to_binary(X);
to_binary(X) when is_atom(X)    ->
    erlang:list_to_binary(erlang:atom_to_list(X));
to_binary(X) when is_integer(X) ->
    erlang:list_to_binary(erlang:integer_to_list(X));
to_binary(X) when is_float(X)   ->
    erlang:list_to_binary(erlang:float_to_list(X));
to_binary(X)                    ->
    erlang:term_to_binary(X).

eredis_to_bulk(B) when is_binary(B) ->
    [<<$$>>, integer_to_list(iolist_size(B)), <<?NL>>, B, <<?NL>>].

eredis_to_multibulk(Args) when is_list(Args) ->
    ArgCount = [<<$*>>, integer_to_list(length(Args)), <<?NL>>],
    ArgsBin = lists:map(fun eredis_to_bulk/1,
        lists:map(fun to_binary/1, Args)),
    [ArgCount, ArgsBin].

%% ============================================================================

% ---------------------------------------------------------------------------

string_to_term(String) ->
    {ok, T, _} = erl_scan:string(lists:concat([String, "."])),
    case erl_parse:parse_term(T) of
        {ok, Term}      -> Term;
        {error, Error}  -> Error
    end.

from_plain(String) -> string_to_term(String).


% ===========================================================================
-include_lib("eunit/include/eunit.hrl").
test()->

    % TO_PLAIN 
    % ----------------------------------
    ?assertEqual("1",               to_plain(1)),
    ?assertEqual("1.0",             to_plain(1.0)),
    ?assertEqual("atom",            to_plain(atom)),
    % !!! "'atom atom'" != "atom atom"
    ?assertEqual("'atom atom'",     to_plain('atom atom')),
    ?assertEqual("\"1\"",           to_plain("1")),
    ?assertEqual("[1]",             to_plain([1])),
    ?assertEqual("[1,2]",           to_plain([1, 2])),
    ?assertEqual("<<\"binary\">>",  to_plain(<<"binary">>)),

    % assertEqual_failed
    % ?assertEqual("мама мыла раму", to_plain("мама мыла раму")),

    % assertEqual_failed
    % ?assertEqual("мама мыла раму", to_plain("мама мыла раму")),

    % assertEqual_failed 
    % ?assertEqual([1084,1072,1084,1072],
    %    lists:flatten(io_lib:format("~ts", ["мама"]))),

    % assertEqual_failed
    % ?assertEqual("Ð¼Ð°Ð¼Ð°", lists:flatten(io_lib:format("~ts", ["мама"]))),

    % assertEqual_failed
    % ?assertEqual("[1084,1072,1084,1072,32, \
    %    1084,1099,1083,1072,32,1088,1072,1084,1091]",
    %       to_plain("мама мыла раму")),

    % TO_INTEGER 
    % ----------------------------------
    ?assertEqual(1, to_integer(1)),
    ?assertEqual(1, to_integer("1")),
    ?assertEqual(1, to_integer(1.0)),
    ?assertEqual(1, to_integer(<<"1">>)),

    % TO_ATOM 
    % ----------------------------------
    ?assertEqual(atom,          to_atom(atom)),
    ?assertEqual('atom ',       to_atom('atom ')),
    ?assertEqual(atom,          to_atom("atom")),
    ?assertEqual('atom atom',   to_atom("atom atom")),

    % TO_LIST
    % ----------------------------------
    ?assertEqual("binary", to_list(<<"binary">>)),

    % TO_FLOAT
    % ----------------------------------
    % ...

    % EREDIS_TO_BINARY
    % ----------------------------------
    ?assertEqual(<<"atom atom">>,       to_binary('atom atom')),
    ?assertEqual(<<"atom">>,            to_binary(atom)),
    % assertEqual_failed 
    % ?assertEqual(<<"atom">>,          to_binary([atom])),
    ?assertEqual(<<"string">>,          to_binary("string")),
    ?assertEqual(<<1,2,3,4>>,           to_binary([1, 2, 3, 4])),
    % !!! wrong idea
    % ?assertEqual(<<1,2,3,4>>,         to_binary({1, 2, 3, 4})),
    ?assertEqual(<<"1111">>,
        to_binary([["1", "1"], ["1", "1"]])),
    ?assertEqual(<<"abbcxyyz">>,
        to_binary([["ab", "bc"], ["xy", "yz"]])),
    ?assertEqual(<<98,99,1,3,12>>,
        to_binary(["bc", 1, 3, [12]])),

    % EREDIS_TO_BULK
    % ----------------------------------
    ?assertEqual([<<"$">>,"1",<<"\r\n">>,<<1>>,<<"\r\n">>],
        eredis_to_bulk(<<1>>)),
    ?assertEqual([<<"$">>,"2",<<"\r\n">>,<<1,1>>,<<"\r\n">>],
        eredis_to_bulk(<<1,1>>)),
    ?assertEqual([<<"$">>,"4",<<"\r\n">>,<<"some">>,<<"\r\n">>],
        eredis_to_bulk(<<"some">>)),

    % EREDIS_TO_MULTIBULK
    % ----------------------------------
    ?assertEqual([[<<"*">>,"1",<<"\r\n">>],
        [[<<"$">>,"4",<<"\r\n">>,<<"atom">>,<<"\r\n">>]]],
        eredis_to_multibulk([atom])),

    % STRING_TO_TERM 
    % ----------------------------------
    ?assertEqual([1, 2],                    string_to_term("[1, 2]")),
    ?assertEqual([a, 2],                    string_to_term("[a, 2]")),
    ?assertEqual(['a ', 2],                 string_to_term("['a ', 2]")),
    ?assertEqual(["a", 2],                  string_to_term("[\"a\", 2]")),
    ?assertEqual({["a", "b"], ["x", "y"]},
        string_to_term("{[\"a\", \"b\"], [\"x\", \"y\"]}")),
    ?assertEqual({["ф", "и"], ["ч", "ы"]},
        string_to_term("{[\"ф\", \"и\"], [\"ч\", \"ы\"]}")),
    ?assertEqual("мама мыла раму",
        string_to_term("\"мама мыла раму\"")),

    % STRING_TO_TERM as FROM_PLAIN
    % ----------------------------------
    ?assertEqual(atom,              from_plain(to_plain(atom))),
    ?assertEqual('atom atom',       from_plain(to_plain('atom atom'))),
    ?assertEqual([1, 2],            from_plain(to_plain([1, 2]))),
    ?assertEqual({1, 2},            from_plain(to_plain({1, 2}))),
    ?assertEqual(<<"binary">>,      from_plain(to_plain(<<"binary">>))),
    ?assertEqual("мама мыла раму",  from_plain(to_plain("мама мыла раму"))),

    %%
    %% Приведено для сравнения.
    %% При прочих равных условиях лучше пользоваться стандартными функциями.
    %%
    ?assertEqual({1, 2},
        erlang:binary_to_term(erlang:term_to_binary({1, 2}))),
    ?assertEqual("мама мыла раму",
        erlang:binary_to_term(erlang:term_to_binary("мама мыла раму"))),
    ok.

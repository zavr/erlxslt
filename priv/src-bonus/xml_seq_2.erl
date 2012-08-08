-module(xml_seq_2).
-export([encode/1, encode_set/1, encode_data/1, test/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

encode_set(Proplist) ->
    string:join([?XML_TOP, encode({"set", Proplist})], ?DELIM).

encode_data(Proplist) ->
    string:join([?XML_TOP, encode({"data", Proplist})], ?DELIM).

encode({Tag, Proplist})
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, convert:to_list(Tag), ?OPEN_E,
            encode(Proplist),
        ?CLOSE_B, convert:to_list(Tag), ?CLOSE_E,
        ?DELIM
    ]);

encode([{Tag, Value} | Rest ])
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, convert:to_list(Tag), ?OPEN_E,
            encode(Value),
        ?CLOSE_B, convert:to_list(Tag), ?CLOSE_E,
        ?DELIM,
            encode(Rest),
        ?DELIM
    ]);

encode([ Item | _ ] = Proplist)
        when erlang:is_integer(Item) ->
    Proplist;

encode([ Item | Rest ])  ->
    lists:append([
        ?ITEM_B,
            encode(Item),
        ?ITEM_E,
        ?DELIM,
            encode(Rest),
        ?DELIM
    ]);

encode(Value) when is_tuple(Value) ->
    Result = string:join([[encode(Vi)] || Vi <- tuple_to_list(Value)], ?SPACE),
    convert:to_list(Result);

encode(Value) -> convert:to_list(Value).

-include_lib("eunit/include/eunit.hrl").

test() ->
    ?assertEqual(
        "<data>"
            "some data"
        "</data>",
        encode({data, "some data"})),

    ?assertEqual(
        "<data>"
            "<more>1</more>"
        "</data>",
        encode({data,{more, 1}})),

    ?assertEqual(
        "<data>"
            "<item>a</item>"
            "<item>b</item>"
            "<item>c</item>"
        "</data>",
        encode({data, [a, b, c]})),

    ?assertEqual(
        "<key_a>value_a</key_a>"
        "<key_b>value_b</key_b>",
        encode([
            {key_a, value_a},
            {key_b, value_b}
        ])),

    ?assertEqual(
        "<key_a>value_a</key_a>"
        "<key_b>value_b</key_b>"
        "<item>c</item>",
        encode([
            {key_a, value_a},
            {key_b, value_b},
            c
        ])),

    ?assertEqual(
        "<key_a>"
            "<item>a_1</item>"
            "<item>a_2</item>"
        "</key_a>"
        "<key_b>"
            "<k-b1>v-b1</k-b1>"
            "<k-b2>v-b2</k-b2>"
        "</key_b>"
        "<key_c>"
            "<key_d>"
                "<key e>"
                    "value e"
                "</key e>"
                "<key f>"
                    "value f"
                "</key f>"
            "</key_d>"
        "</key_c>",
        encode([
            {key_a, [a_1, a_2]},
            {key_b, [{'k-b1', 'v-b1'}, {'k-b2', 'v-b2'}]},
            {"key_c", {"key_d", [{"key e", "value e"}, {"key f", "value f"}]}}
        ])),

    ok.

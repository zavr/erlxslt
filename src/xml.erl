-module(xml).
-behaviour(gen_server).

-include("../include/common.hrl").

-export([start/0, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

-export([encode/1, encode_set/1, encode_data/1, test/0]).

-export([encode_seq/1, encode_set_seq/1, encode_data_seq/1]).


start()->
    start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
     {ok, []}.

terminate(_, _) ->
     ok.

handle_cast(start, _Data) ->
     {noreply, []};

handle_cast(stop, Data) ->
     {stop, normal, Data}.

handle_call({encode, Data}, _From, State) ->
    {reply, encode_seq(Data), State};

handle_call({encode_data, Data}, _From, State) ->
    {reply, encode_data_seq(Data), State};

handle_call({encode_set, Data}, _From, State) ->
    {reply, encode_set_seq(Data), State};

handle_call(_, _From, _Data) ->
     {reply, [], []}.

handle_info(Info, Data) ->
     error_logger:info_report([{module, ?MODULE},
                              {line, ?LINE},
                              {self, self()},
                              {message, Info}]),
     {noreply, Data}.

code_change(_, Data, _) ->
     {ok, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode(Data) ->
    gen_server:call(whereis(?MODULE), {encode, Data}, ?XML_TIMEOUT).

encode_set(Data) ->
    gen_server:call(whereis(?MODULE), {encode_set, Data}, ?XML_TIMEOUT).

encode_data(Data) ->
    gen_server:call(whereis(?MODULE), {encode_data, Data}, ?XML_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_set_seq(Proplist) ->
    string:join([?XML_TOP, encode_seq({"set", Proplist})], ?DELIM).

encode_data_seq(Proplist) ->
    string:join([?XML_TOP, encode_seq({"data", Proplist})], ?DELIM).

encode_seq({Tag, Proplist})
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, xslt_convert:to_list(Tag), ?OPEN_E,
            encode_seq(Proplist),
        ?CLOSE_B, xslt_convert:to_list(Tag), ?CLOSE_E,
        ?DELIM
    ]);

encode_seq([{Tag, Value} | Rest ])
        when erlang:is_list(Tag) or erlang:is_atom(Tag) ->
    lists:append([
        ?OPEN_B, xslt_convert:to_list(Tag), ?OPEN_E,
            encode_seq(Value),
        ?CLOSE_B, xslt_convert:to_list(Tag), ?CLOSE_E,
        ?DELIM,
            encode_seq(Rest),
        ?DELIM
    ]);

encode_seq([ Item | _ ] = Proplist)
        when erlang:is_integer(Item) ->
    Proplist;

encode_seq([ Item | Rest ])  ->
    lists:append([
        ?ITEM_B,
            encode_seq(Item),
        ?ITEM_E,
        ?DELIM,
            encode_seq(Rest),
        ?DELIM
    ]);

encode_seq(Value) when is_tuple(Value) ->
    Result = string:join([[encode_seq(Vi)] || Vi <- tuple_to_list(Value)], ?SPACE),
    xslt_convert:to_list(Result);

encode_seq(Value) -> xslt_convert:to_list(Value).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->

    start(),

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

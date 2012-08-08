-module(xml_seq_1).
-export([encode/1, encode/2, encodeSet/1, encodeData/1]).

-define(XML_TOP, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

-define(ITEM_B, "<item>").
-define(ITEM_E, "</item>").

-define(SET_B, "<set>").
-define(SET_E, "</set>").

-define(DATA_B, "<data>").
-define(DATA_E, "</data>").

-define(DELIM, "\n").


% <?xml version="1.0" encoding="ISO-8859-1"?>
%   <Name>Val</Name>
%   ................
encode([R]) ->
    string:join([?XML_TOP | nameFields(R)], ?DELIM).

%%
%% Создает множественный XML.
%%

% <?xml version="1.0" encoding="ISO-8859-1"?>
% <set>
%   <JsonObjName1>
%        ...
%   </JsonObjName1>
%   <JsonObjName2>
%       ...    
%   </JsonObjName2>
% </set>

encodeSet(List) ->
    string:join([?XML_TOP, ?SET_B, encodeSetTopless(List), ?SET_E], ?DELIM).

% <?xml version="1.0" encoding="ISO-8859-1"?>
% <data>
%   <JsonObjName1>
%        ...
%   </JsonObjName1>
%   <JsonObjName2>
%       ...    
%   </JsonObjName2>
% </data>

encodeData(List) ->
    string:join([?XML_TOP, ?DATA_B, encodeSetTopless(List), ?DATA_E], ?DELIM).
    
%%
%% Создает множественный XML без XML_TOP.
%%
encodeSetTopless([{list, Rows, JsonObjName}| Rest]) ->
    S = encodeTopless(Rows, JsonObjName),
    string:join([S, encodeSetTopless(Rest)], ?DELIM);
    
encodeSetTopless([{one, Rows, JsonObjName}| Rest]) ->
    S = encodeToplessOne(Rows, JsonObjName),
    string:join([S, encodeSetTopless(Rest)], ?DELIM);
    
encodeSetTopless([]) -> " ".

%%
%% Создает одиночный XML.
%%

% <?xml version="1.0" encoding="ISO-8859-1"?>
% <JsonObjName>
%   <item>
%       <Name>Val</Name>
%       ................
%   </item>
% </JsonObjName>

encode(Rows, JsonObjName) when is_list(Rows)->
    Res = lists:append([lists:append([[?ITEM_B], nameFields(R), [?ITEM_E]]) || R <- Rows]),
    mountXml(Res, JsonObjName);
        
encode([R], JsonObjName) ->
    mountXml(nameFields(R), JsonObjName);

encode(Row, JsonObjName) ->
    mountXml(toList(Row), JsonObjName).

%%
%% Создает одиночный XML без XML_TOP.
%%         
encodeTopless(Rows, JsonObjName) when is_list(Rows)->
    Res = lists:append([lists:append([[?ITEM_B], nameFields(R), [?ITEM_E]]) || R <- Rows]),
    mountXmlTopless(Res, JsonObjName);

encodeTopless([R], JsonObjName)->
    mountXmlTopless(nameFields(R), JsonObjName);

encodeTopless(Row, JsonObjName)->
    mountXmlTopless(toList(Row), JsonObjName).
    
encodeToplessOne(Rows, JsonObjName) when is_list(Rows)->
    Res = lists:append([nameFields(R) || R <- Rows]),
    mountXmlTopless(Res, JsonObjName).

%%
%% Собирает одиночный XML.
%%
mountXml([_H|_T]=Res, JsonObjName) ->
    Str = string:join(Res, ?DELIM),
    string:join([?XML_TOP, lists:append(["<", convert:to_list(JsonObjName), ">"]), Str, lists:append(["</", convert:to_list(JsonObjName), ">"])], ?DELIM);

mountXml(Res, JsonObjName) ->
    string:join([?XML_TOP, lists:append(["<", JsonObjName, ">"]), Res, lists:append(["</", JsonObjName, ">"])], ?DELIM).


%%
%% Собирает одиночный XML без XML_TOP.
%%

mountXmlTopless([_H|_T]=Res, JsonObjName) ->
    Str = string:join(Res, ?DELIM),
    string:join([lists:append(["<", JsonObjName, ">"]), Str, lists:append(["</", JsonObjName, ">"])], ?DELIM);
    
mountXmlTopless(Res, JsonObjName) ->
    string:join([lists:append(["<", JsonObjName, ">"]), Res, lists:append(["</", JsonObjName, ">"])], ?DELIM).

% %
% % =======================================================================
% %

nameFields([{Name, [ValI | VT ]}|T]) when is_list(ValI) ->
    [encodeTopless([ValI | VT ], convert:to_list(Name))| nameFields(T)];
    
nameFields([{Name, Val}|T]) ->
    [lists:append(["<", convert:to_list(Name), ">", encodeValue(Val), "</", convert:to_list(Name), ">"]) | nameFields(T)];
   
nameFields([])->
    [].

encodeValue(V) when is_tuple(V) ->
    R = string:join([[encodeValue(Vi)] || Vi <- tuple_to_list(V)], " "),
    toList(R);

encodeValue(V) ->
    toList(V).


toList(V) when is_atom(V), V =:= null ->
    "";
toList(V) when is_atom(V) ->
    atom_to_list(V);
toList(V) when is_binary(V) ->
    binary_to_list(V);
toList(V) when is_integer(V) ->
    integer_to_list(V);
toList(V) when is_float(V) ->
    float_to_list(V);
toList(V) when is_list(V) ->
    V.

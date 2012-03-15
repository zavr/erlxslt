%% stuff to implement a protocol with C adapter
%% {packet,4} assumed

-module(xslt_port_util).

-export([cond_result_list/2, 
         cond_result/2,  
         check_result/2, 
         port_commands/2]).

cond_result_list(Port, Tout) ->
    case cond_result(Port, Tout) of
        {ok, B} -> {ok, binary_to_list(B)};
        Error -> Error
    end.

cond_result(Port, Tout) ->
    case check_result(Port, Tout) of
        ok -> get_response(Port, Tout);
        Error -> Error
    end.

check_result(Port, Tout) ->
    case get_response(Port, Tout) of
        {ok, B} ->
            case binary_to_list(B) of
                [0,0,0,0] -> ok;
                _Any ->
                    case get_response(Port, Tout) of
                        {ok, MsgBin} ->
                            {error, binary_to_list(MsgBin)};
                        Error ->
                            Error
                    end
            end;
        Error -> Error
    end.

get_response(Port, Tout) ->
    receive
        {Port, {data, B}} -> {ok, B}
    after Tout ->
        io:format("XSLT PROCESSOR TIMEOUT~n"),
        {error, "XSLT PROCESSOR Timeout"}
    end.

port_commands(Port, Commands) ->
    lists:foreach(fun(C) -> port_command(Port,C) end, Commands). 


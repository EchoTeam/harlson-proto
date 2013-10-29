-module(harlson_proto_sendrecv).         
                               
-export([sendrecv/4, recv_unti_end/3]).

sendrecv(Addr, Port, Timeout, Binary) ->
    case gen_tcp:connect(Addr, Port, [binary, {active, false}], Timeout) of
        {ok, Sock} ->          
            try                
                ok = gen_tcp:send(Sock, Binary),
                ok = gen_tcp:shutdown(Sock, write),
                NewBinary = recv_unti_end(Sock, Timeout, []),
                gen_tcp:close(Sock),            
                {ok, NewBinary}
            catch _C:Reason -> 
                gen_tcp:close(Sock),            
                {error, Reason}                 
            end;               
        {error, Reason} ->     
            {error, Reason}    
    end.                       

recv_unti_end(Sock, Timeout, Acc) ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {ok, Binary} -> recv_unti_end(Sock, Timeout, [Binary | Acc]);
        {error, closed} -> erlang:iolist_to_binary(lists:reverse(Acc));
        {error, Reason} -> throw({bad_recv, Reason})
    end.


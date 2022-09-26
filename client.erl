-module(client).
-export([start/1,mine/3,test/1]).
-define(CONCURRENCY,4).

test(Node) ->
    spawn(client,start, [Node]).

start(Node) ->
    {_,_} = statistics(runtime),
    {server,Node} ! {self(), i_am_available},
    client_receive(Node).
 
client_receive(Node) -> 
    receive
        {server, start, Message,String, Zero} -> 
            io:format("Message from server: ~p",[Message]),
            compute(String, lists:duplicate(Zero,$0),?CONCURRENCY, Node),
            client_receive(Node);
        end_mine ->
            {_,Real} = statistics(runtime),
            {time, Node} ! {self(),Real},
            exit(shut)
    end.
enc(String) ->
    <<Integer:256>> = crypto:hash(sha256,String),
    Hash = io_lib:format("~64.16.0b", [Integer]),
    Hash.

mine(S,Zero, Node) -> 
    Nonce = binary_to_list(base64:encode(crypto:strong_rand_bytes(8))),
    Msg = lists:append(S,Nonce),
    Hash = enc(Msg),
    case string:prefix(Hash,Zero) of
        nomatch -> mine(S,Zero,Node);
        _ -> 
            {server, Node} ! {coin,"client", Hash, Nonce},
            spawn_link(client, mine, [S,Zero,Node]),
            exit(normal)

    end.

compute(S,Zero,Concurrency,Node) ->

    case Concurrency>0 of
        true ->   
            spawn_link(client, mine, [S,Zero,Node]),
            compute(S,Zero,Concurrency-1,Node);
        false -> ok
    end.
-module(server).

-export([input/2, mine/2, compute/3,listen/4, start/1,compute_time/0]).
-define(CONCURRENCY,4).
-define(GATORID,"pdande62316196").
-define(MAX_COINS, 20).

enc(String) ->      %Gives the sha256 hash of given String
    <<Byte_Hash:256>> = crypto:hash(sha256,String),
    Hash = io_lib:format("~64.16.0b", [Byte_Hash]),
    Hash.

mine(S,Zero) ->  % Mines the coin until it finds the hash with given leading 0's
    Nonce = binary_to_list(base64:encode(crypto:strong_rand_bytes(8))),
    Msg = lists:append(S,Nonce),
    Hash = enc(Msg),
    case string:prefix(Hash,Zero) of  % checks if Hash has required leading 0's 
        nomatch -> mine(S,Zero);
        _ -> 
            server ! {server,coin,Hash,Nonce},  % sends the coin to server
            exit(normal)
    end.

compute(S,Zero,Concurrency) ->  % spawns parallel minng processes based on the Concurrency defined

    case Concurrency>0 of
        true ->   
            spawn_link(server, mine, [S,Zero]),
            compute(S,Zero,Concurrency-1);
        false -> ok
    end.


input(S,Z) ->        % initiates the listener and mining processes        
    Zero = lists:duplicate(Z,$0),
    compute(S,Zero,?CONCURRENCY),
    listen(S,Z,?MAX_COINS,[]).

start(Z) ->              % main function which starts the server
    register(server, spawn(server,input, [?GATORID,Z])),
    register(time,spawn(server,compute_time,[])).

compute_time() ->    % actor process which computes the CPU , REAL and their ratio
    {_,_} = statistics(runtime),
    {_,_} = statistics(wall_clock),
    receive
        {done,Clients} ->
            {_,Cpu} = statistics(runtime),
            receive_times(Cpu,length(Clients))
    end.

receive_times(Cpu,0) ->    %fucntion which receives times from server and clients
    {_,Real} = statistics(wall_clock),
    io:format("Cpu Time:~p  Real Time: ~p  Ratio: ~p ~n", [Cpu,Real, Cpu / Real]);

receive_times(Cpu,N) ->
    receive 
        {Client,Cpu_Time} ->
            receive_times(Cpu + Cpu_Time, N-1)
    end.



end_all_clients(Clients) ->  % ends all the clients after required coins got mined
    lists:foreach(fun(Client) -> 
        Client ! end_mine
        end, Clients).


listen(S,Z,N,Clients) -> %listener which receives coins from both server and clients
    receive 
        {Client,i_am_available} ->   % when the client tries to connect to server
            io:format("Client ~p  connected~n",[pid_to_list(Client)]),
            Message = io:format("~p coins left, client joined",[N]),
            %link(Client),
            Client ! {server,start, Message,S, Z},
            listen(S,Z,N,[Client | Clients]);

        {coin, Source, Hash, Nonce} -> % when a coin got mined
            case N>0 of
                true ->
                    io:format("~p   ~p~n", [Hash,Nonce]),
                    case N-1 == 0 of %last coin mined
                        true ->
                            time ! {done,Clients} ,
                            io:format("~p coins mined.~n",[?MAX_COINS]),
                            end_all_clients(Clients),
                            listen(S,Z,N-1,Clients);
                        false ->
                            listen(S,Z,N-1,Clients)
                    end;
                    
                false ->
                    listen(S,Z,N,Clients)
                    %exit(server,shut)
            end;
        {server,coin,B,C} ->  
            case N >=1 of
                true ->
                   
                    %io:format("~p~n",[N]),
                    case N > ?CONCURRENCY  of 
                        true ->
                            spawn_link(server,mine,[S,lists:duplicate(Z,$0)]);
                        
                        false -> ok
                    end,
                server ! {coin, "server", B, C},    
                listen(S,Z,N,Clients); 
                false -> ok
            end
            
    end.
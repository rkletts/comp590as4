-module(server_chain).
-export([start/0, serv1/1, serv2/1, serv3/1]).

start() ->
    Serv1 = spawn(?MODULE, serv1, [self()]),
    loop(Serv1).

loop(Serv1) ->
    io:format("Enter a message: "),
    Msg = read_input(),
    case Msg of
        all_done -> ok;
        _ -> 
            Serv1 ! {self(), Msg},
            loop(Serv1)
    end.

read_input() ->
    {ok, Tokens, _} = io:fread("", "~s"),
    list_to_atom(hd(Tokens)).

serv1(Serv2) ->
    Serv2_Pid = spawn(?MODULE, serv2, [self()]),
    receive
        {_, {add, A, B}} ->
            io:format("(serv1) ~p + ~p = ~p~n", [A, B, A + B]),
            serv1(Serv2_Pid);
        {_, {sub, A, B}} ->
            io:format("(serv1) ~p - ~p = ~p~n", [A, B, A - B]),
            serv1(Serv2_Pid);
        {_, {mult, A, B}} ->
            io:format("(serv1) ~p * ~p = ~p~n", [A, B, A * B]),
            serv1(Serv2_Pid);
        {_, {div, A, B}} when B =/= 0 ->
            io:format("(serv1) ~p / ~p = ~p~n", [A, B, A / B]),
            serv1(Serv2_Pid);
        {_, {neg, A}} ->
            io:format("(serv1) -~p = ~p~n", [A, -A]),
            serv1(Serv2_Pid);
        {_, {sqrt, A}} when A >= 0 ->
            io:format("(serv1) sqrt(~p) = ~p~n", [A, math:sqrt(A)]),
            serv1(Serv2_Pid);
        {_, halt} ->
            Serv2_Pid ! {self(), halt},
            io:format("(serv1) Halting...~n");
        Msg ->
            Serv2_Pid ! {self(), Msg},
            serv1(Serv2_Pid)
    end.

serv2(Serv3) ->
    Serv3_Pid = spawn(?MODULE, serv3, [0]),
    receive
        {_, [H | T]} when is_integer(H) ->
            Sum = lists:sum([X || X <- [H | T], is_number(X)]),
            io:format("(serv2) Sum = ~p~n", [Sum]),
            serv2(Serv3_Pid);
        {_, [H | T]} when is_float(H) ->
            Prod = lists:foldl(fun(X, Acc) when is_number(X) -> X * Acc; (_, Acc) -> Acc end, 1, [H | T]),
            io:format("(serv2) Product = ~p~n", [Prod]),
            serv2(Serv3_Pid);
        {_, halt} ->
            Serv3_Pid ! {self(), halt},
            io:format("(serv2) Halting...~n");
        Msg ->
            Serv3_Pid ! {self(), Msg},
            serv2(Serv3_Pid)
    end.

serv3(Count) ->
    receive
        {_, {error, Msg}} ->
            io:format("(serv3) Error: ~p~n", [Msg]),
            serv3(Count);
        {_, halt} ->
            io:format("(serv3) Halting... Total unprocessed messages: ~p~n", [Count]);
        Msg ->
            io:format("(serv3) Not handled: ~p~n", [Msg]),
            serv3(Count + 1)
    end.

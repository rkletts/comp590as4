-module(chain).
-export([start/0, serv1/0, serv2/0, serv3/1]).

start() ->
    Serv1 = spawn(chain, serv1, []),
    loop(Serv1).

loop(Serv1) ->
    io:format("Enter a message (or 'all_done' to exit): "),
    Input = io:get_line(""),
    Message = parse_input(Input),
    case Message of
        all_done ->
            Serv1 ! halt,
            io:format("Main function halting~n");
        _ ->
            Serv1 ! Message,
            loop(Serv1)
    end.

parse_input(Input) ->
    % Here you would parse the input string into the appropriate message format
    % For simplicity, let's assume the input is already in the correct format
    list_to_atom(string:trim(Input)).

serv1() ->
    receive
        {add, X, Y} ->
            Result = X + Y,
            io:format("(serv1) ~p + ~p = ~p~n", [X, Y, Result]),
            serv1();
        {sub, X, Y} ->
            Result = X - Y,
            io:format("(serv1) ~p - ~p = ~p~n", [X, Y, Result]),
            serv1();
        {mult, X, Y} ->
            Result = X * Y,
            io:format("(serv1) ~p * ~p = ~p~n", [X, Y, Result]),
            serv1();
        {div, X, Y} ->
            Result = X / Y,
            io:format("(serv1) ~p / ~p = ~p~n", [X, Y, Result]),
            serv1();
        {neg, X} ->
            Result = -X,
            io:format("(serv1) -~p = ~p~n", [X, Result]),
            serv1();
        {sqrt, X} ->
            Result = math:sqrt(X),
            io:format("(serv1) sqrt(~p) = ~p~n", [X, Result]),
            serv1();
        halt ->
            io:format("(serv1) Halting~n"),
            serv2();
        Msg ->
            Serv2 = spawn(chain, serv2, []),
            Serv2 ! Msg,
            serv1()
    end.

serv2() ->
    receive
        [H | T] when is_integer(H) ->
            Sum = lists:sum([X || X <- [H | T], is_number(X)]),
            io:format("(serv2) Sum = ~p~n", [Sum]),
            serv2();
        [H | T] when is_float(H) ->
            Product = lists:foldl(fun(X, Acc) -> X * Acc end, 1, [X || X <- [H | T], is_number(X)]),
            io:format("(serv2) Product = ~p~n", [Product]),
            serv2();
        halt ->
            io:format("(serv2) Halting~n"),
            Serv3 = spawn(chain, serv3, [0]),
            Serv3 ! halt;
        Msg ->
            Serv3 = spawn(chain, serv3, [0]),
            Serv3 ! Msg,
            serv2()
    end.

serv3(UnprocessedCount) ->
    receive
        {error, Msg} ->
            io:format("(serv3) Error: ~p~n", [Msg]),
            serv3(UnprocessedCount);
        halt ->
            io:format("(serv3) Halting with ~p unprocessed messages~n", [UnprocessedCount]);
        Msg ->
            io:format("(serv3) Not handled: ~p~n", [Msg]),
            serv3(UnprocessedCount + 1)
    end.

-module(chain_server).
-export([start/0]).

% Team members: [Reese Letts and Maria Thomas]

start() ->
    Serv3Pid = spawn(fun() -> serv3(0) end),
    Serv2Pid = spawn(fun() -> serv2(Serv3Pid) end),
    Serv1Pid = spawn(fun() -> serv1(Serv2Pid) end),
    user_input(Serv1Pid).

user_input(Serv1Pid) ->
    Input = io:get_line("Enter a message (or 'all_done' to quit): "),
    Trimmed = string:trim(Input),
    case Trimmed of
        "all_done" ->
            Serv1Pid ! halt,
            io:format("Main process ending.~n");
        _ ->
            Term = parse_input(Trimmed),
            Serv1Pid ! Term,
            user_input(Serv1Pid)
    end.

parse_input(Input) ->
    try
        {ok, Tokens, _} = erl_scan:string(Input ++ "."),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _:_ -> Input
    end.

serv1(Serv2Pid) ->
    receive
        {Op, A, B} when Op == add; Op == sub; Op == mult; Op == 'div' ->
            Result = case Op of
                add -> A + B;
                sub -> A - B;
                mult -> A * B;
                'div' -> A / B
            end,
            io:format("(serv1) ~p ~p ~p = ~p~n", [Op, A, B, Result]),
            serv1(Serv2Pid);
        {Op, A} when Op == neg; Op == sqrt ->
            Result = case Op of
                neg -> -A;
                sqrt -> math:sqrt(A)
            end,
            io:format("(serv1) ~p ~p = ~p~n", [Op, A, Result]),
            serv1(Serv2Pid);
        halt ->
            Serv2Pid ! halt,
            io:format("(serv1) Halting.~n");
        Other ->
            Serv2Pid ! Other,
            serv1(Serv2Pid)
    end.

serv2(Serv3Pid) ->
    receive
        [Head | _] = List when is_number(Head) ->
            case is_integer(Head) of
                true ->
                    Sum = lists:sum([X || X <- List, is_number(X)]),
                    io:format("(serv2) Sum of numbers in list: ~p~n", [Sum]);
                false ->
                    Product = lists:foldl(fun(X, Acc) -> 
                        case is_number(X) of
                            true -> X * Acc;
                            false -> Acc
                        end
                    end, 1, List),
                    io:format("(serv2) Product of numbers in list: ~p~n", [Product])
            end,
            serv2(Serv3Pid);
        halt ->
            Serv3Pid ! halt,
            io:format("(serv2) Halting.~n");
        Other ->
            Serv3Pid ! Other,
            serv2(Serv3Pid)
    end.

serv3(Count) ->
    receive
        {error, Msg} ->
            io:format("(serv3) Error: ~p~n", [Msg]),
            serv3(Count);
        halt ->
            io:format("(serv3) Unprocessed message count: ~p~n", [Count]),
            io:format("(serv3) Halting.~n");
        Other ->
            io:format("(serv3) Not handled: ~p~n", [Other]),
            serv3(Count + 1)
    end.

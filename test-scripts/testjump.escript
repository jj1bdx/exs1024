#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

-define(TARGET_MODULE, exs1024).

loop(_S, 0) -> ok;
loop(S, N) ->
    {V, NS} = ?TARGET_MODULE:next(?TARGET_MODULE:jump(S)),
    io:format("~p~n", [V]),
    loop(NS, N - 1).

main(_) ->

    code:load_file(?TARGET_MODULE),
    {V, S} = ?TARGET_MODULE:next(?TARGET_MODULE:seed()),
    io:format("~p~n", [V]),
    ok = loop(S, 999).

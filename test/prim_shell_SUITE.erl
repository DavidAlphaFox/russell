-module(prim_shell_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [file_not_exist,
     verify_error,
     demo0].

format_prompt(Prompt, Encoding) ->
    lists:flatten(io_lib:format_prompt(Prompt, Encoding)).

consume([], Text) ->
    Text;
consume([H|T1], [H|T2]) ->
    consume(T1, T2).

read_line(Text, Prompt) ->
    Text1 = consume(format_prompt(Prompt, unicode), Text),
    case string:str(Text1, "\n") of
        0 ->
            {Text1, []};
        Index ->
            {lists:sublist(Text1, 1, Index),
             lists:nthtail(Index, Text1)}
    end.

loop(Text) ->
    receive
        {io_request, From, ReplyAs, {get_line, unicode, Prompt}} ->
            case read_line(Text, Prompt) of
                {"\n", []} ->
                    From ! {io_reply, ReplyAs, eof},
                    Text1 = "\n";
                {Result, Text1} ->
                    From ! {io_reply, ReplyAs, Result}
            end,
            loop(Text1);
        {io_request, From, ReplyAs, {put_chars, unicode, Module, Function, Args}} ->
            case consume(unicode:characters_to_list(apply(Module, Function, Args)), Text) of
                [] ->
                    ok;
                Text1 ->
                    From ! {io_reply, ReplyAs, ok},
                    loop(Text1)
            end;
        Data ->
            io:format("Unknown Data: ~p~n~n", [Data]),
            error
    end.

start_verify_session(Text, Args) ->
    process_flag(trap_exit, true),
    G = group_leader(),
    group_leader(self(), self()),
    Shell = spawn_link(fun() -> russell:run(["prim", "shell"|Args]) end),
    group_leader(G, self()),

    try
        ok = loop(Text)
    after
            exit(Shell, kill)
    end.

verify_session(Session, Args) ->
    {ok, Bin} = file:read_file(Session),
    Text = unicode:characters_to_list(Bin),
    {Pid, Ref} = spawn_monitor(fun() -> start_verify_session(Text, Args) end),
    receive
        {'DOWN', Ref, process, Pid, Info} ->
            normal = Info
    end.

file_not_exist(C) ->
    Args = [filename:join(?config(data_dir, C), "file_not_exist.prim")],
    {error, {_, russell_prim, {file_not_exist, _}}} =
        russell_prim_shell:run(Args),
    ok.

verify_error(C) ->
    Args = [filename:join(?config(data_dir, C), "verify_error.prim")],
    {error, _} =
        russell_prim_shell:run(Args),
    ok.

demo0(C) ->
    Args = [filename:join(code:lib_dir(russell, priv), "demo0.prim")],
    Session = filename:join(?config(data_dir, C), "demo0.session"),
    verify_session(Session, Args),
    ok.

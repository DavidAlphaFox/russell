-module(prim_shell_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [demo0].

format_prompt(Prompt, Encoding) ->
    lists:flatten(io_lib:format_prompt(Prompt, Encoding)).

consume([], Text) ->
    Text;
consume([H|T1], [H|T2]) ->
    consume(T1, T2).

read_line(Text, Prompt) ->
    case consume(format_prompt(Prompt, unicode), Text) of
        "\n" ->
            {eof, []};
        Text1 ->
            case string:str(Text1, "\n") of
                0 ->
                    {Text1, []};
                Index ->
                    {lists:sublist(Text1, 1, Index),
                     lists:nthtail(Index, Text1)}
            end
    end.

loop(Text) ->
    receive
        {io_request, From, ReplyAs, {get_line, unicode, Prompt}} ->
            {Result, Text1} = read_line(Text, Prompt),
            From ! {io_reply, ReplyAs, Result},
            case Result of
                eof ->
                    ok;
                _ ->
                    loop(Text1)
            end;
        {io_request, From, ReplyAs, {put_chars, unicode, Module, Function, Args}} ->
            Text1 = consume(unicode:characters_to_list(apply(Module, Function, Args)), Text),
            From ! {io_reply, ReplyAs, ok},
            loop(Text1);
        Data ->
            io:format("Unknown Data: ~p~n~n", [Data]),
            error
    end.

start_verify_session(Text, Args) ->
    process_flag(trap_exit, true),
    G = group_leader(),
    group_leader(self(), self()),
    Shell = spawn_link(fun() -> russell_prim_shell:run(Args) end),
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

demo0(C) ->
    Args = [filename:join(code:lib_dir(russell, priv), "demo0.prim")],
    Session = filename:join(?config(data_dir, C), "demo0.session"),
    verify_session(Session, Args),
    ok.

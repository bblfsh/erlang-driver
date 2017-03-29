-module(driver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,read_input/0]).



start(_StartType, _StartArgs) ->
    read_input(),
    driver_sup:start_link().

stop(_State) ->
    ok.

read_input() ->
    case io:get_line("Reading>") of
        eof ->
            ok;
        N ->
            SubS = string:substr(N,1,string:len(N)-1),
            Data= jsx:decode(list_to_binary(SubS)),
            Content = proplists:get_value(<<"content">>,Data),
            ExprList = tokenize(Content),
            ParseList = parse(ExprList),
            io:format("~p\n",[ParseList]),
            read_input()
    end.

tokenize(Content) ->
    Formated= string:join(lists:map(fun erlang:binary_to_list/1,[Content]),""),
    string:tokens(Formated,".").

parse(ExprList) ->
    lists:foldl(fun (Expr,ParseList)->
        ExprDot = string:concat(Expr,"."),
        {ok, Tokens, _} = erl_scan:string(ExprDot),
        AST = parseExpr(Tokens),
        lists:append(ParseList,AST)
    end,[],ExprList).


parseExpr(Tokens) ->
    {ok, AbsForm} =
        try
            {ok, _} = erl_parse:parse_form(Tokens)
        catch
            _:_ ->
                {ok, _} = erl_parse:parse_exprs(Tokens)
        end,
    Result = if
      is_list(AbsForm) -> AbsForm;
      true -> [AbsForm]
    end,
    Result.

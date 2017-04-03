-module(driver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,process/0]).



start(_StartType, _StartArgs) ->
    process(),
    driver_sup:start_link().

stop(_State) ->
    ok.

process() ->
    case io:get_line("Reading>") of
        eof ->
            ok;
        N ->
            Content = decode(N),
            ExprList = tokenize(Content),
            ParseList = parse(ExprList),
            FormatParse = format(ParseList),
            JSON = jsx:encode([{<<"AST">>,FormatParse}]),
            io:format("~p\n",[JSON]),
            process()
    end.

decode(InputSrt) ->
    SubS = string:substr(InputSrt,1,string:len(InputSrt)-1),
    Data= jsx:decode(list_to_binary(SubS)),
    proplists:get_value(<<"content">>,Data).

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

%% Format do a conversion of erlang tuples to list, also change strings to binaries
format(T) when is_list(T)->
    format(list_to_tuple(T));
format(T) ->
    format(T, tuple_size(T), []).

format(T, 0, Acc) ->
    Result = case io_lib:printable_unicode_list(Acc) of
        true -> list_to_binary(Acc);
        false -> Acc
    end,
    Result;
format(T, N, Acc) when is_tuple(element(N,T)) ->
    format(T, N-1, [format(element(N,T),tuple_size(element(N,T)),[])|Acc]);
format(T,N,Acc) when is_list(element(N,T)) ->
    Tuple = list_to_tuple(element(N,T)),
    format(T, N-1, [format(Tuple,tuple_size(Tuple),[])|Acc]);
format(T,N,Acc)->
    format(T,N-1,[element(N,T)|Acc]).

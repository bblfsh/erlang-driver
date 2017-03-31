-module(driver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,read_input/0,tup2list/1]).



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

tup2list(T) when is_list(T)->
    tup2list(list_to_tuple(T));
tup2list(T) ->
    tup2list(T, tuple_size(T), []).


tup2list(T, 0, Acc) ->
    Acc;
tup2list(T, N, Acc) when is_tuple(element(N,T)) ->
    tup2list(T, N-1, [tup2list(element(N,T),tuple_size(element(N,T)),[])|Acc]);
tup2list(T,N,Acc) when is_list(element(N,T)) ->
    Tuple = list_to_tuple(element(N,T)),
    tup2list(T, N-1, [tup2list(Tuple,tuple_size(Tuple),[])|Acc]);
tup2list(T,N,Acc)->
    tup2list(T,N-1,[element(N,T)|Acc]).

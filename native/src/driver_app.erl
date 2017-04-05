-module(driver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,loop/0]).



start(_StartType, _StartArgs) ->
    loop(),
    driver_sup:start_link().

stop(_State) ->
    ok.

loop()->
  try
    process()
  catch
    throw:{json,BadJSON} ->
        Json =jsx:encode([{<<"errors">>,[BadJSON]},{<<"AST">>,[""]}]),
        io:format("~p\n",[Json]);
    throw:{parse,BadParse}->
        {_,_,ErrorAux} = BadParse,
        ErrStr = list_to_binary(lists:concat(["An error ocurred while parsing: ",lists:concat(ErrorAux)])),
        Json =jsx:encode([{<<"errors">>,[ErrStr]},{<<"AST">>,[""]}]),
        io:format("~p\n",[Json]);
    throw:_ ->
        Json =jsx:encode([{<<"errors">>,[<<"Unexpected error">>]},{<<"AST">>,[""]}]),
        io:format("~p\n",[Json])
  end,
  loop().

process() ->
    case io:get_line("") of
        eof ->
            ok;
        N ->
            Content = case  decode(N) of
              {ok,Res} -> Res;
              {error,BadJSON} ->
                  throw(BadJSON)
            end,
            ExprList = tokenize(Content),

            ParseList = parse(ExprList),
            FormatParse = format(ParseList),
            JSON = jsx:encode([{<<"AST">>,FormatParse}]),
            io:format("~p\n",[JSON])
    end.

decode(InputSrt) ->
    SubS = string:substr(InputSrt,1,string:len(InputSrt)-1),
    case jsx:is_json(list_to_binary(SubS)) of
      true->Data= jsx:decode(list_to_binary(SubS)),
          case proplists:lookup(<<"content">>,Data) of
              none -> {error,{json,<<"Content propertie don't found in JSON input">>}};
              _ -> Content = proplists:get_value(<<"content">>,Data),
                  {ok,Content}
          end;
      false->
          {error,{json,<<"Input is not a valid JSON">>}}
      end.
tokenize(Content) ->
    Formated= string:join(lists:map(fun erlang:binary_to_list/1,[Content]),""),
    string:tokens(Formated,".").

parse(ExprList) ->
    List = lists:foldl(fun (Expr,ParseList)->
        ExprDot = string:concat(Expr,"."),
        {ok, Tokens, _} = erl_scan:string(ExprDot),
        case parseExpr(Tokens) of
          {ok,AST} -> lists:append(ParseList,AST);
          {error,BadParse} -> throw({parse,BadParse})
        end
    end,[],ExprList),
    {ok,List}.

parseExpr(Tokens) ->
    case erl_parse:parse_form(Tokens) of
        {ok,AbsForm} -> {ok,[AbsForm]};
        {error,_}->
            case erl_parse:parse_exprs(Tokens) of
                {ok,AbsForm} -> {ok,AbsForm};
                {error,BadParse} -> {error,BadParse}
            end
    end.

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

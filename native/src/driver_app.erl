%%%-------------------------------------------------------------------
%% @doc driver public API
%% @end
%%%-------------------------------------------------------------------

-module(driver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,read_input/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    read_input(),



    driver_sup:start_link().
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% Read input as a string
read_input() ->
    case io:get_line("Reading>") of
        eof ->
            ok;
        N ->
            % display the list
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
        IsForm = (element(1,hd(Tokens))=='-') and not is_number(element(2,hd(Tokens))),
        {ok, ListAST} = parseExpr(IsForm,Tokens),
        lists:append(ParseList,ListAST)
    end,[],ExprList).

parseExpr(true,Tokens) ->
  erl_parse:parse_form(Tokens);
parseExpr(false,Tokens) ->
  erl_parse:parse_exprs(Tokens).

%% Coers is a small tool for inline conversion.
%% It is a part of LDC-Json project

-module(coers).
-vsn(1).
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).

-include("coers_type.hrl").


-export([to_string/1, of_string/1]).
-export([to_int/1]).


%% Try to convert a term into a String
-spec to_string(any()) -> string().
to_string(Term) ->
    List = io_lib:format("~p", [Term]),
    lists:flatten(List).

%% Try to convert a string into a term, wrapped into a 
%% wrapped_result
-spec of_string(string()) -> wrapped_result().
of_string(String) ->
    {ok, Regexp} = re:compile("^.+(\\,|\\;|\\.)$"),
    S = case re:run(String, Regexp) of 
            {match, [_, {Offset, _}]} -> 
                Substring = string:substr(String, 1, Offset -1),
                Substring ++ ".";
            _ -> String ++ "."
        end,
    case erl_scan:string(S) of 
        {ok, Tokens, _} -> 
            case erl_parse:parse_exprs(Tokens) of 
                {ok, Exprs} ->
                    {value, Result, []} = erl_eval:exprs(Exprs, []),
                    {ok, Result};
                {error, {_, Mod, Desc}} -> {error, Mod, Desc}
            end;
        {error, {_, Mod, Desc}, _} -> {error, Mod, Desc}
    end.
            
%% Int coersion rules
to_int(Object) when is_integer(Object) -> {ok, Object};
to_int(Object) when is_list(Object) -> list_to_integer(Object);  
to_int(Object) when is_float(Object) -> round(Object);
to_int(Object) when is_atom(Object) -> 
    Pred = atom_to_list(Object),
    to_int(Pred).

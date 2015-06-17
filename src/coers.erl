%% @copyright 2015 Derniercri
%% @version 1.0.0
%% @title `coers`, small coersion library

-module(coers).
-vsn(1).
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-include("coers_type.hrl").


-export([is_string/1, to_string/1, of_string/1]).
-export([to_int/1]).
-export([to_float/1]).
-export([to_atom/1]).
-export([to_bool/1]).


%% @doc Check if a list is a String
-spec is_string(list()) -> boolean().
is_string(List) when is_list(List) -> 
    lists:all(fun(X) -> (X >= 32) and (X < 127) end, List);
is_string(_) -> false. 

%% @doc Try to convert a term into a String
-spec to_string(any()) -> string().
to_string(Term) ->
    case is_string(Term) of 
        true -> Term;
        false ->
            List = io_lib:format("~p", [Term]),
            lists:flatten(List)
    end.

%% @doc Try to convert a string into a term, wrapped into a wrapped_result
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

%% @doc Return the color of a string
-spec string_color(string()) -> atom().
string_color(String) ->
    {ok, Regexp} = re:compile("^\\d+(\\.|\\,)?"),
    case re:run(String, Regexp) of 
        {match, [_A]} -> integer;
        {match, [_A,_B]} -> float;            
        _ -> any
    end.

%% @doc Try to convert a term into an integer
-spec to_int(primitive_for_int()) -> wrapped_result().
to_int(Object) when is_integer(Object) -> {ok, Object};
to_int(Object) when is_float(Object) -> {ok, round(Object)};
to_int(Object) when is_list(Object) -> 
    try list_to_integer(Object) of 
        Result -> {ok, Result}
    catch _:_ ->
            case string_color(Object) of 
                float -> to_int(list_to_float(Object));
                _ -> {error, 0}
            end
    end;
to_int(Object) when is_atom(Object) -> 
    try
        Pred = atom_to_list(Object),
        to_int(Pred) of 
        Result -> Result
    catch  _:_ -> {error, 0}
    end;
to_int(_) -> {error, 0}. 

%% @doc Try to convert a term into a float
-spec to_float(primitive_for_int()) -> wrapped_result().
to_float(Object) when is_float(Object) -> {ok, Object};
to_float(Object) when is_integer(Object) -> {ok, float(Object)};
to_float(Object) when is_list(Object) ->
    try list_to_float(Object) of 
        Result -> {ok, Result}
    catch _:_ ->
            case string_color(Object) of 
                integer -> to_float(list_to_integer(Object));
                _ -> {error, 0.0}
            end
    end;
to_float(Object) when is_atom(Object) ->
    try
        Pred = atom_to_list(Object),
        to_float(Pred) of 
        Result -> Result
    catch _:_ -> {error, 0.0}
    end;
to_float(_) -> {error, 0.0}.

%% @doc Try to convert a term into an atom
-spec to_atom(primitive_for_int()) -> wrapped_result().
to_atom(Object) when is_atom(Object) -> {ok, Object};
to_atom(Object) when is_list(Object)-> 
    try list_to_atom(Object) of 
        Result -> {ok, Result}
    catch _:_ -> {error, false}
    end;
to_atom(Object) when is_number(Object) -> 
    Pred = to_string(Object),
    to_atom(Pred).
        

%% @doc Try to convert a term into a boolean
-spec to_bool(primitive_for_int()) -> wrapped_result().
to_bool(Object) when is_atom(Object) -> {ok, not (Object == false)};
to_bool(Object) when is_list(Object) ->
    case string:to_lower(Object) of 
        "true" -> {ok, true};
        "false" -> {ok, false};
        _ -> {error, true}
    end;
to_bool(0) -> {ok, false};
to_bool(1) -> {ok, true};
to_bool(_) -> {error, true}. 

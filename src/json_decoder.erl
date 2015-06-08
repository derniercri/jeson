 %% @author Arthur d'Azémar
 %% @doc function for convert erlang value into json string and json string into erlang value

-module(json_decoder).
-export([gen_decoder/3]).
-include("json_type.hrl").
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-vsn(1).



%% enlève les espace et le : entre le nom d'un champ et sa valeur
purify(String) -> purify(String, false).
purify([32 | T], Bool) -> purify(T, Bool);
purify([$: | _], true) -> throw('parse_error : too much :'); %% il y a un : en trop
purify([$: | T], false) -> purify(T, true);
purify(String, true) -> String;
purify(_, false) -> throw('parse_error : missing : '). %%le : manque pour séparer le champ de la valeur

%% extrait une chaine de caractère json 
-spec extract_string(string()) -> {string(), string()}.
%% extract_string([ 32 | T]) ->
%%     extract_value(T);
%% extract_string([$:| T]) ->
%%     extract_value(T);
extract_string(String) ->
    case purify(String) of
	[$\\, $" | S] -> extract_string(S, "");
	Other ->
	    io:format("~w~n",[Other]),
	    throw("parse_error : the string " ++ Other ++ "dont't begin with \\\"")
    end.


-spec extract_string(string(), string()) -> {string(), string()}.
extract_string([C], Acc) ->
    if 
	(C =:= $")  -> {lists:reverse(Acc), []};
	true ->  throw('parse_error : end of stream before object ending')
    end;

extract_string([C | T], Acc) ->
    case C of
	$, ->
	    Acc_striped = string:strip(Acc, left),
	    case Acc_striped of
		[$", $\\, $\\ | _] -> extract_string(T, [C | Acc_striped]);
		%%AF chercher dans l'ACC la strucure [$", $\\ en sortant les espace
		[$", $\\ | Acc_tail] -> {lists:reverse(Acc_tail), T};
		_ -> extract_string(T, [C | Acc])
	    end;
	_ -> extract_string(T, [C | Acc])
    end.

extract_value(String) ->
    extract_value(purify(String),"").

extract_value([$, | T], Acc) -> 
    {lists:reverse(string:strip(Acc, left)), T};
extract_value([C | T], Acc) ->
    extract_value(T, [C | Acc]).


simple_echaped([$\\ , C |_]) -> C =/= $\\;
simple_echaped(_) -> false.

    
extract_list(String) ->
    extract_list(purify(String), "", false, 0).

extract_list([$" | T], Acc,  false, N) -> 
    case simple_echaped(Acc) of  
	true -> extract_list(T, [$" | Acc], true, N);
	false -> throw("parse error : too much \\")
    end;
extract_list([$" | T], Acc, true, N) -> 
	case simple_echaped(Acc) of
	    true -> extract_list(T, [$" | Acc],  false, N);
	    false -> extract_list(T, [$" | Acc], true, N)
    end;
extract_list([C | T], Acc, true, N) ->
    extract_list(T, [C | Acc], true, N);
extract_list([C | T], Acc, false, N_bracket) when C =:= $[->
    extract_list(T, [C | Acc], false, N_bracket + 1);
extract_list([C | T], Acc, false, 1) when C =:= $]->
    T_striped = string:strip(T, left),
    case T_striped of
	[$, | T2 ] -> {lists:reverse([C|Acc]), T2};
	_ -> throw("parse error : missing ,")
    end;
extract_list([C | T], Acc, false, N_bracket) when C =:= $]->
    extract_list(T, [C | Acc], false, N_bracket - 1);
extract_list([C | T], Acc, In_string, N) ->
    extract_list(T, [C | Acc], In_string, N).

%%renvoi la liste des valeurs extraites
-spec extract_list_value(string()) -> [string()].
extract_list_value(String) ->
    [_ | T] = String,
    extract_list_value(T, "", [], false, 1).

extract_list_value([$" | T], Acc_value, Acc_list, false, N) ->
    case simple_echaped(Acc_value) of  
	true -> extract_list_value(T, [$" | Acc_value],Acc_list, true, N);
	false -> throw("parse error : too much \\")
    end;
extract_list_value([$" | T], Acc_value, Acc_list, true, N) -> 
	case simple_echaped(Acc_value) of
	    true -> extract_list_value(T, [$" | Acc_value], Acc_list,  false, N);
	    false -> extract_list_value(T, [$" | Acc_value], Acc_list, true, N)
    end;
extract_list_value([C | _], Acc_value, Acc_list, false, 1) when (C =:= $]) ->
    lists:reverse([string:strip(lists:reverse(Acc_value), both)  | Acc_list]);
extract_list_value([C | T], Acc_value, Acc_list, false, N_bracket) when C =:= $[->
    extract_list_value(T, [C | Acc_value], Acc_list, false, N_bracket + 1);
extract_list_value([C | T], Acc_value, Acc_list, false, N_bracket) when (C =:= $]) ->
    extract_list_value(T, [C | Acc_value], Acc_list, false, N_bracket - 1);
extract_list_value([C | T], Acc_value, Acc_list, false, 1) when (C =:= $,) ->
    extract_list_value(T, "", [string:strip(lists:reverse(Acc_value), both) | Acc_list], false, 1);
extract_list_value([C | T], Acc_value, Acc_list, In_string, N_bracklet) ->
    extract_list_value(T, [C | Acc_value], Acc_list, In_string, N_bracklet).

%% convertie une liste json en liste erlang
parse_pure_list(String, Type) ->
    case (String) of
	[$[ | _] ->      
	    P = fun (Value, Acc) -> [convert_value(Value, Type) | Acc] end,
	    Value_list = extract_list_value(String),
	    lists:reverse(lists:foldl(P, [], Value_list));
	_ -> throw("parse error, the list must begin with a [")
    end.

parse_impure_list(String, Types) ->
    case (String) of
	[$[ | List_string] ->  
	    P = fun ({Value, Type}, Acc) -> [convert_value(Value, Type) | Acc] end,
	    Value_list = extract_list_value(String),
	    Info_list = lists:zip(Value_list, Types),
	    lists:reverse(lists:foldl(P, [], Info_list));
	_ -> throw("parse error, the list must begin with a [")
    end.
	    
extract_field_name(String) ->
    extract_field_name(String, "").
extract_field_name([$" | T], Acc) ->
    case Acc of
	[$\\, $\\ | _] -> extract_field_name(T, [$" | Acc]);
	[$\\ | Field_name] -> {lists:reverse(Field_name), T}
    end;
extract_field_name([H | T], Acc_field_name) ->
    extract_field_name(T, [H | Acc_field_name]).


-spec convert_value(string(), json_type()) -> any().
convert_value(Value, Type) ->
    case convert_value_aux(Value, Type) of
	{error, _} -> io:format( "value : ~p type ~p~n", [Value, Type]),
		      throw("conversion erro");
	{ok, Val} -> Val
    end.

convert_value_aux(Value, int) ->
    coers:to_int(Value);
convert_value_aux(Value, string) ->
    {ok, Value};
convert_value_aux(Value, atom) ->
    coers:to_atom(Value);
convert_value_aux(Value, {pure_list, Type}) ->
    {ok, parse_pure_list(Value, Type)};
convert_value_aux(Value, {impure_list, Type}) ->
    {ok, parse_impure_list(Value, Type)};
convert_value_aux(Value, float) ->
    coers:to_float(Value).

%% -spec parse_object(string(), #{string() => json_type()}, #{string() => any()}) -> [{string(), any()}].

parse_object([], _, _) -> throw(parse_error);

parse_object([C | T], Field_info, Acc) ->
    case C of
	${ -> parse_object(T, Field_info, Acc);
	$} -> Acc;
	32 -> parse_object(T, Field_info, Acc);
	$\\ -> case T of 
		   %%si on tombe sur un caractère échapé on regarde si c'est une guillement (sinon on lance une exepction)
		   [$" | T2] -> 
		       %%on extrait le nom du champ
		       {Field_name, T3} = extract_field_name(T2),
		       %%on traite la valeur en fonction du type du champ 
		       case maps:find(Field_name, Field_info) of
			   error -> throw(wrong_field_name);
			   {ok, string} ->
			       {Value, T4} = extract_string(T3),
			       parse_object(T4, Field_info, maps:put(Field_name, Value, Acc));
			   {ok, {pure_list, Type}} ->
			       {Value, T4} = extract_list(T3),
			       parse_object(T4, Field_info, maps:put(Field_name, convert_value(Value, {pure_list, Type}), Acc));
			   {ok, {impure_list, Type}} ->
			       {Value, T4} = extract_list(T3),
			       parse_object(T4, Field_info, maps:put(Field_name, convert_value(Value, {impure_list, Type}), Acc));
			   {ok, Type} -> 
			       {Value, T4} = extract_value(T3),
			       parse_object(T4, Field_info, maps:put(Field_name, convert_value(Value, Type), Acc))
		       end;
		   [Carac | _] ->  io:format("Acc2 : ~p, S : |~s|~n",[Acc,[C | T]]),
			       throw("parse_error : " ++ [Carac] ++ "is wrong")
	       end;
	C -> io:format("Acc : ~p, S : |~s|~n",[Acc,[C | T]]),
	     throw("parse_error : " ++ [C] ++ "is wrong")
    end.

map_to_record(Map, Fields, Record_name) ->
    P = fun(Field_name, Acc) ->	[maps:get(Field_name, Map) | Acc] end,
    List = [Record_name | lists:reverse(lists:foldl(P, [], Fields))],
    list_to_tuple(List).


json_to_record(String, Type_list, Record_name, Fields) ->
    Map = maps:from_list(lists:zip(Fields, Type_list)),
    Result = parse_object(String, Map, maps:new()),
    map_to_record(Result, Fields, Record_name).
    


%% @doc generate a function which convert a json string into an erlang record
%% @spec gen_decoder(Type_list:: [json_type()], Fields::[string()], Record_name::atom()) -> fun ((string()) -> tuple())
%% gen_encoder(Type_list, Fields, Record_name)
-spec gen_decoder(Type_list:: [json_type()], Fields::[string()], Record_name::atom()) -> fun ((string()) -> tuple()).
gen_decoder(Type_list, Fields, Record_name) ->
    fun(String) ->
	    json_to_record(String, Type_list, Record_name, Fields)
    end.



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
	    throw("parse_error : the string " ++ Other ++ "dont't begin with \\\"")
    end.


-spec extract_string(string(), string()) -> {string(), string()}.
extract_string([], _) ->
    throw('parse_error : end of stream before object ending');

extract_string([C | T], Acc) ->
    if 
	(C =:= $,) or (C =:= $}) ->
	    Acc_striped = string:strip(Acc, left),
	    case Acc_striped of
		[$", $\\, $\\ | _] -> extract_string(T, [C | Acc_striped]);
		%%AF chercher dans l'ACC la strucure [$", $\\ en sortant les espace
		[$", $\\ | Acc_tail] -> {lists:reverse(Acc_tail), T};
		_ -> extract_string(T, [C | Acc])
	    end;
	true -> extract_string(T, [C | Acc])
    end.

%%extrait une valeur json d'une chaine
%%AF lancer une exeption quand un espace est entre 2 valeur ex : {champ:  val val}
%% extract_value([ C | T]) when (C =:= 32) or (C =:= $:) ->
%%     extract_value(T);
extract_value(String) ->
    extract_value(purify(String),"").

extract_value([C | T], Acc) when (C =:= $,) or (C =:= $})-> 
    {lists:reverse(string:strip(Acc, left)), T};
extract_value([C | T], Acc) ->
    extract_value(T, [C | Acc]).

simple_echaped([$\\ ]) -> true;
simple_echaped([$\\ , C |_]) -> C =/= $\\;
simple_echaped(_) -> false.


extract_object(String, Separator_end, Separator_begin) ->
    extract_object(purify(String), "", false, 0, Separator_end, Separator_begin).

extract_object([$" | T], Acc,  false, N, Separator_end, Separator_begin) -> 
    case simple_echaped(Acc) of  
	true -> extract_object(T, [$" | Acc], true, N, Separator_end, Separator_begin);
	false -> throw("parse error : too much \\")
    end;
extract_object([$" | T], Acc, true, N, Separator_end, Separator_begin) -> 
	case simple_echaped(Acc) of
	    true -> extract_object(T, [$" | Acc],  false, N, Separator_end, Separator_begin);
	    false -> extract_object(T, [$" | Acc], true, N, Separator_end, Separator_begin)
    end;
extract_object([C | T], Acc, true, N, Separator_end, Separator_begin) ->
    extract_object(T, [C | Acc], true, N, Separator_end, Separator_begin);
extract_object([C | T], Acc, false, N_separator, Separator_end, Separator_begin) when C =:= Separator_begin->
    extract_object(T, [C | Acc], false, N_separator + 1, Separator_end, Separator_begin);
extract_object([C | T], Acc, false, 1, Separator_end, _) when C =:= Separator_end->
    T_striped = string:strip(T, left),
    case T_striped of
	[$, | T2 ] -> {lists:reverse([C|Acc]), T2};
	[$} | T2 ] -> {lists:reverse([C|Acc]), T2};
	_ -> throw("parse error : missing ,")
    end;
extract_object([C | T], Acc, false, N_separator, Separator_end, Separator_begin) when C =:= Separator_end->
    extract_object(T, [C | Acc], false, N_separator - 1, Separator_end, Separator_begin);
extract_object([C | T], Acc, In_string, N, Separator_end, Separator_begin) ->
    extract_object(T, [C | Acc], In_string, N, Separator_end, Separator_begin).


%%renvoi la liste des valeurs extraites
-spec extract_list_value(string()) -> [string()].
extract_list_value(String) ->
    [_ | T] = String,
    extract_list_value(T, {"", []}, false, 0, 1).

extract_list_value([$" | T], {Acc_value, Acc_list}, false, N_accolade, N) ->
    case simple_echaped(Acc_value) of 
	true -> extract_list_value(T, {[$" | Acc_value],Acc_list}, true, N_accolade, N);
	false -> throw("parse error : too much \\")
    end;
extract_list_value([$" | T], {Acc_value, Acc_list}, true, N_accolade, N) -> 
    case simple_echaped(Acc_value) of
	true -> Acc = { [$" | Acc_value], Acc_list},
		extract_list_value(T,Acc , false, N_accolade, N);
	false -> Acc = { [$" | Acc_value], Acc_list},
		 extract_list_value(T,Acc, true, N_accolade, N)
    end;
extract_list_value([C | _], Acc, false, _, 1) when (C =:= $]) ->
    {Acc_value, Acc_list} = Acc,
    lists:reverse([string:strip(lists:reverse(Acc_value), both)  | Acc_list]);
extract_list_value([C | T], Acc, false, N_accolade, N_bracket) when C =:= $[->
    {Acc_value, Acc_list} = Acc,
    Acc2 = {[C | Acc_value], Acc_list},
    extract_list_value(T, Acc2, false, N_accolade, N_bracket + 1);
extract_list_value([C | T],Acc , false, N_accolade, N_bracket) when (C =:= $]) ->
    {Acc_value, Acc_list} = Acc,
    Acc2 = {[C | Acc_value], Acc_list},
    extract_list_value(T, Acc2, false, N_accolade, N_bracket - 1);

extract_list_value([C | T], Acc, false, N_accolade, N_bracket) when C =:= ${->
    {Acc_value, Acc_list} = Acc,
    Acc2 = {[C | Acc_value], Acc_list},
    extract_list_value(T, Acc2, false, N_accolade + 1, N_bracket);
extract_list_value([C | T],Acc , false, N_accolade, N_bracket) when (C =:= $}) ->
    {Acc_value, Acc_list} = Acc,
    Acc2 = {[C | Acc_value], Acc_list},
    extract_list_value(T, Acc2, false, N_accolade - 1, N_bracket);

extract_list_value([C | T], Acc, false, 0, 1) when (C =:= $,) ->
    {Acc_value, Acc_list} = Acc,
    Value = string:strip(lists:reverse(Acc_value), both),
    Acc2 = { "", [ Value | Acc_list]},
    extract_list_value(T,Acc2, false, 0, 1);
extract_list_value([C | T], Acc, In_string, N_accolade, N_bracklet) ->
    {Acc_value, Acc_list} = Acc,
    Acc2 = {[C | Acc_value], Acc_list},
    extract_list_value(T, Acc2, In_string, N_accolade, N_bracklet).


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
	[$[ | _] ->  
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
convert_value_aux(Value, {object, F}) ->
    {ok, F(Value)};
convert_value_aux(Value, float) ->
    coers:to_float(Value).

%% -spec parse_object(string(), #{string() => json_type()}, #{string() => any()}) -> [{string(), any()}].
parse_object(String, Field_info) ->
    parse_object(String, Field_info, maps:new()).

parse_object([], _, Acc) -> Acc;

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
			       Value2 = maps:put(Field_name, Value, Acc),
			       parse_object(T4, Field_info, Value2);
			   {ok, {pure_list, Type}} ->
			       {Value, T4} = extract_object(T3, $], $[),
			       parse_object(T4, Field_info, maps:put(Field_name, convert_value(Value, {pure_list, Type}), Acc));
			   {ok, {impure_list, Type}} ->
			       {Value, T4} = extract_object(T3, $], $[),
			       parse_object(T4, Field_info, maps:put(Field_name, convert_value(Value, {impure_list, Type}), Acc));
			   {ok, {object, F}} ->
			       {Value, T4} = extract_object(T3, $}, ${),
			       parse_object(T4, Field_info, maps:put(Field_name, F(Value), Acc));
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
    Result= parse_object(String, Map),
    map_to_record(Result, Fields, Record_name).
    

gen_decoder(Type_list, Fields, Record_name) ->
    fun(String) ->
	    json_to_record(String, Type_list, Record_name, Fields)
    end.

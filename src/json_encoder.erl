 %% @author Arthur d'Azémar
 %% @doc function for convert erlang value into json string and json string into erlang value

-module(json_encoder).
-export([gen_encoder/2]).
-include("json_converter.hrl").
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-vsn(1).

-spec value_to_json(json_type(), any()) -> string().
value_to_json (int, Val)  ->
    integer_to_list(Val);
value_to_json (atom, Val) ->
    atom_to_list(Val);
value_to_json (float, Val) ->
    float_to_list(Val);
value_to_json ({object, F}, Val) ->
    F(Val);
value_to_json (List_type, Val) ->
    list_to_json(List_type, Val).

-spec list_to_json(json_type(), [any()]) -> string().
list_to_json({impure_list, Type_list}, List) ->
    P = fun ({Type, A}, Acc) -> Acc ++ "," ++ value_to_json(Type, A) end,
    L = lists:zip(Type_list, List),
    [{Type, A}| T] = L,
    Acc0 =  "[" ++ value_to_json(Type, A),
    lists:foldl(P,Acc0, T) ++ "]";
list_to_json({pure_list, Type}, List) ->
    P = fun (A, Acc) -> Acc ++ "," ++ value_to_json(Type, A) end,
    [H | T] = List,
    Acc0 =  "[" ++ value_to_json(Type, H),
    lists:foldl(P,Acc0, T) ++ "]".


%% entoure une chaine de guillemet
quote(S) -> [$"] ++ S ++ [$"].

-spec json_field(string(), string()) -> string().
json_field(Field, Val) ->
   quote(Field) ++ ":" ++ Val.

%% creer un champ json ,
%% Field est le nom du champ,
%% Type son type
%% Val sa valeur
-spec json_field(string(), json_type(), string()) -> string().
json_field(Field, string, Val) ->
    json_field(quote(Field), Val);
pjson_field(Field, Type, Val) ->
    json_field(Field, value_to_json(Type, Val)).

%% Transforme un record en chaine json
%% Fields est la liste des nom des champs du record (string)
%% Type_fields est la liste du type de chaque champ du record
-spec record_to_json([string()], [json_type()], tuple()) -> string().
record_to_json(Fields, Type_field, Record) ->
    [_| T] = tuple_to_list(Record),
    List = lists:zip3(Fields, Type_field, T),
    "{" ++ record_to_json_aux(List) ++ "}".

-spec record_to_json_aux( [ {string(), json_type(), [any()] } ] ) -> string().
record_to_json_aux(List) ->
    P = fun ({Field, Type, Val}, Acc) ->
		Acc ++ "," ++ json_field(Field, Type, Val)
	end,
    [{Field, Type, Val}|T] = List,
    
    json_field(Field, Type, Val) ++
	lists:foldl(P, "", T).

%% renvoi une fonction permetant de transformé le record spécifié en argument en une chaine json
%% @doc generate a fonction which convert erlang value into a json string
-spec gen_encoder( [json_type()], [string()]) -> fun ((tuple()) -> nonempty_string()).
gen_encoder (Type_list, Fields) ->
    fun (Record) ->
	    record_to_json(Fields, Type_list, Record)
    end.

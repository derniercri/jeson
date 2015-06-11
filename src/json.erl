-module(json).
-include_lib("eunit/include/eunit.hrl").
-export([gen_decoder/3, gen_encoder/2]).
-include("json_type.hrl").
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-vsn(1).



gen_decoder_test() ->
    String = "{\\\"c1\\\":[1,2,3], \\\"c2\\\":[\\\"toto\\\", 4], \\\"c3\\\":{\\\"c1\\\":123}}",
    F1 = gen_decoder([int], ["c1"], record1),
    F2 = gen_decoder([{pure_list, int}, {impure_list, [string, int]},{object, F1}],
			  ["c1","c2","c3"],
			  record2),
    ?assertEqual(F2(String), {record2, [1,2,3], ["toto", 1], {record1, 123}}).
	

%% @doc generate a function which convert a json string into an erlang record. 
%% @spec gen_decoder(Type_list:: [json_type()], Fields::[string()], Record_name::atom()) -> fun ((string()) -> tuple())
gen_decoder(Type_list, Field_names, Record_name) -> 
    json_decoder:gen(Type_list, Field_names, Record_name).



%% @doc generate a fonction which convert erlang value into a json string
%% @spec gen_decoder(Type_list:: [json_type()], Fields::[string()]) -> fun (tuple() -> (string()))
gen_encoder(Type_list, Field_names) ->
    json_encoder:gen(Type_list, Field_names).

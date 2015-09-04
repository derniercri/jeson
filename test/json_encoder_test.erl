-module(json_encoder_test).
-include_lib("eunit/include/eunit.hrl").
-include("json_type.hrl").
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-vsn(1).

%%json_encoder:gen

test_aux(Field_names, Type_list, Record, String) ->
    F = json_encoder:gen(Type_list, Field_names),
    ?assertEqual(F(Record), String).

gen_base_test() ->
    test_aux(["c1", "c2", "c3"],
	     [int, float, atom],

	     {toto, 1, 1.2, true},
	     "{\"c1\":1,\"c2\":1.19999999999999995559e+00,\"c3\":true}").

gen_string_test() ->
        test_aux(["c1", "c2"],
	     [string, string],
	     {toto, "toto", "titi"},
	     "{\"c1\":\"toto\",\"c2\":\"titi\"}").

gen_nested_string_test() ->
    test_aux(["c1", "c2"],
	     [string, string],
	     {toto, "toto", "titi\"tutu\"tata"},
	     "{\"c1\":\"toto\",\"c2\":\"titi\\\"tutu\\\"tata\"}").

gen_list_test() ->
    test_aux(["c1", "c2"],
	     [int, {pure_list, int}],
	     {toto, 1, [2,3,4,5]},
	     "{\"c1\":1,\"c2\":[2,3,4,5]}").
    
gen_list_string_test() ->
    test_aux(["c1", "c2"],
	     [int, {pure_list, string}],
	     {toto, 1, ["toto", "titi\"tutu\"tata"]},
	     "{\"c1\":1,\"c2\":[\"toto\",\"titi\\\"tutu\\\"tata\"]}").

gen_object_test() ->
    Type_list = [{impure_list, [int, string]}, string],
    Field_names = ["c1", "c2"],
    F = json_encoder:gen(Type_list, Field_names),
    test_aux(["c1", "c2"],
	     [int, {object, F}],
	     {toto, 1, {toto, [1, "toto"], "[,]{a:t}"}},
	     "{\"c1\":1,\"c2\":{\"c1\":[1,\"toto\"],\"c2\":\"[,]{a:t}\"}}").

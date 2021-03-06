-module(json_decoder_test).
-include_lib("eunit/include/eunit.hrl").
-include("json_type.hrl").
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-vsn(1).

%%json_decoder:extract_list_value()

extract_list_value_empty_test() ->
    ?assertEqual(json_decoder:extract_list_value("[]"), []).

extract_list_value_int_test() ->
    ?assertEqual(json_decoder:extract_list_value("[1,2,3]"), ["1","2","3"]).

extract_list_value_string_test() ->
    ?assertEqual(json_decoder:extract_list_value("[\"toto\", \"titi\"]"), ["\"toto\"","\"titi\""]).

extract_list_value_object_test() ->
    String = "[{l1:1, l2:2}, {l1:3}]",
    Result = ["{l1:1, l2:2}", "{l1:3}"],
    ?assertEqual(json_decoder:extract_list_value(String), Result).

extract_list_value_string_with_list_test() ->
    String = "[\"[1,2,3]\", \"[4,5,6,]]]\"]",
    Result = ["\"[1,2,3]\"", "\"[4,5,6,]]]\""],
    ?assertEqual(json_decoder:extract_list_value(String), Result).

extract_list_value_object_with_list_test() ->
    String = "[{l1:[1,2,3], l2:[4,5,6]}, {l1:[]}]",
    Result = ["{l1:[1,2,3], l2:[4,5,6]}", "{l1:[]}"],
    ?assertEqual(json_decoder:extract_list_value(String), Result).



%%json_decoder:extract_object()

extract_object_empty_list_test() ->
    String = ":[],end_of_object],],],",
    Result = "[]",
    ?assertEqual(json_decoder:extract_object(String, $], $[), {Result, "end_of_object],],],"}).

extract_object_list_int_test() ->
    String = ":[1,2,3],end_of_object],],],",
    Result = "[1,2,3]",
    ?assertEqual(json_decoder:extract_object(String, $], $[), {Result, "end_of_object],],],"}).

extract_object_list_string_test() ->
    String = ":[\"toto],\", \"titi\"],end_of_object],],],",
    Result = "[\"toto],\", \"titi\"]",
    ?assertEqual(json_decoder:extract_object(String, $], $[), {Result, "end_of_object],],],"}).

extract_object_object_with_list_test() ->
    String = ":{l1:[1,2,3], l2:[4,5], l3:[]},end_of_object],],],",
    Result = "{l1:[1,2,3], l2:[4,5], l3:[]}",
    ?assertEqual(json_decoder:extract_object(String, $}, ${), {Result, "end_of_object],],],"}).

%%reduce_escape
reduce_escape_1_test() ->
    ?assertEqual(json_decoder:reduce_escape("\\\"toto\\\\titi\\\\\\\""), "\"toto\\\\titi\\\\\"").

reduce_escape_2_test() ->
    ?assertEqual(json_decoder:reduce_escape("\\\"toto\\\\titi\\\\\\\"qsd"), "\"toto\\\\titi\\\\\"qsd").

%%gen_decoder

gen_decoder_test() ->
    String = "{\"c1\":[1,2,3], \"c2\":[\"toto\", 4], \"c3\":{\"c1\":123}}",
    F1 = json_decoder:gen([int], ["c1"], record1),
    F2 = json_decoder:gen([{pure_list, int}, {impure_list, [string, int]},{object, F1}],
			  ["c1","c2","c3"],
			  record2),
    ?assertEqual(F2(String), {record2, [1,2,3], ["toto", 4], {record1, 123}}).

%% gen_decoder_extern_test() ->
%%     F = json_decoder:gen([int, int, string, string], ["userId", "id", "title", "body"], toto),
%%     inets:start(),
%%     {_ , {_,_,Body}} = httpc:request("http://jsonplaceholder.typicode.com/posts/3"),
%%     io:format("------------------~n~p~n", [Body]),
%%     ?assertEqual(F(Body),
%% {toto, 1, 3, "ea molestias quasi exercitationem repellat qui ipsa sit aut", "et iusto sed quo iure\\nvoluptatem occaecati omnis eligendi aut ad\\nvoluptatem doloribus vel accusantium quis pariatur\\nmolestiae porro eius odio et labore et velit aut"}),
%%     inets:start().

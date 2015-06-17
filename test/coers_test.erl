%% @copyright 2015 Derniercri
%% @version 1.0.0
%% @title `coers`, small coersion library

-module(coers_test).
-vsn(1).
-author(["Arthur d'Azémar", "Xavier van De Woestyne"]).
-include_lib("eunit/include/eunit.hrl").
-include("coers_type.hrl").

is_string_test() ->
    ?assert(coers:is_string("")),
    ?assert(coers:is_string("Hello")),
    ?assert(coers:is_string([32, 33, 34])),
    ?assertNot(coers:is_string(45)),
    ?assertNot(coers:is_string([0,1])),
    ?assertNot(coers:is_string(atom)),
    ?assertNot(coers:is_string(45.0)).
    

to_string_test() ->
    ?assert(coers:to_string("coers") == "coers"),
    ?assert(coers:to_string(coers) == "coers"),
    ?assert(coers:to_string([]) == ""),
    ?assert(coers:to_string(45) == "45"),
    ?assert(coers:to_string(45.0) == "45.0").


of_string_atomic_test() ->
    {ok, Atom} = coers:of_string("an_atom"),
    ?assert(Atom == an_atom).

of_string_list_test() ->
    {ok, List} = coers:of_string("[1,2,3,4]"),
    ?assert(List == [1,2,3,4]).

of_string_numeric_test() ->
    {ok, Tuple} = coers:of_string("{45, 45.3}"),
    ?assert(Tuple == {45, 45.3}).
    
of_string_error_test() ->
    {Flag, _, _} = coers:of_string("{45"),
    ?assert(Flag == error).
    
to_int_test() ->
    ?assert(coers:to_int(45) == {ok, 45}),
    ?assert(coers:to_int("-45") == {ok, -45}),
    ?assert(coers:to_int(45.0) == {ok, 45}),
    ?assert(coers:to_int('45') == {ok, 45}),
    ?assert(coers:to_int("test") == {error, 0}),    
    ?assert(coers:to_int('test') == {error, 0}).

    
to_float_test() ->
    ?assert(coers:to_float(45.0) == {ok, 45.0}),
    ?assert(coers:to_float("-45.77") == {ok, -45.77}),
    ?assert(coers:to_float(45) == {ok, 45.0}),
    ?assert(coers:to_float('45.0') == {ok, 45.0}),
    ?assert(coers:to_float("test") == {error, 0.0}),    
    ?assert(coers:to_float('test') == {error, 0.0}).

to_atom_test() ->
    ?assert(coers:to_atom(me) == {ok, me}),
    ?assert(coers:to_atom("me") == {ok, me}),
    ?assert(coers:to_atom(45.87) == {ok, '45.87'}).

to_bool_test() ->
    ?assert(coers:to_bool(true) == {ok, true}),
    ?assert(coers:to_bool(me) == {ok, true}),
    ?assert(coers:to_bool(false) == {ok, false}),
    ?assert(coers:to_bool("true") == {ok, true}),
    ?assert(coers:to_bool("false") == {ok, false}),
    ?assert(coers:to_bool("hello") == {error, true}),
    ?assert(coers:to_bool(1) == {ok, true}),
    ?assert(coers:to_bool(0) == {ok, false}),
    ?assert(coers:to_bool(45) == {error, true}).

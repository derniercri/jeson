-module(run_test).
-export([run/0]).


test(Module) ->
    case eunit:test(Module) of
	ok -> ok;
	{error, String} ->
	    io:format("~s~n", [String]),
	    throw(error)
    end.

run() ->
    {ok, [Val]} = init:get_argument(a),
    Module_list = lists:map(fun list_to_atom/1, Val),
    lists:foreach(fun test/1, Module_list),
    init:stop().



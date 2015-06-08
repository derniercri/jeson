%% Define data structures of Coers module

%% @doc return type. Each result will be wrapped into {ok|error, term}
-type wrapped_result() :: 
        {ok, term()} 
      | {error, module(), term()}
      | {error, term()}.


%% @doc primitive type 
-type primitive_for_int() :: 
        integer()
      | string()
      | float()
      | atom().

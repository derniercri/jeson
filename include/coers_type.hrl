%% Define data structures of Coers module

%% Returnable result
-type wrapped_result() :: 
        {ok, term()} 
      | {error, module(), term()}
      | {error, term()}.


%% Low level type 
-type primitive_for_int() :: 
        integer()
      | string()
      | float()
      | atom().

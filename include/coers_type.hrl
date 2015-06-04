%% Define data structures of Coers module
-type wrapped_result() :: 
        {ok, any()} 
      | {error, module(), term()}.

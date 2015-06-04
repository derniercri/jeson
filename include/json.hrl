-type json_type() ::
	'int'
      | 'atom'
      | 'float'
      | 'string'
      | {'object', fun ((tuple()) -> nonempty_string())}
      | {'pure_list', json_type()}
      | {'impure_list', [json_type()]}.

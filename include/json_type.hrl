-type json_type() ::
	'int'
      | 'atom'
      | 'float'
      | 'string'
      | {'object', fun ((tuple()) -> nonempty_string()) | fun ((string()) -> tuple() )}
      | {'pure_list', json_type()}
      | {'impure_list', [json_type()]}.

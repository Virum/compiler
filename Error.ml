let format = Printf.sprintf

let to_string = function
  | `Syntax_error (line, column) ->
       format "%d:%d: Syntax error" line column
  | `Bad_escape_sequence ((line, column), escape_sequence) ->
       format "%d:%d: Bad escape sequence %s" line column escape_sequence

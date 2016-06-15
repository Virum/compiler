let format = Printf.sprintf

let location lexbuf =
  let position = Lexing.(lexbuf.lex_curr_p) in
  let line = Lexing.(position.pos_lnum)
  and column = Lexing.(position.pos_cnum - position.pos_bol) in
  line, column

module Term = struct
  let parse_lexbuf lexbuf =
    match Grammar.parse_term Lexer.read lexbuf with
      | value -> Ok value
      | exception Grammar.Error ->
          Error (`Syntax_error (location lexbuf))
      | exception Lexer.Error (`Bad_escape_sequence sequence) ->
          Error (`Bad_escape_sequence (location lexbuf, sequence))

  let parse source = parse_lexbuf (Lexing.from_string source)

  let parse_channel channel = parse_lexbuf (Lexing.from_channel channel)
end

module Items = struct
  let parse_lexbuf lexbuf =
    match Grammar.parse_items Lexer.read lexbuf with
      | value -> Ok value
      | exception Grammar.Error ->
          Error (`Syntax_error (location lexbuf))
      | exception Lexer.Error (`Bad_escape_sequence sequence) ->
          Error (`Bad_escape_sequence (location lexbuf, sequence))

  let parse source = parse_lexbuf (Lexing.from_string source)

  let parse_channel channel = parse_lexbuf (Lexing.from_channel channel)
end

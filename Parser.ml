let format = Printf.sprintf

let location lexbuf =
  let position = Lexing.(lexbuf.lex_curr_p) in
  let line = Lexing.(position.pos_lnum)
  and column = Lexing.(position.pos_cnum - position.pos_bol) in
  format "%d:%d:" line column

module Term = struct
  let parse source =
    let lexbuf = Lexing.from_string source in
    match Grammar.parse_term Lexer.read lexbuf with
      | value -> Ok value
      | exception Grammar.Error ->
(*           print_endline (format "%s Syntax error" (location lexbuf)); *)
          Error (format "%s Syntax error" (location lexbuf))
      | exception Lexer.Error message ->
          Error (format "%s %s" (location lexbuf) message)
end

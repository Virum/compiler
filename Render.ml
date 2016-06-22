let fprintf = Format.fprintf

let parenthesise f =
  fprintf f "(%a)"

let make_infix_format ~binding ~format_naive =
  let rec format ?(precedence=0) f node =
    let inner_precedence, left, right = match binding node with
      | `Left  n -> n, n, n + 1
      | `Right n -> n, n + 1, n
      | `None  n -> n, n, n
    in
    let format_rec f =
      format_naive f
        (format ~precedence:left)
        (format ~precedence:right)
    in
    if precedence > inner_precedence
      then parenthesise f format_rec node
      else format_rec f node
  in format

let escape_char char =
  let code = Char.code char in
  assert (code >= 0 && code < 128);
  match char with
  | '"' -> "\\" ^ "\""
  | '\b' -> "\\" ^ "b"
  | '\n' -> "\\" ^ "n"
  | '\r' -> "\\" ^ "r"
  | '\t' -> "\\" ^ "t"
  | _ when code < 32 || code = 127 -> Printf.sprintf "\\u%.4x" code
  | _ -> Printf.sprintf "%c" char

let escape_string ?(start="\"") ?(finish="\"") string =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer start;
  String.iter (fun char -> escape_char char |> Buffer.add_string buffer) string;
  Buffer.add_string buffer finish;
  Buffer.to_bytes buffer

let matches re string =
  let result = Str.(string_match (regexp (re ^ "$"))) string 0 in
  result && Str.match_end () = String.length string

let is_valid_identifier = matches "[$_a-zA-Z][$_a-zA-Z0-9]*"

let format_string = Format.pp_print_string

let text string f () = fprintf f string

let format_list
  ?(start=text "")
  ?(sep=text "")
  ?(trailer=text "")
  format_item f item_list
=
  if item_list = [] then () else
  let format_list = Format.pp_print_list ~pp_sep:sep format_item in
  fprintf f "%a%a%a"
    start ()
    format_list item_list
    trailer ()

let format_comma_separated format_item =
  format_list ~sep:(text ",@ ") format_item


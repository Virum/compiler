open Core_kernel.Std

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

let escape_char = function
  | '"' -> "\\" ^ "\""
  | '\b' -> "\\" ^ "b"
  | '\n' -> "\\" ^ "n"
  | '\r' -> "\\" ^ "r"
  | '\t' -> "\\" ^ "t"
  | char when Char.is_print char -> Char.to_string char
  | char -> sprintf "\\u%.4x" (Char.to_int char)

let escape_string ?(start="\"") ?(finish="\"") string =
  let fold rope char = Rope.(rope ^ of_string (escape_char char)) in
  let rope = String.fold string ~init:(Rope.of_string start) ~f:fold in
  Rope.(rope ^ of_string finish |> to_string)

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

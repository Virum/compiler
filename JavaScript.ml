let fprintf = Format.fprintf

module Operator = struct
  type t =
    | Plus | Minus | Times | Divide | Equal | NotEqual
    | Less | Greater | LessOrEqual | GreaterOrEqual
    | And | Or

  let to_string = function
    | Plus           -> "+"
    | Minus          -> "-"
    | Times          -> "*"
    | Divide         -> "/"
    | Equal          -> "=="
    | NotEqual       -> "!="
    | Less           -> "<"
    | Greater        -> ">"
    | LessOrEqual    -> "<="
    | GreaterOrEqual -> ">="
    | And            -> "&&"
    | Or             -> "||"

  let precedence = function
    | Plus | Minus -> 13
    | Times | Divide -> 14
    | Equal | NotEqual -> 10
    | Less | Greater | LessOrEqual | GreaterOrEqual -> 11
    | And -> 6
    | Or -> 5
end

type parameters = string list

type term =
  | Identifier of string
  | String of string
  | Number of float
  | Function of string option * parameters * statement list
  | Call of term * arguments
  | Member of term * term
  | Infix of term * Operator.t * term
  | Object of (string * term) list

and arguments = term list

and statement =
  | Return of term
  | Term of term
  | IfElse of term * statement list * statement list
  | Var of string * term

let precedence = function
  | Number _ | Identifier _ | String _ | Object _ -> 999
  | Infix (_, operator, _) -> Operator.precedence operator
  | Call _ -> 17
  | Member _ -> 18
  | Function _ -> 0

let format_list ?(sep=", ") ?(trailer="") format_item f list =
  let pp_sep f () = fprintf f "%s" sep in
  let format_list = Format.pp_print_list ~pp_sep format_item in
  let trailer = if list = [] then "" else trailer in
  fprintf f "%a%s" format_list list trailer

let text string f () = fprintf f string

let format_list_2
  ?(start=text "")
  ?(sep=fun f () -> fprintf f ", ")
  ?(trailer=fun f () -> fprintf f "")
  format_item f item_list
=
  if item_list = [] then () else
  let format_list = Format.pp_print_list ~pp_sep:sep format_item in
  fprintf f "%a%a%a"
    start ()
    format_list item_list
    trailer ()

let matches re string =
  let result = Str.(string_match (regexp (re ^ "$"))) string 0 in
  result && Str.match_end () = String.length string

let is_valid_identifier = matches "[$_a-zA-Z][$_a-zA-Z0-9]*"

let escape_char char =
  let code = Char.code char in
  assert (code >= 0 && code < 128);
  match char with
  | '"' -> "\\" ^ "\""
  | '\b' -> "\\" ^ "b"
  | '\n' -> "\\" ^ "n"
  | '\r' -> "\\" ^ "r"
  | _ when code < 32 || code = 127 -> Printf.sprintf "\\u%.4x" code
  | _ -> Printf.sprintf "%c" char

let escape_string ?(start="\"") ?(finish="\"") string =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer start;
  String.iter (fun char -> escape_char char |> Buffer.add_string buffer) string;
  Buffer.add_string buffer finish;
  Buffer.to_bytes buffer

let default option default =
  match option with
  | None -> default
  | Some value -> value

let format_string = Format.pp_print_string

let box f tail = if tail then text "" f () else text "@[<v 2>" f ()

let rec format_statements f statements =
(*   format_list ~sep:"; " ~trailer:";" format_statement f statements *)
  format_list_2 ~start:(text "@,") ~sep:(text ";@ ") ~trailer:(text ";@;<0 -2>")
    (format_statement false) f statements

and format_pair f (name, term) =
  fprintf f "%s: %a" name format term

and format_pairs f pairs =
  format_list_2 ~start:(text "@,") ~sep:(text ",@ ") ~trailer:(text "@;<0 -2>")
    format_pair f pairs

and format_statement tail f = function
  | Term term -> format f term
  | IfElse (condition, consequence, [IfElse _ as nested_if_else]) -> fprintf f
      "%aif (%a) {%a@]@[<v 2>} else %a"
        box tail format condition
        format_statements consequence
        (format_statement true) nested_if_else
  | IfElse (condition, consequence, alternative) -> fprintf f
      "%aif (%a) {%a@]@[<v 2>} else {%a}@]"
        box tail
        format condition
        format_statements consequence
        format_statements alternative
  | Return term -> fprintf f
      "return %a" format term
  | Var (name, term) -> fprintf f
      "var %s = %a" name format term

and format f =
  format_precedence (-1) f

and format_precedence outer_precedence f ast =
  let inner_precedence = precedence ast in
  let format_rec = format_precedence inner_precedence in
  let format_ast f = function
    | Identifier id -> fprintf f
        "%s" id
    | Number float -> fprintf f
        "%F" float
    | String string -> fprintf f
        "%s" (escape_string string)
    | Infix (left, operator, right) -> fprintf f
        "%a %s %a" format_rec left
                   (Operator.to_string operator)
                   format_rec right
    | Call (callee, arguments) -> fprintf f
        "%a(%a)" format_rec callee (format_list (format_precedence (-1))) arguments
    | Member (value, String string) when is_valid_identifier string -> fprintf f
        "%a.%s" format_rec value string
    | Member (value, member) -> fprintf f
        "%a[%a]" format_rec value format_rec member
    | Function (name, parameters, body) -> fprintf f
        "@[<v 2>function %s(%a) {%a}@]" (default name "")
                                        (format_list format_string) parameters
                                        format_statements body
    | Object pairs -> fprintf f
        "@[<hv 2>{%a}@]" format_pairs pairs
  in
    if outer_precedence > inner_precedence
      then fprintf f "(%a)" format_ast ast
      else format_ast f ast



let a, b, c, d = Identifier "a", Identifier "b", Identifier "c", Identifier "d"


let to_string ast = Format.asprintf "%a" format ast

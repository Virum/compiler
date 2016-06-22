let fprintf = Format.fprintf
module Option = Core_kernel.Option
let text, format_list, format_string = Render.(text, format_list, format_string)

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

  let binding = function
    | Plus | Minus -> `None 13
    | Times | Divide -> `None 14
    | Equal | NotEqual -> `None 10
    | Less | Greater | LessOrEqual | GreaterOrEqual -> `None 11
    | And -> `None 6
    | Or -> `None 5
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

let binding = function
  | Number _ | Identifier _ | String _ | Object _ -> `None 999
  | Infix (_, operator, _) -> Operator.binding operator
  | Call _ -> `None 17
  | Member _ -> `None 18
  | Function _ -> `None 0

let box f tail = if tail then text "" f () else text "@[<v 2>" f ()

let rec format_statements f statements =
  format_list ~start:(text "@,") ~sep:(text "@ ") ~trailer:(text "@;<0 -2>")
    (format_statement false) f statements

and format_pair f (name, term) =
  fprintf f "%s: %a" name format_term term

and format_pairs f pairs =
  format_list ~start:(text "@,") ~sep:(text ",@ ") ~trailer:(text "@;<0 -2>")
    format_pair f pairs

and format_statement tail f = function
  | Term term -> fprintf f
     "@[<v 2>%a;@]" format_term term
  | IfElse (condition, consequence, [IfElse _ as nested_if_else]) -> fprintf f
      "%aif (%a) {%a@]@[<v 2>} else %a"
        box tail
        format_term condition
        format_statements consequence
        (format_statement true) nested_if_else
  | IfElse (condition, consequence, alternative) -> fprintf f
      "%aif (%a) {%a@]@[<v 2>} else {%a}"
        box tail
        format_term condition
        format_statements consequence
        format_statements alternative
  | Return term -> fprintf f
      "@[<v 2>return %a;@]" format_term term
  | Var (name, term) -> fprintf f
      "@[<v 2>var %s = %a;@]" name format_term term

and format_term_naive f format_left format_right = function
  | Identifier id -> fprintf f
      "%s" id
  | Number float -> fprintf f
      "%F" float
  | String string -> fprintf f
      "%s" (Render.escape_string string)
  | Infix (left, operator, right) -> fprintf f
      "%a %s %a" format_left left
                 (Operator.to_string operator)
                 format_right right
  | Call (callee, arguments) -> fprintf f
      "%a(@[<hv>%a@])" format_left callee
                       (Render.format_comma_separated format_term) arguments
  | Member (value, String string)
    when Render.is_valid_identifier string -> fprintf f
      "%a.%s" format_left value string
  | Member (value, member) -> fprintf f
      "%a[%a]" format_left value format_term member
  | Function (name, parameters, body) -> fprintf f
      "function %s(@[<hv>%a@]) {%a}"
        (Option.value ~default:"" name)
        (Render.format_comma_separated format_string) parameters
        format_statements body
  | Object pairs -> fprintf f
      "@[<hv 2>{%a}@]" format_pairs pairs

and format_term f =
  Render.make_infix_format
    ~binding ~format_naive:format_term_naive ~precedence:0 f

let a, b, c, d = Identifier "a", Identifier "b", Identifier "c", Identifier "d"

let to_string = Format.asprintf "%a" (format_statement false)
let print statement = print_endline (to_string statement)

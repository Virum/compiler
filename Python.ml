let fprintf = Format.fprintf
let text, format_list, format_string = Render.(text, format_list, format_string)
let format_comma_separated = Render.format_comma_separated

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
    | And            -> "and"
    | Or             -> "or"

  let binding = function
    (* Lambda -> `Left 1 *)
    | Plus | Minus -> `Left 11
    | Times | Divide -> `Left 12
    | Equal | NotEqual
    | Less | Greater | LessOrEqual | GreaterOrEqual -> `Left 6
    | And -> `Left 4
    | Or -> `Left 3
end

type parameters = string list

type term =
  | Identifier of string
  | String of string
  | Number of float
  | Call of term * arguments
  | Member of term * string
  | Infix of term * Operator.t * term
  | Dict of (term * term) list
  | Tuple of term list

and arguments = term list

and block = statement list

and statement =
  | Pass
  | Return of term
  | Term of term
  | If of term * block * if_tail
  | Assignment of term * term
  | Def of string * parameters * statement list

and if_tail =
  | Else of block
  | Elif of term * block * if_tail

let binding = function
  | Number _ | Identifier _ | String _ | Dict _ | Tuple _ -> `None 999
  | Infix (_, operator, _) -> Operator.binding operator
  | Call _
  | Member _ -> `Left 15

let rec format_pair f (left, right) =
  fprintf f "%a: %a" format_term left format_term right

and format_term_naive f format_left format_right = function
  | Identifier id -> fprintf f
      "%s" id
  | Number float -> fprintf f
      "%F" float
  | String string -> fprintf f
      "%s" (Render.escape_string string)
  | Infix (left, op, right) -> fprintf f
      "%a %s %a" format_left left (Operator.to_string op) format_right right
  | Call (callee, arguments) -> fprintf f
      "%a(@[<hv>%a@])" format_left callee
                       (format_comma_separated format_term) arguments
  | Member (value, member) -> fprintf f
      "%a.%s" format_left value member
  | Dict pairs -> fprintf f
      "@[<hv 4>{@,%a@;<0 -4>}@]" (format_comma_separated format_pair) pairs
  | Tuple [singleton] -> fprintf f
      "(%a,)" format_term singleton
  | Tuple items -> fprintf f
      "@[<hv 4>(@,%a@;<0 -4>)@]" (format_comma_separated format_term) items

and format_term f =
  Render.make_infix_format
    ~binding ~format_naive:format_term_naive ~precedence:0 f

let rec format_statement f = function
  | Pass -> fprintf f
      "pass"
  | Term term -> fprintf f
      "%a" format_term term
  | Return term -> fprintf f
      "return %a" format_term term
  | Assignment (left, right) -> fprintf f
      "%a = %a" format_term left format_term right
  | Def (name, parameters, []) ->
      format_statement f (Def (name, parameters, [Pass]))
  | Def (name, parameters, body) -> fprintf f
      "@[<v 4>def %s(@[<hv>%a@]):@,%a@]"
        name
        (format_comma_separated format_string) parameters
        (format_list ~sep:(text "@,") format_statement) body
  | If (condition, body, tail) -> fprintf f
      "@[<v 4>if %a:@,%a@;<0 -4>@]%a"
        format_term condition
        format_block body
        format_tail tail

and format_tail f = function
  | Else block -> fprintf f
      "@[<v 4>else:@,%a@]" format_block block
  | Elif (condition, block, tail) -> fprintf f
      "@[<v 4>elif %a:@,%a@;<0 -4>@]%a"
        format_term condition
        format_block block
        format_tail tail

and format_block f statements =
  let statements = if statements = [] then [Pass] else statements in
  (format_list ~sep:(text "@,") format_statement f) statements

let a, b, c, d = Identifier "a", Identifier "b", Identifier "c", Identifier "d"

let to_string = Format.asprintf "%a" format_statement

let fprintf = Format.fprintf
let text, format_list, format_string = Render.(text, format_list, format_string)
let format_comma_separated = Render.format_comma_separated
module Float = Core_kernel.Std.Float
module String = Core_kernel.Std.String

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
  | Lambda of parameters * term
  | IfElse of {consequence: term; condition: term; alternative: term}

and arguments = term list

and block = statement list

and statement =
  | Pass
  | Include of string list
  | Return of term
  | Term of term
  | If of term * block * if_tail
  | Assignment of term * term
  | Def of string option * string * parameters * statement list
  | Class of string * string * statement list

and if_tail =
  | Else of block
  | Elif of term * block * if_tail

let binding = function
  | Number _ | Identifier _ | String _ | Dict _ | Tuple _ -> `None 999
  | Infix (_, operator, _) -> Operator.binding operator
  | Call _
  | Member _ -> `Left 15
  | IfElse _ -> `Left 2
  | Lambda _ -> `Left 1

let float_to_string float =
  match Float.classify float with
  | Float.Class.Infinite when Float.is_negative float -> "float('-inf')"
  | Float.Class.Infinite -> "float('inf')"
  | Float.Class.Nan -> "float('nan')"
  | _ ->
      let string = Float.to_string_round_trippable float in
      (* Dropping fractional part (like `10.`) is valid in Python,
         but it does not compose with method calls (`10..hex()` is invalid),
         so we generate `10.0` to safely be able to write `10.0.hex()`. *)
      if String.is_suffix string ~suffix:"." then string ^ "0" else string

let rec format_pair f (left, right) =
  fprintf f "%a: %a" format_term left format_term right

and format_term_naive f format_left format_right = function
  | Identifier id -> fprintf f
      "%s" id
  | Number float -> fprintf f
      "%s" (float_to_string float)
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
  | Lambda (arguments, term) -> fprintf f
      "lambda%a: %a"
        (format_list ~start:(text " ") ~sep:(text ", ") format_string) arguments
        format_right term
  | IfElse {consequence; condition; alternative} -> fprintf f
      "%a if %a else %a"
        format_left consequence
        format_term condition
        format_right alternative

and format_term f =
  Render.make_infix_format
    ~binding ~format_naive:format_term_naive ~precedence:0 f

let format_decorator f = function
  | Some decorator -> fprintf f
      "@%s@;<0 -4>" decorator
  | None -> ()

let rec format_statement f = function
  | Pass -> fprintf f
      "pass"
  | Term term -> fprintf f
      "%a" format_term term
  | Return term -> fprintf f
      "return %a" format_term term
  | Assignment (left, right) -> fprintf f
      "%a = %a" format_term left format_term right
  | Def (decorator, name, parameters, []) ->
      format_statement f (Def (decorator, name, parameters, [Pass]))
  | Def (decorator, name, parameters, body) -> fprintf f
      "@[<v 4>%adef %s(@[<hv>%a@]):@,%a@]"
        format_decorator decorator
        name
        (format_comma_separated format_string) parameters
        (format_list ~sep:(text "@,") format_statement) body
  | If (condition, body, tail) -> fprintf f
      "@[<v 4>if %a:@,%a@;<0 -4>@]%a"
        format_term condition
        format_block body
        format_tail tail
  | Class (name, superclass, body) -> fprintf f
      "@[<v 4>class %s(%s):@,%a@]"
        name
        superclass
        (format_list ~sep:(text "\n@,") format_statement) body
  | Include lines -> fprintf f
      "@[<v 0>%a@]" (format_list ~sep:(text "@,") format_string) lines

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
let print statement = print_endline (to_string statement)

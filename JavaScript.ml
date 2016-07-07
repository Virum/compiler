let fprintf = Format.fprintf
module Option = Core_kernel.Option
module Float = Core_kernel.Std.Float
module String = Core_kernel.Std.String
let text, format_list, format_string = Render.(text, format_list, format_string)
let format_comma_separated = Render.format_comma_separated

module Operator = struct
  type t =
    | Plus | Minus | Times | Divide | Equal | NotEqual
    | Less | Greater | LessOrEqual | GreaterOrEqual
    | And | Or
    | Assignment | Instanceof

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
    | Assignment     -> "="
    | Instanceof     -> "instanceof"

  let binding = function
    | Plus | Minus -> `Left 13
    | Times | Divide -> `Left 14
    | Equal | NotEqual -> `Left 10
    | Less | Greater | LessOrEqual | GreaterOrEqual | Instanceof -> `Left 11
    | And -> `Left 6
    | Or -> `Left 5
    | Assignment -> `Right 3

  module Prefix = struct
    type t = Not | Typeof | Delete | UnaryPlus | UnaryMinus | New

    let binding = function
      | Not | Typeof | Delete | UnaryPlus | UnaryMinus -> `Right 15
      | New -> `Right 18

    let to_string = function
      | Not        -> "!"
      | Typeof     -> "typeof "
      | Delete     -> "delete "
      | UnaryPlus  -> "+"
      | UnaryMinus -> "-"
      | New        -> "new "
  end
end

type parameters = string list

type term =
  | Identifier of string
  | String of string
  | Number of float
  | Function of string option * parameters * statement list
  | Call of term * arguments
  | NewCall of term * term list
  | Member of term * term
  | Infix of term * Operator.t * term
  | Prefix of Operator.Prefix.t * term
  | Object of (string * term) list
  | Array of term list

and arguments = term list

and statement =
  | Return of term
  | Include of string list
  | Term of term
  | IfElse of term * statement list * statement list
  | Var of string * term

let binding = function
  | Number _ | Identifier _ | String _ | Object _ | Array _ -> `None 999
  | Infix (_, operator, _) -> Operator.binding operator
  | Prefix (operator, _) -> Operator.Prefix.binding operator
  | Call _ -> `Left 17
  | NewCall _ -> `Left 18
  | Member _ -> `Left 18
  | Function _ -> `None 0

let number_to_string float =
  match Float.classify float with
  | Float.Class.Infinite when Float.is_negative float -> "-Infinity"
  | Float.Class.Infinite -> "Infinity"
  | Float.Class.Nan -> "NaN"
  | _ -> Float.to_string_round_trippable float |> String.rstrip ~drop:((=) '.')

let box f tail = if tail then text "" f () else text "@[<v 2>" f ()

let break f () = fprintf f "@<80>@ "

let rec format_statements f statements =
  (* "@<80>%s" forces a line break no matter what box we are in. *)
  let sep f () = fprintf f "@<80>%s" "" in
  format_list ~start:(text "@,") ~sep ~trailer:(text "@;<0 -2>")
    (format_statement false) f statements

and format_pair f (name, term) =
  if Render.is_valid_identifier name then
    fprintf f "%s: %a" name format_term term
  else
    fprintf f "%s: %a" (Render.escape_string name) format_term term

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
      "@[<hv 2>return %a;@]" format_term term
  | Var (name, term) -> fprintf f
      "@[<hv 2>var %s = %a;@]" name format_term term
  | Include lines -> fprintf f
      "%a" (format_list ~start:break ~sep:break format_string) lines

and format_term_naive f format_left format_right = function
  | Identifier id -> fprintf f
      "%s" id
  | Number float -> fprintf f
      "%s" (number_to_string float)
  | String string -> fprintf f
      "%s" (Render.escape_string string)
  | Infix (left, op, right) -> fprintf f
      "%a %s %a" format_left left (Operator.to_string op) format_right right
  | Prefix (op, term) -> fprintf f
      "%s%a" (Operator.Prefix.to_string op) format_right term
  | Call (callee, arguments) -> fprintf f
      "%a(@[<hv>%a@])" format_left callee
                       (format_comma_separated format_term) arguments
  | NewCall (callee, arguments) -> fprintf f
      "new %a(@[<hv>%a@])" format_left callee
                           (format_comma_separated format_term) arguments
  | Member (value, String string)
    when Render.is_valid_identifier string -> fprintf f
      "%a.%s" format_left value string
  | Member (value, member) -> fprintf f
      "%a[%a]" format_left value format_term member
  | Function (name, parameters, body) -> fprintf f
      "function %s(@[<hv>%a@]) {%a}"
        (Option.value ~default:"" name)
        (format_comma_separated format_string) parameters
        format_statements body
  | Object pairs -> fprintf f
      "@[<hv 2>{@,%a@;<0 -2>}@]" (format_comma_separated format_pair) pairs
  | Array items -> fprintf f
      "@[<hv 2>[@,%a@;<0 -2>]@]" (format_comma_separated format_term) items

and format_term f =
  Render.make_infix_format
    ~binding ~format_naive:format_term_naive ~precedence:0 f

let id x = Identifier x
let a, b, c, d = Identifier "a", Identifier "b", Identifier "c", Identifier "d"

let to_string = Format.asprintf "%a" (format_statement false)
let print statement = print_endline (to_string statement)

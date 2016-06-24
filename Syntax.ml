module List = Core_kernel.Std.List
open Sexplib.Std

type pattern = string
  [@@deriving sexp]

type operator =
  | Plus | Minus | Times | Divide | Equal | NotEqual
  | Less | Greater | LessOrEqual | GreaterOrEqual
  | And | Or
  [@@deriving sexp]

type term =
  | Identifier of string
  | String of string
  | Number of int
  | CaseFunction of case list
  | Switch of term * case list
  | IfElse of term * term * term
  | LetIn of pattern * term * term
  | Infix of term * operator * term
  | Call of term * term list
  [@@deriving sexp]

and case = pattern * term
  [@@deriving sexp]


type item =
  | Let of pattern * pattern list option * term
  | Do of term
  | Module of string * item list
  | Class of string * pattern list * item list
  [@@deriving sexp]



let a, b, c, d = Identifier "a", Identifier "b", Identifier "c", Identifier "d"

let dump t = print_string (Sexplib.Sexp.to_string_hum (sexp_of_term t))

let dump_item t = print_string (Sexplib.Sexp.to_string_hum (sexp_of_item t))


let bindings = List.filter_map ~f:(function
  | Let (name, _, _) | Module (name, _) | Class (name, _, _) -> Some name
  | Do _ -> None)

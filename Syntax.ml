module List = Core_kernel.Std.List
open Sexplib.Std

type 'a typed = 'a * string
  [@@deriving sexp]

type pattern = string
  [@@deriving sexp]

type operator =
  | Plus | Minus | Times | Divide | Equal | NotEqual
  | And | Or
  [@@deriving sexp]

type term =
  | Identifier of string
  | String of string
  | Number of int
  | Boolean of bool
  | Tuple of term list (* length <> 1 *)
  | CaseFunction of case list
  | Switch of term * case list
  | IfElse of term * term * term
  | LetIn of pattern * term * term
  | Infix of term * operator * term
  | Call of term * term
  [@@deriving sexp]

and case = pattern * term
  [@@deriving sexp]


type item =
  | Let of pattern typed * pattern typed list option * term
  | Do of term
  | Module of string * item list
  | Class of string * pattern typed list * item list
  [@@deriving sexp]



let a, b, c, d = Identifier "a", Identifier "b", Identifier "c", Identifier "d"

let dump t = print_string (Sexplib.Sexp.to_string_hum (sexp_of_term t))

let dump_item t = print_string (Sexplib.Sexp.to_string_hum (sexp_of_item t))


let binding = function
  | Let ((name, _), _, _) | Module (name, _) | Class (name, _, _) -> Some name
  | Do _ -> None

let bindings = List.filter_map ~f:binding

let parameter_names = List.map ~f:fst

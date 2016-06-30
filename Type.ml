module List = Core_kernel.Std.List
module Sexp = Core_kernel.Std.Sexp
module String = Core_kernel.Std.String
module V = Syntax

module Result = struct
  let to_either = function
    | Ok ok -> `Fst ok
    | Error error -> `Snd error

  let all items =
    let oks, errors = List.partition_map items ~f:to_either in
    let errors = List.concat errors in
    if errors <> [] then Error errors else Ok oks

  let bind = function
    | Ok ok   -> fun f -> f ok
    | Error e -> fun _ -> Error e

  let map ~f result =
    bind result (fun x -> Ok (f x))

  let both left right = match left, right with
    | Ok left, Ok right -> Ok (left, right)
    | Error left, Error right -> Error (left @ right)
    | Error error, _ | _, Error error -> Error error

  let of_option ~error = function
    | None -> Error [error]
    | Some x -> Ok x
end
module Let_syntax = Result


type t =
  | Any
  | Boolean
  | Number
  | String
  | Tuple of t list (* length <> 1 *)
  | Arrow of t * t
  | Class of string * (string * t) list
  | Module of string * (string * t) list

let rec to_string = function
  | Any     -> "Any"
  | Boolean -> "Boolean"
  | Number  -> "Number"
  | String  -> "String"
  | Tuple items ->
      "(" ^ String.concat ~sep:", " (List.map items ~f:to_string) ^ ")"
  | Arrow (left, right) ->
      "(" ^ to_string left ^ " -> " ^ to_string right ^ ")"
  | Class _ -> "Class"
  | Module _ -> "Module"

let print t =
  print_endline (to_string t)


module Table = struct
  include Core_kernel.Std.String.Map

  let to_string table =
    Sexp.to_string_hum (sexp_of_t (fun t -> Sexp.Atom (to_string t)) table)

  let print table =
    print_endline (to_string table)

  let merge_right left right =
    merge left right ~f:(fun ~key -> function
      | `Both (_, value) | `Left value | `Right value -> Some value)
end


let operator_signature = function

  | V.Plus | V.Minus | V.Times | V.Divide ->
      Arrow (Tuple [Number; Number], Number)

  | V.Equal | V.NotEqual ->
      Arrow (Tuple [Any; Any], Boolean)

  | V.And | V.Or ->
      Arrow (Tuple [Boolean; Boolean], Boolean)


module Term = struct

  let rec infer_call tenv env caller_t argument = match caller_t with

    | Arrow (parameter_t, return_t) ->
        let%bind argument_t = infer tenv env argument in
          if argument_t = parameter_t
            then Ok return_t
            else Error [`Parameter_type_does_not_match_argument]

    | _ ->
        Error [`Not_a_function]

  and infer tenv env = function

    | V.Boolean _ ->
        Ok Boolean

    | V.Number _ ->
        Ok Number

    | V.String _ ->
        Ok String

    | V.Identifier id ->
        Table.find env id |> Result.of_option ~error:(`Unbound_identifier id)

    | V.Tuple items ->
        let%bind types = items |> List.map ~f:(infer tenv env) |> Result.all in
        Ok (Tuple types)

    | V.Infix (left, operator, right) ->
        let caller_t = operator_signature operator in
        infer_call tenv env caller_t (V.Tuple [left; right])

    | V.Call (caller, argument) ->
        let%bind caller_t = infer tenv env caller in
        infer_call tenv env caller_t argument

    | V.IfElse (condition, consequence, alternative) ->
        let%bind condition_t =
          let%bind condition_t = infer tenv env condition in
          if condition_t <> Boolean
            then Error [`If_condition_not_boolean]
            else Ok condition_t
        and return_t =
          let%bind consequence_t = infer tenv env consequence
          and alternative_t = infer tenv env alternative in
          if consequence_t <> alternative_t
            then Error [`If_consequence_and_alternative_types_do_not_match]
            else Ok consequence_t
        in Ok return_t

    | V.LetIn (name, value, body) ->
        let%bind value_t = infer tenv env value in
        let body_env = Table.add env ~key:name ~data:value_t in
        let%bind body_t = infer tenv body_env body in
        Ok body_t

    | V.CaseFunction _
    | V.Switch _ -> assert false
end


let find_type tenv type_id =
  Table.find tenv type_id |> Result.of_option ~error:(`Cannot_find_type type_id)


let infer_parameter tenv env = function

  | [parameter, type_id] ->
      let%bind parameter_t = find_type tenv type_id in
      let env' = Table.add env ~key:parameter ~data:parameter_t in
      Ok (env', parameter_t)

  | parameters ->
      let pair_to_type (parameter, type_id) =
        let%bind parameter_t = find_type tenv type_id in
        Ok (parameter, parameter_t)
      in
      let%bind pairs = parameters |> List.map ~f:pair_to_type |> Result.all in
      let types = List.map ~f:snd pairs in
      let%bind parameters_env = Table.of_alist pairs |> (function
        | `Duplicate_key key -> Error [`Duplicate_parameter_name key]
        | `Ok env -> Ok env)
      in
      let new_env = Table.merge_right env parameters_env in
      Ok (new_env, Tuple types)


let rec infer tenv env = function

  | V.Do term ->
      let%bind term_t = Term.infer tenv env term in
      if term_t <> Tuple []
        then Error [`Do_should_evaluate_to_unit]
        else Ok (Tuple [])

  | V.Let ((name, return_type_id), None, body) ->
      let%bind return_t = find_type tenv return_type_id
      and body_t = Term.infer tenv env body in
      if body_t <> return_t
        then Error [`Declared_type_does_not_match_real_one return_type_id]
        else Ok body_t

  | V.Let ((name, return_type_id), Some parameter, body) ->
      let%bind return_t = find_type tenv return_type_id
      and body_env, parameter_t = infer_parameter tenv env parameter in
      let signature_t = Arrow (parameter_t, return_t) in
      let%bind body_t = Term.infer tenv body_env body in
      if body_t <> return_t
        then Error [`Declared_type_does_not_match_real_one return_type_id]
        else Ok signature_t

  | _ -> assert false
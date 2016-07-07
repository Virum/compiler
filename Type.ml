open Sexplib.Std
module Sexp = Sexplib.Sexp
module List = Core_kernel.Std.List
module String = Core_kernel.Std.String
module V = Syntax

module Result = Outcome
module Let_syntax = Outcome

type t =
  | Any
  | Boolean
  | Number
  | String
  | Tuple of t list (* length <> 1 *)
  | Arrow of t * t
  | Class of string * environment * environment
  | Module of string * environment * environment
  [@@deriving sexp]


and environment = t Core_kernel.Std.String.Map.t
  [@@deriving sexp]

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


module Environment = struct
  module Map = Core_kernel.Std.String.Map

  type t = environment

  let find = Map.find
  let empty = Map.empty
  let add = Map.add
  let of_alist = Map.of_alist
  let of_alist_exn = Map.of_alist_exn

  let to_string env =
    Sexp.to_string_hum (Map.sexp_of_t (fun t -> Sexp.Atom (to_string t)) env)

  let print env =
    print_endline (to_string env)

  let merge_right left right =
    Map.merge left right ~f:(fun ~key -> function
      | `Both (_, value) | `Left value | `Right value -> Some value)
end
module Env = Environment


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
        Env.find env id |> Result.of_option ~error:(`Unbound_identifier id)

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
        let body_env = Env.add env ~key:name ~data:value_t in
        let%bind body_t = infer tenv body_env body in
        Ok body_t

    | V.Map _
    | V.Array _
    | V.Member _
    | V.CaseFunction _
    | V.Extension _
    | V.Switch _ -> assert false
end


let find_type tenv type_id =
  Env.find tenv type_id |> Result.of_option ~error:(`Cannot_find_type type_id)


let rec is_subtype left right = match left, right with

  | _, Any ->
      true

  | Any, _ ->
      false

  | Boolean, Boolean | Number, Number | String, String ->
      true

  | Tuple left, Tuple right ->
      if List.length left <> List.length right then false else
      (* exn: we just checked that length is same *)
      List.for_all2_exn left right ~f:is_subtype

  | Arrow (left_in, left_out), Arrow (right_in, right_out) ->
      is_subtype right_in left_in && is_subtype left_out right_out

  | Module _, Module _ ->
      true (* we'll see *)

  | _ -> false


let infer_parameter tenv env = function

  | [parameter, type_id] ->
      let%bind parameter_t = find_type tenv type_id in
      let env' = Env.add env ~key:parameter ~data:parameter_t in
      Ok (env', parameter_t)

  | parameters ->
      let pair_to_type (parameter, type_id) =
        let%bind parameter_t = find_type tenv type_id in
        Ok (parameter, parameter_t)
      in
      let%bind pairs = parameters |> List.map ~f:pair_to_type |> Result.all in
      let types = List.map ~f:snd pairs in
      let%bind parameters_env = Env.of_alist pairs |> (function
        | `Duplicate_key key -> Error [`Duplicate_parameter_name key]
        | `Ok env -> Ok env)
      in
      let new_env = Env.merge_right env parameters_env in
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
      if not (is_subtype body_t return_t)
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

  | V.Module (name, body) ->
      let%bind tenv, env, tenv', env' =
        List.fold body ~init:(Ok Env.(tenv, env, empty, empty))
        ~f:begin fun result item ->
          let%bind tenv, env, tenv', env' = result in
          let%bind item_t = infer tenv env item in
          match V.binding item with
          | Some name ->
              let tenv, tenv' = match item_t with
                | Module _ ->
                  let tenv = Env.add tenv ~key:name ~data:item_t in
                  let tenv' = Env.add tenv' ~key:name ~data:item_t in
                  tenv, tenv'
                | _ -> tenv, tenv'
              in
              let env = Env.add env ~key:name ~data:item_t in
              let env' = Env.add env' ~key:name ~data:item_t in
              Ok (tenv, env, tenv', env')
          | None -> Ok (tenv, env, tenv', env')
      end in
      Ok (Module (name, tenv', env'))

  | V.Class _ ->
      assert false

  | V.Import _ ->
      assert false

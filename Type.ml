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
  | Array of t
  | Map of t * t
  | Arrow of t * t
  | Class of string * environments
  | Module of string * environments
  [@@deriving sexp]


and environment = t Core_kernel.Std.String.Map.t
  [@@deriving sexp]

and environments = {types: environment; values: environment}

let rec to_string = function
  | Any     -> "Any"
  | Boolean -> "Boolean"
  | Number  -> "Number"
  | String  -> "String"
  | Tuple items ->
      "(" ^ String.concat ~sep:", " (List.map items ~f:to_string) ^ ")"
  | Array item ->
      Printf.sprintf "Array[%s]" (to_string item)
  | Map (key, value) ->
      Printf.sprintf "Map[%s, %s]" (to_string key) (to_string value)
  | Arrow (left, right) ->
      "(" ^ to_string left ^ " -> " ^ to_string right ^ ")"
  | Class (name, _) -> "class " ^ name
  | Module (name, _) -> "module " ^ name

let print t =
  print_endline (to_string t)


module Environment = struct
  module Map = Core_kernel.Std.String.Map

  type t = environment

  let find = Map.find
  let empty = Map.empty
  let add map key value = Map.add map ~key ~data:value
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

  let rec infer_call xenv caller_t argument = match caller_t with

    | Arrow (parameter_t, return_t) ->
        let%bind argument_t = infer xenv argument in
          if argument_t = parameter_t
            then Ok return_t
            else Error [`Parameter_type_does_not_match_argument]

    | _ ->
        Error [`Not_a_function]

  and infer xenv = function

    | V.Boolean _ ->
        Ok Boolean

    | V.Number _ ->
        Ok Number

    | V.String _ ->
        Ok String

    | V.Identifier id ->
        Env.find xenv.values id |> Result.of_option ~error:(`Unbound_identifier id)

    | V.Tuple items ->
        let%bind types = items |> List.map ~f:(infer xenv) |> Result.all in
        Ok (Tuple types)

    | V.Infix (left, operator, right) ->
        let caller_t = operator_signature operator in
        infer_call xenv caller_t (V.Tuple [left; right])

    | V.Call (caller, argument) ->
        let%bind caller_t = infer xenv caller in
        infer_call xenv caller_t argument

    | V.IfElse (condition, consequence, alternative) ->
        let%bind () =
          let%bind condition_t = infer xenv condition in
          if condition_t <> Boolean
            then Error [`If_condition_not_boolean]
            else Ok ()
        and return_t =
          let%bind consequence_t = infer xenv consequence
          and alternative_t = infer xenv alternative in
          if consequence_t <> alternative_t
            then Error [`If_consequence_and_alternative_types_do_not_match]
            else Ok consequence_t
        in Ok return_t

    | V.LetIn (name, value, body) ->
        let%bind value_t = infer xenv value in
        let body_env = Env.add xenv.values name value_t in
        let%bind body_t = infer {xenv with values=body_env} body in
        Ok body_t

    | V.Array items ->
        let%bind types = items |> List.map ~f:(infer xenv) |> Result.all in
        (match List.dedup types with
        | [] -> Error [`Empty_array_needs_type_annotation]
        | [type_] -> Ok (Array type_)
        | types -> Error [`Heterogeneous_array (List.map ~f:to_string types)])

    | V.Map pairs ->
        let%bind pairs_of_types =
          pairs |> Pair.List.map ~f:(infer xenv)
                |> List.map ~f:(Pair.uncurry Result.both)
                |> Result.all in
        (match List.dedup pairs_of_types with
        | [] -> Error [`Empty_map_needs_type_annotation]
        | [key_t, value_t] -> Ok (Map (key_t, value_t))
        | type_pairs ->
            Error [`Heterogeneous_map (Pair.List.map type_pairs ~f:to_string)])

    | V.Member (value, member) ->
        (match%bind infer xenv value with
        | Module (_, xenv) | Class (_, xenv) as value_t ->
            Env.find xenv.values member |> Result.of_option
              ~error:(`Member_does_not_belong (member, to_string value_t))
        | value_t ->
            Error [`Member_does_not_belong (member, to_string value_t)])

    | V.CaseFunction _
    | V.Extension _
    | V.Switch _ -> assert false
end


let find_type xenv type_id =
  Env.find xenv.types type_id
    |> Result.of_option ~error:(`Cannot_find_type type_id)


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


let add_type xenv key value = {xenv with types=Env.add xenv.types key value}
let add_value xenv key value = {xenv with values=Env.add xenv.values key value}

let infer_parameter xenv = function

  | [parameter, type_id] ->
      let%bind parameter_t = find_type xenv type_id in
      let venv = Env.add xenv.values parameter parameter_t in
      Ok (venv, parameter_t)

  | parameters ->
      let pair_to_type (parameter, type_id) =
        let%bind parameter_t = find_type xenv type_id in
        Ok (parameter, parameter_t)
      in
      let%bind pairs = parameters |> List.map ~f:pair_to_type |> Result.all in
      let types = List.map ~f:snd pairs in
      let%bind parameters_venv = Env.of_alist pairs |> (function
        | `Duplicate_key key -> Error [`Duplicate_parameter_name key]
        | `Ok venv -> Ok venv)
      in
      let new_env = Env.merge_right xenv.values parameters_venv in
      Ok (new_env, Tuple types)


let rec infer ({types=tenv; values=env} as xenv) = function

  | V.Do term ->
      let%bind term_t = Term.infer xenv term in
      if term_t <> Tuple []
        then Error [`Do_should_evaluate_to_unit]
        else Ok (Tuple [])

  | V.Let ((name, return_type_id), None, body) ->
      let%bind return_t = find_type xenv return_type_id
      and body_t = Term.infer xenv body in
      if not (is_subtype body_t return_t)
        then Error [`Declared_type_does_not_match_real_one return_type_id]
        else Ok body_t

  | V.Let ((name, return_type_id), Some parameter, body) ->
      let%bind return_t = find_type xenv return_type_id
      and body_env, parameter_t = infer_parameter xenv parameter in
      let signature_t = Arrow (parameter_t, return_t) in
      let%bind body_t = Term.infer {xenv with values=body_env} body in
      if body_t <> return_t
        then Error [`Declared_type_does_not_match_real_one return_type_id]
        else Ok signature_t

  | V.Module (name, body) ->
      let%bind outer, inner =
        List.fold body ~init:(Ok Env.(xenv, {types=empty; values=empty}))
        ~f:begin fun result item ->
          let%bind outer, inner = result in
          let%bind item_t = infer outer item in
          match V.binding item with
          | None -> result
          | Some name ->
              let outer, inner = match item_t with
                | Module _ ->
                    add_type outer name item_t, add_type inner name item_t
                | _ -> outer, inner
              in
              Ok (add_value outer name item_t, add_value inner name item_t)
      end in
      Ok (Module (name, inner))

  | V.Class _ ->
      assert false

  | V.Import _ ->
      assert false

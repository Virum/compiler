module Table = Core_kernel.Std.String.Map
module List = Core_kernel.Std.List
module V = Syntax


let or_error error = function
  | None -> Error [error]
  | Some x -> Ok x


module Let_syntax = struct
  let bind = function
    | Ok ok   -> fun f -> f ok
    | Error e -> fun _ -> Error e

  let map result f =
    bind result (fun x -> Ok (f x))

  let both r1 r2 = match r1, r2 with
    | Ok ok1, Ok ok2 -> Ok (ok1, ok2)
    | Error e1, Error e2 -> Error (e1 @ e2)
    | Error e, _ | _, Error e -> Error e
end


type t =
  | Any
  | Boolean
  | Number
  | String
  | Tuple of t list (* length <> 1 *)
  | Arrow of t * t
  | Class of string * (string * t) list
  | Module of string * (string * t) list


let operator_signature = function

  | V.Plus | V.Minus | V.Times | V.Divide ->
      Arrow (Tuple [Number; Number], Number)

  | V.Equal | V.NotEqual ->
      Arrow (Tuple [Any; Any], Boolean)

  | V.And | V.Or ->
      Arrow (Tuple [Boolean; Boolean], Boolean)


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
      Table.find env id |> or_error (`Unbound_identifier id)

  | V.Tuple items ->
      let oks, errors = List.partition_map items ~f:(fun term ->
        match infer tenv env term with
        | Ok ok -> `Fst ok
        | Error e -> `Snd e) in
      let errors = List.concat errors in
      if errors <> [] then Error errors else Ok (Tuple oks)

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


  (* Syntax.term -> (unit, poly) Result.t *)
(* let check = *)

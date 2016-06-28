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

  | _ -> assert false


  (* Syntax.term -> (unit, poly) Result.t *)
(* let check = *)

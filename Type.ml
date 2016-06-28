module Table = Core_kernel.Std.String.Map
(* module Result = Core_kernel.Std.Result *)
module V = Syntax

let or_error error = function
  | None -> Error [error]
  | Some x -> Ok x

type t =
  | Any
  | Boolean
  | Number
  | String
  | Tuple of t list (* length <> 1 *)
  | Arrow of t * t
  | Class of string * (string * t) list
  | Module of string * (string * t) list


let to_signature = function

  | V.Plus | V.Minus | V.Times | V.Divide ->
      Arrow (Tuple [Number; Number], Number)

  | V.Equal | V.NotEqual ->
      Arrow (Tuple [Any; Any], Boolean)

  | V.And | V.Or ->
      Arrow (Tuple [Boolean; Boolean], Boolean)


let rec infer tenv env = function

  | V.Number _ ->
      Ok Number

  | V.String _ ->
      Ok String

  | V.Identifier id ->
      Table.find env id |> or_error (`Unbound_identifier id)

  | V.Infix (left, _, right) ->
      (match infer tenv env left, infer tenv env right with
      | Ok Number, Ok Number -> Ok Number
      | Ok _     , Ok _      -> Error [`Infix_operand_type_error]
      | Error e  , Ok _
      | Ok _     , Error e   -> Error e
      | Error e1 , Error e2  -> Error (e1 @ e2)
      )

  | V.Call (caller, argument) ->
      let caller_t = infer tenv env caller in
      (match caller_t with
      | Ok Arrow (argument_t, return_t) -> Ok return_t
      | Ok _ -> Error [`Not_a_function]
      | Error e -> Error e)

  | _ -> assert false


  (* Syntax.term -> (unit, poly) Result.t *)
(* let check = *)

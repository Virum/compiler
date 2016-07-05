module List = Core_kernel.Core_list
module Result = Outcome
module Let_syntax = Outcome
open Syntax


module Term = struct

  let rec compile ~f = function

    | Identifier _ | String _ | Number _ | Boolean _ as atom ->
        Ok atom

    | IfElse (condition, consequence, alternative) ->
        let%bind condition = f condition
        and consequence = f consequence
        and alternative = f alternative in
        Ok (IfElse (condition, consequence, alternative))

    | LetIn (left, right, body) ->
        let%bind right = f right and body = f body in
        Ok (LetIn (left, right, body))

    | Infix (left, operator, right) ->
        let%bind left = f left and right = f right in
        Ok (Infix (left, operator, right))

    | Call (caller, argument) ->
        let%bind caller = f caller
        and argument = f argument in
        Ok (Call (caller, argument))

    | Member (term, member) ->
        let%bind term = f term in
        Ok (Member (term, member))

    | Map pairs ->
        let compile_pair (left, right) =
          let%bind left = f left and right = f right in
          Ok (left, right)
        in
        let%bind pairs = pairs |> List.map ~f:compile_pair |> Result.all in
        Ok (Map pairs)

    | Array terms ->
        let%bind terms = terms |> List.map ~f:f |> Result.all in
        Ok (Array (terms))

    | Tuple terms ->
        let%bind terms = terms |> List.map ~f:f |> Result.all in
        Ok (Tuple (terms))

    | Extension (left, right) ->
        let%bind left = f left and right = f right in
        Ok (Extension (left, right))

    | Switch _ | CaseFunction _ as other -> Ok other (* TODO *)

end


let rec compile ~f = function

  | Let (left, right, term) ->
      let%bind term = f term in
      Ok (Let (left, right, term))

  | Do term ->
      let%bind term = f term in
      Ok (Do term)

  | Import name as import ->
      Ok import

  | Module (name, items) ->
      let%bind items = items |> List.map ~f:(compile ~f) |> Result.all in
      Ok (Module (name, items))

  | Class (name, parameters, items) ->
      let%bind items = items |> List.map ~f:(compile ~f) |> Result.all in
      Ok (Class (name, parameters, items))

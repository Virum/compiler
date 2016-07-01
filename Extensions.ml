module List = Core_kernel.Core_list
module Let_syntax = Outcome
open Syntax







module Json = struct
  let compile = function
    | Extension (Identifier "Json", _) ->
        Ok (Identifier "yay")
    | Extension _ ->
        Error [`Invalid_syntax_extension]
    | _ -> assert false

end


let rec compile = function

  | Let (left, right, term) ->
      let%bind term = Json.compile term in
      Ok (Let (left, right, term))

  | Do term ->
      let%bind term = Json.compile term in
      Ok (Do term)

  | Module (name, items) ->
(*       Module (name, List.map items ~f:compile) *)
      Ok (Module (name, items))

  | Class (name, parameters, items) ->
(*       Class (name, parameters, List.map items ~f:compile) *)
      Ok (Class (name, parameters, items))


module List = Core_kernel.Core_list
module Result = Outcome
module Let_syntax = Outcome
open Syntax


module Json = struct
  let rec compile = function

    | Extension (Identifier "Json", _) ->
        Ok (Identifier "yay")

    | Extension _ ->
        Error [`Invalid_syntax_extension]

    | other ->
        Traversal.Term.compile ~f:compile other

end


let compile = Traversal.compile ~f:Json.compile

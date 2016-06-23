module List = Core_kernel.Std.List

module V = Syntax
module Py = Python

module Operator = struct
  let compile = function
    | V.Plus           -> Py.Operator.Plus
    | V.Minus          -> Py.Operator.Minus
    | V.Times          -> Py.Operator.Times
    | V.Divide         -> Py.Operator.Divide
    | V.Equal          -> Py.Operator.Equal
    | V.NotEqual       -> Py.Operator.NotEqual
    | V.Less           -> Py.Operator.Less
    | V.Greater        -> Py.Operator.Greater
    | V.LessOrEqual    -> Py.Operator.LessOrEqual
    | V.GreaterOrEqual -> Py.Operator.GreaterOrEqual
    | V.And            -> Py.Operator.And
    | V.Or             -> Py.Operator.Or
end


module Term = struct

  let rec compile = function

    | V.Number int ->
        Py.Number (float_of_int int)

    | V.String s ->
        Py.String s

    | V.Identifier s ->
        Py.Identifier s

    | V.CaseFunction [(parameter, term)] ->
        Py.(Lambda ([parameter], (compile term)))

    | V.IfElse (condition, consequence, alternative) ->
        Py.(IfElse {
          condition=compile condition;
          consequence=compile consequence;
          alternative=compile alternative;
        })

    | V.LetIn (left, right, body) ->
        Py.(Call (Lambda ([left], compile body), [compile right]))

    | V.Infix (left, operator, right) ->
        let operator = Operator.compile operator in
        Py.(Infix (compile left, operator, compile right))

    | V.Call (callee, arguments) ->
        Py.(Call (compile callee, List.map ~f:compile arguments))

    | _ -> assert false
end

let bindings statements = List.filter_map statements ~f:(function
  | V.Let (name, _) | V.Module (name, _) | V.Class (name, _, _) -> Some name
  | V.Do _ -> None)

let rec compile = function

  | V.Let (name, term) ->
      Py.(Assignment (Identifier name, Term.compile term))

  | V.Do term ->
      Py.Term (Term.compile term)

  | V.Module (name, body) ->
      let body' = List.map body ~f:compile in
      let name_to_pair name = Py.(String name, Identifier name) in
      let dict = List.map (bindings body) ~f:name_to_pair in
      Py.(Def (Some "apply", name, [], body' @ [
         Return (Call (Call (Identifier "type", [
           String name;
           Tuple [];
           Dict dict;
         ]), []))
      ]))

  | V.Class (name, parameters, body) ->
      let body' = List.map body ~f:compile in
      let name_to_pair name =
        Py.(Assignment (Member (Identifier "self", name), Identifier name)) in
      Py.(Class (name, "object", [
        Def (None, "__init__", ["self"] @ parameters, body' @
          List.map (bindings body) ~f:name_to_pair
        );
      ]))

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

    | V.Call (callee, V.Tuple arguments) ->
        Py.(Call (compile callee, List.map ~f:compile arguments))

    | V.Call (callee, arguments) ->
        (* TODO depending on type of arguments, it should be
           either called as f(x) or apply(f, x) *)
        Py.(Call (compile callee, [compile arguments]))

    | _ -> assert false
end

let rec compile = function

  | V.Let ((name, _), None, term) ->
      Py.(Assignment (Identifier name, Term.compile term))

  | V.Let ((name, _), Some parameters, term) ->
      let parameters = V.parameter_names parameters in
      Py.(Def (None, name, parameters, [Return (Term.compile term)]))

  | V.Do term ->
      Py.Term (Term.compile term)

  | V.Module (name, body) ->
      let body' = List.map body ~f:compile in
      let name_to_pair name = Py.(String name, Identifier name) in
      let dict = List.map (V.bindings body) ~f:name_to_pair in
      Py.(Def (Some "apply", name, [], body' @ [
         Return (Call (Call (Identifier "type", [
           String name;
           Tuple [];
           Dict dict;
         ]), []))
      ]))

  | V.Class (name, parameters, body) ->
      let parameters = V.parameter_names parameters in
      let body' = List.map body ~f:compile in
      let name_to_pair name =
        Py.(Assignment (Member (Identifier "self", name), Identifier name)) in
      Py.(Class (name, "object", [
        Def (None, "__init__", ["self"] @ parameters, body' @
          List.map (V.bindings body) ~f:name_to_pair
        );
      ]))

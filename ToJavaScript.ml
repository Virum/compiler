module List = Core_kernel.Std.List

module V = Syntax
module JS = JavaScript



module Operator = struct
  let compile = function
    | V.Plus           -> JS.Operator.Plus
    | V.Minus          -> JS.Operator.Minus
    | V.Times          -> JS.Operator.Times
    | V.Divide         -> JS.Operator.Divide
    | V.Equal          -> JS.Operator.Equal
    | V.NotEqual       -> JS.Operator.NotEqual
    | V.Less           -> JS.Operator.Less
    | V.Greater        -> JS.Operator.Greater
    | V.LessOrEqual    -> JS.Operator.LessOrEqual
    | V.GreaterOrEqual -> JS.Operator.GreaterOrEqual
    | V.And            -> JS.Operator.And
    | V.Or             -> JS.Operator.Or

end


module Term = struct
  let rec compile = function
    | V.Number int -> JS.Number (float_of_int int)
    | V.String s -> JS.String s
    | V.Identifier "print" -> JS.(Member (Identifier "console", String "log"))
    | V.Identifier s -> JS.Identifier s
    | V.CaseFunction [(parameter, term)] ->
        JS.(Function (None, [parameter], [Return (compile term)]))

    | V.IfElse (condition, consequence, alternative) ->
        JS.(Call (Function (None, [], [
          IfElse (
            compile condition,
            [Return (compile consequence)],
            [Return (compile alternative)]
          )
        ]), []))

    | V.LetIn (left, (V.CaseFunction [(parameter, term)]), body) ->
        let f = JS.(Function (Some left, [parameter], [Return (compile term)])) in
        JS.(Call (Function (None, [left], [
          Return (compile body);
        ]), [f]))

    | V.LetIn (left, right, body) ->
        JS.(Call (Function (None, [left], [
          Return (compile body);
        ]), [compile right]))
    | V.Infix (left, operator, right) ->
        let operator = Operator.compile operator in
        JS.(Infix (compile left, operator, compile right))
    | V.Call (callee, arguments) ->
        JS.(Call (compile callee, List.map ~f:compile arguments))
    | _ -> assert false
end


let compile_prelude name =
  let open JS in let module Op = JS.Operator in
  IfElse (
    Prefix (Op.Prefix.Not, (Infix (id "this", Op.Instanceof, id name))),
    [
      Return (
        Prefix (
          Op.Prefix.New,
          Call (
            Member (
              Member (
                Member (id "Function", String "prototype"),
                String "bind"),
              String "apply"),
            [id name; id "arguments"])));
    ],
    []
  )

let rec compile_namespace ~name ~parameters ~body =
  let body' = List.map body ~f:compile in
  let name_to_pair name = (name, JS.Identifier name) in
  let object' = List.map (V.bindings body) ~f:name_to_pair in
  JS.(Function (Some name, parameters, body' @ [Return (Object object')]))

and compile = function
  | V.Module (name, body) ->
      JS.(Var (name, (Call (compile_namespace ~name ~parameters:[] ~body, []))))

  | V.Let (name, None, term) ->
      JS.(Var (name, Term.compile term))

  | V.Let (name, Some parameters, term) ->
      JS.(Term (Function (Some name, parameters, [Return (Term.compile term)])))

  | V.Do term -> JS.Term (Term.compile term)
  | V.Class (name, parameters, body) ->
      let body' = List.map body ~f:compile in
      let name_to_pair name =
        JS.(Term (Infix (Member (Identifier "this", String name),
                         Operator.Assignment,
                         Identifier name))) in
      JS.(Var (name, Function (Some name, parameters,
        compile_prelude name :: body' @ List.map (V.bindings body) ~f:name_to_pair
      )))
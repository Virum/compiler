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
    | V.Call (callee, V.Tuple arguments) ->
        JS.(Call (compile callee, List.map ~f:compile arguments))
    | V.Call (callee, argument) ->
        (* TODO depending on type of argument it should be compiled
           either as a f(x) or as f.apply(x) *)
        JS.(Call (compile callee, [compile argument]))
    | V.Member (term, member) ->
        JS.(Member (compile term, String member))
    | V.Array terms ->
        JS.(Array (List.map ~f:compile terms))
    | V.Map pairs ->
        if List.for_all pairs ~f:(function V.String _, _ -> true | _ -> false)
          then
            JS.(Object (List.map pairs ~f:(function V.String key, term ->
              (key, compile term) | _ -> assert false)))
          else
            let compile_pair (key, value) =
              JS.(Term (Infix (Member (Identifier "this", compile key),
                               Operator.Assignment,
                               compile value))) in
            JS.(NewCall (Function (None, [], List.map ~f:compile_pair pairs), []))
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

let rec compile_module_body name body =
  let compile_assignment name =
    JS.(Term (Infix (Member (Identifier "this", String name),
                     Operator.Assignment,
                     Identifier name))) in
  List.map body ~f:compile @ List.map (V.bindings body) ~f:compile_assignment

and compile = function
  | V.Module (name, body) ->
      JS.(Var (name, Prefix (Operator.Prefix.New, (Call (
        Function (Some name, [], compile_module_body name body),
      [])))))

  | V.Let ((name, _), None, term) ->
      JS.(Var (name, Term.compile term))

  | V.Let ((name, _), Some parameters, term) ->
      let parameters = V.parameter_names parameters in
      JS.(Term (Function (Some name, parameters, [Return (Term.compile term)])))

  | V.Do term ->
      JS.Term (Term.compile term)

  | V.Class (name, parameters, body) ->
      let parameters = V.parameter_names parameters in
      JS.(Var (name, Function (Some name, parameters,
        compile_prelude name :: compile_module_body name body
      )))

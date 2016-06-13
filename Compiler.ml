
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
      JS.(Call (compile callee, List.map compile arguments))
  | _ -> assert false

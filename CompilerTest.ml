let (test), (=>) = Test.(test, (=>))


module JS = JavaScript
module V = Syntax

let compile = Compiler.compile

let () = test "Atoms" @@ fun () ->
  compile (V.Number 1) => JS.Number 1.;
  compile (V.String "s") => JS.String "s";
  compile (V.Identifier "s") => JS.Identifier "s"

let () = test "Case function" @@ fun () ->
  compile V.(CaseFunction [("parameter", a)])
    => JS.(Function (None, ["parameter"], [Return a]))

let () = test "If-else" @@ fun () ->
  compile V.(IfElse (a, b, c))
    => JS.(Call (Function (None, [], [
         IfElse (a, [Return b], [Return c])
       ]), []))

let () = test "Let in" @@ fun () ->
  compile V.(LetIn ("a", b, c))
    => JS.(Call (Function (None, ["a"], [
         Return c
       ]), [b]))

let () = test "Infix" @@ fun () ->
  compile V.(Infix (a, Times, b))
    => JS.(Infix (a, Operator.Times, b))

let () = test "Call" @@ fun () ->
  compile V.(Call (a, [b; c]))
    => JS.(Call (a, [b; c]))

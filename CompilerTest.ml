let (test), (=>) = Test.(test, (=>))


module JS = JavaScript
module V = Syntax

let term = Compiler.Term.compile
let item = Compiler.compile

let () = test "Atoms" @@ fun () ->
  term (V.Number 1) => JS.Number 1.;
  term (V.String "s") => JS.String "s";
  term (V.Identifier "s") => JS.Identifier "s"

let () = test "Case function" @@ fun () ->
  term V.(CaseFunction [("parameter", a)])
    => JS.(Function (None, ["parameter"], [Return a]))

let () = test "If-else" @@ fun () ->
  term V.(IfElse (a, b, c))
    => JS.(Call (Function (None, [], [
         IfElse (a, [Return b], [Return c])
       ]), []))

let () = test "Let in" @@ fun () ->
  term V.(LetIn ("a", b, c))
    => JS.(Call (Function (None, ["a"], [
         Return c
       ]), [b]))

let () = test "Infix" @@ fun () ->
  term V.(Infix (a, Times, b))
    => JS.(Infix (a, Operator.Times, b))

let () = test "Call" @@ fun () ->
  term V.(Call (a, [b; c]))
    => JS.(Call (a, [b; c]))

(* ITEM *)

(*
let () = test "Module" @@ fun () ->
  item V.(Module ("foo", []))
    => JS.(Var ("foo", Function (None, [], [
         Return (Number 42);
       ])))
*)


(*

var foo = {}


var foo = (function () {
  return {};
})

var foo = (function () {
  var bar = ...;
  return {bar: bar};
})
*)

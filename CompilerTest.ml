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

let () = test "Module" @@ fun () ->
  item V.(Module ("foo", []))
    => JS.(Var ("foo", (Call (Function (Some "foo", [], [
         Return (Object []);
       ]), []))));
  item V.(Module ("foo", [
    Let ("a", b);
    Let ("b", c);
  ]))
    => JS.(Var ("foo", (Call (Function (Some "foo", [], [
         Var ("a", b);
         Var ("b", c);
         Return (Object [("a", a); ("b", b)]);
       ]), []))));
  item V.(Module ("foo", [
    Module ("a", [])
  ]))
    => JS.(Var ("foo", (Call (Function (Some "foo", [], [
         Var ("a", Call (Function (Some "a", [], [Return (Object [])]), []));
         Return (Object [("a", a)]);
       ]), []))));
  item V.(Module ("foo", [
    Do (Call (Identifier "factorial", [Number 5]));
  ]))
    => JS.(Var ("foo", (Call (Function (Some "foo", [], [
         Term (Call (Identifier "factorial", [Number 5.]));
         Return (Object []);
       ]), []))))

let () = test "Let" @@ fun () ->
  item V.(Let ("a", b))
    => JS.(Var ("a", b))

let () = test "Class" @@ fun () ->
  item V.(Class ("Foo", ["a"; "b"], [
    Do (Call (a, []));
    Let ("b", b);
  ])) |> JS.print_statement;
  item V.(Class ("Foo", ["a"; "b"], [
    Do (Call (a, []));
    Let ("b", b);
  ]))
    => JS.(Var ("Foo", Function (Some "Foo", ["a"; "b"], [
         Term (Call (a, []));
         Var ("b", b);
         Return (Object [("b", b)]);
       ])))


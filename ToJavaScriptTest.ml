let (test), (=>) = Test.(test, (=>))


module JS = JavaScript
module V = Syntax

let term = ToJavaScript.Term.compile
let item = ToJavaScript.compile

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
    Let ("a", None, b);
    Let ("b", None, c);
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
  item V.(Let ("a", None, b))
    => JS.(Var ("a", b));
  item V.(Let ("a", Some [], b))
    => JS.(Term (Function (Some "a", [], [Return b])));
  item V.(Let ("a", Some ["b"; "c"], d))
    => JS.(Term (Function (Some "a", ["b"; "c"], [Return d])))

let () = test "Class" @@ fun () ->
  item V.(Class ("Foo", ["a"; "b"], [
    Do (Call (a, []));
    Let ("b", None, b);
  ]))
    => JS.(Var ("Foo", Function (Some "Foo", ["a"; "b"], [
         IfElse (
           Prefix (Operator.Prefix.Not, (Infix (Identifier "this",
                                         Operator.Instanceof,
                                         Identifier "Foo"))), [
           Return (
             Prefix (
               Operator.Prefix.New,
               Call (
                 Member (
                   Member (
                     Member(
                       Identifier "Function",
                       String "prototype"),
                     String "bind"),
                   String "apply"),
                 [Identifier "Foo"; Identifier "arguments"])));
         ],
         []);

         Term (Call (a, []));
         Var ("b", b);

         Term (Infix (Member (Identifier "this", String "b"),
                      Operator.Assignment,
                      b));
       ])))

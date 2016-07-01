let (test), (=>) = Test.(test, (=>))

module Py = Python
module V = Syntax

let term = ToPython.Term.compile
let item = ToPython.compile

let () = test "Atoms" @@ fun () ->
  term (V.Number 1) => Py.Number 1.;
  term (V.String "s") => Py.String "s";
  term (V.Identifier "s") => Py.Identifier "s"

let () = test "Case function" @@ fun () ->
  term V.(CaseFunction [("parameter", a)])
    => Py.(Lambda (["parameter"], a))

let () = test "If-else" @@ fun () ->
  term V.(IfElse (a, b, c))
    => Py.(IfElse {consequence=b; condition=a; alternative=c})

let () = test "Let in" @@ fun () ->
  term V.(LetIn ("a", b, c))
    => Py.(Call (Lambda (["a"], c), [b]))

let () = test "Infix" @@ fun () ->
  term V.(Infix (a, Times, b))
    => Py.(Infix (a, Operator.Times, b))

let () = test "Call" @@ fun () ->
  term V.(Call (a, Tuple []))
    => Py.(Call (a, []));
  term V.(Call (a, b))
    => Py.(Call (a, [b]));
  term V.(Call (a, Tuple [b; c]))
    => Py.(Call (a, [b; c]))

let () = test "Member" @@ fun () ->
  term V.(Member (a, "b"))
    => Py.(Member (a, "b"))

(* ITEM *)

let () = test "Let" @@ fun () ->
  item V.(Let (("a", "T"), None, b))
    => Py.(Assignment (a, b));
  item V.(Let (("a", "T"), Some [], b))
    => Py.(Def (None, "a", [], [
         Return b;
       ]));
  item V.(Let (("a", "T"), Some [("b", "T"); ("c", "T")], d))
    => Py.(Def (None, "a", ["b"; "c"], [
         Return d;
       ]))

let () = test "Do" @@ fun () ->
  item V.(Do (Call (a, Tuple [])))
    => Py.(Term (Call (a, [])))

let () = test "Module" @@ fun () ->
  item V.(Module ("foo", []))
    => Py.(Def (Some "apply", "foo", [], [
         Return (Call (Call (Identifier "type", [
           String "foo";
           Tuple [];
           Dict [];
         ]), []))
       ]));
  item V.(Module ("foo", [
    Let (("a", "T"), None, b);
    Let (("b", "T"), None, c);
  ]))
    => Py.(Def (Some "apply", "foo", [], [
         Assignment (a, b);
         Assignment (b, c);
         Return (Call (Call (Identifier "type", [
           String "foo";
           Tuple [];
           Dict [
             (String "a", a);
             (String "b", b);
           ];
         ]), []))
       ]))

let () = test "Class" @@ fun () ->
  item V.(Class ("Foo", [("a", "T"); ("b", "T")], [
    Do (Call (a, Tuple []));
    Let (("b", "T"), None, b);
  ]))
    => Py.(Class ("Foo", "object", [
         Def (None, "__init__", ["self"; "a"; "b"], [
           Term (Call (a, []));
           Assignment (b, b);

           Assignment (Member (Identifier "self", "b"), b);
         ])
       ]));

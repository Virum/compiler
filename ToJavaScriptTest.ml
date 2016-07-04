let (test), (=>) = Test.(test, (=>))


module JS = JavaScript
module Op = JavaScript.Operator
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
  term V.(Call (a, Tuple []))
    => JS.(Call (a, []));
  term V.(Call (a, b))
    => JS.(Call (a, [b]));
  term V.(Call (a, Tuple [b; c]))
    => JS.(Call (a, [b; c]))

let () = test "Member" @@ fun () ->
  term V.(Member (a, "b"))
    => JS.(Member (a, String "b"))

let () = test "Array" @@ fun () ->
  term V.(Array [a; b; c])
    => JS.(Array [a; b; c])

let () = test "Map[String, Any]" @@ fun () ->
  term V.(Map [])
    => JS.(Object []);
  term V.(Map [String "a", a; String "b", b])
    => JS.(Object ["a", a; "b", b])

let () = test "Map[Any, Any]" @@ fun () ->
  term V.(Map [a, b; c, d])
    => JS.(NewCall (Function (None, [], [
         Term (Infix (Member (Identifier "this", a), Operator.Assignment, b));
         Term (Infix (Member (Identifier "this", c), Operator.Assignment, d));
       ]), []))

(* ITEM *)

let () = test "Module" @@ fun () ->
  item V.(Module ("foo", []))
    => JS.(Var ("foo", Prefix (Operator.Prefix.New, (Call (
         Function (Some "foo", [], [

         ]),
       [])))));
  item V.(Module ("foo", [
    Let (("a", "T"), None, b);
    Let (("b", "T"), None, c);
  ]))
    => JS.(Var ("foo", Prefix (Op.Prefix.New, (Call (
         Function (Some "foo", [], [
           Var ("a", b);
           Var ("b", c);

           Term (Infix (Member (id "this", String "a"), Op.Assignment, id "a"));
           Term (Infix (Member (id "this", String "b"), Op.Assignment, id "b"));
         ]),
       [])))));
  item V.(Module ("foo", [
    Module ("bar", [])
  ]))
    => JS.(Var ("foo", Prefix (Operator.Prefix.New, (Call (
         Function (Some "foo", [], [

           JS.(Var ("bar", Prefix (Operator.Prefix.New, (Call (
             Function (Some "bar", [], [

             ]),
           [])))));

           Term (Infix (Member (id "this", String "bar"),
                        Op.Assignment, id "bar"));
         ]),
       [])))));
  item V.(Module ("foo", [
    Do (Call (Identifier "factorial", Number 5));
  ]))
    => JS.(Var ("foo", Prefix (Operator.Prefix.New, (Call (
         Function (Some "foo", [], [
           Term (Call (Identifier "factorial", [Number 5.]));
         ]),
       [])))))

let () = test "Let" @@ fun () ->
  item V.(Let (("a", "T"), None, b))
    => JS.(Var ("a", b));
  item V.(Let (("a", "T"), Some [], b))
    => JS.(Term (Function (Some "a", [], [Return b])));
  item V.(Let (("a", "T"), Some [("b", "T"); ("c", "T")], d))
    => JS.(Term (Function (Some "a", ["b"; "c"], [Return d])))

let () = test "Class" @@ fun () ->
  item V.(Class ("Foo", [("a", "T"); ("b", "T")], [
    Do (Call (a, Tuple []));
    Let (("b", "T"), None, b);
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

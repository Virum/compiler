let (test), (=>) = Test.(test, (=>))
open JavaScript
module Op = JavaScript.Operator

let (=>>) left right = print_newline (); print_endline left; left => right

let () = test "Number" @@ fun () ->
  to_string (Number 1.2) => "1.2"

let () = test "String" @@ fun () ->
  to_string (String "s") => {|"s"|};
  to_string (String "hello\"world") => {|"hello\"world"|};
  to_string (String "hello\nworld") => {|"hello\nworld"|};
  to_string (String "hello\x00world") => {|"hello\u0000world"|}

let () = test "Infix" @@ fun () ->
  to_string (Infix (Number 1.0, Op.Plus, Number 2.0))
    => "1. + 2.";
  to_string (Infix (Number 1.0,
                    Op.Plus,
                    (Infix (Number 2.0,
                            Op.Times,
                            Number 3.0))))
    => "1. + 2. * 3.";
  to_string (Infix (Infix (Number 1.0,
                           Op.Plus,
                           Number 2.0),
                    Op.Times,
                    Number 3.0))
    => "(1. + 2.) * 3."

let () = test "Call" @@ fun () ->
  to_string (Call (a, []))
    => "a()";
  to_string (Call (Infix (a, Op.Plus, b), []))
    => "(a + b)()";
  to_string (Call (a, [b; c; d]))
    => "a(b, c, d)"

let () = test "Member access" @@ fun () ->
  to_string (Member (a, b))
    => "a[b]";
  to_string (Member (a, String "b"))
    => "a.b";
  to_string (Member (a, String "?"))
    => "a[\"?\"]"

let () = test "Function" @@ fun () ->
  to_string (Function (None, [], []))
    => "function () {}";
  to_string (Function (Some "rec", [], []))
    => "function rec() {}";
  to_string (Function (None, ["a"; "b"; "c"], []))
    => "function (a, b, c) {}";
  statement_to_string (Term (Function (None, [], [Term a; Term b; Term c])))
    => "\
function () {
  a;
  b;
  c;
};"

let () = test "If-else" @@ fun () ->
  statement_to_string (IfElse (a, [], []))
    => "if (a) {} else {}";
  statement_to_string (IfElse (a, [Term b; Term c], [Term d]))
    => "\
if (a) {
  b;
  c;
} else {
  d;
}";
  statement_to_string (IfElse (a, [Term b], [IfElse (c, [Term d], [Term a])]))
    => "\
if (a) {
  b;
} else if (c) {
  d;
} else {
  a;
}";
  statement_to_string (
    IfElse (a, [Term b], [
      IfElse (c, [Term d], [
        IfElse (a, [Term b], [Term c])
      ])
    ])
  )
    => "\
if (a) {
  b;
} else if (c) {
  d;
} else if (a) {
  b;
} else {
  c;
}"

let () = test "Return" @@ fun () ->
  statement_to_string (Return a)
    => "return a;"

let () = test "Variable declaration" @@ fun () ->
  statement_to_string (Var ("x", a))
    => "var x = a;"

let () = test "Object literal" @@ fun () ->
  to_string (Object [])
    => "{}";
  to_string (Object [("x", a); ("y", b)])
    => "{x: a, y: b}";
  to_string (Object [("object_too_long_to_fit_on_one_line", a);
                     ("object_too_long_to_fit_on_one_line", b)])
    => "{
  object_too_long_to_fit_on_one_line: a,
  object_too_long_to_fit_on_one_line: b
}"

let () = test "Integration" @@ fun () ->
  statement_to_string (Term (
    Call (
      Function (None, ["factorial"], [
        Return (Call (
          Member (Identifier "console", String "log"),
          [Call (Identifier "factorial", [Number 5.])]
        ))
      ]),
      [
        Function (Some "factorial", ["n"], [
          Return (
            Call (
              Function (None, [], [
                IfElse (Infix (Identifier "n", Op.Equal, Number 0.), [
                  Return (Number 1.)
                ], (* else *) [
                  Return (Infix (
                    Call (Identifier "factorial", [
                      Infix (Identifier "n", Op.Minus, Number 1.)
                    ]),
                    Op.Times,
                    Identifier "n"
                  ))
                ])
              ]),
              []
            )
          )
        ])
      ]
    )
  )) => "\
(function (factorial) {
  return console.log(factorial(5.));
})(function factorial(n) {
  return (function () {
    if (n == 0.) {
      return 1.;
    } else {
      return factorial(n - 1.) * n;
    }
    })();
  });"
(* Wish:
  (function (factorial) {
    return console.log(factorial(5));
  })(function factorial(n) {
    return (function () {
      if (n == 0) {
        return 1;
      } else {
        return factorial(n - 1) * n;
      }
    })();
  })
*)

let () = test "Integration: Mail client" @@ fun () ->
  statement_to_string (
    Var ("Client", (Function (Some "Client", ["api_key"], [
      Var ("api_key", Identifier "api_key");
      Var ("send", (Function (None, ["mail"], [
        Return (Call (Member (Identifier "console", String "log"), [
          Infix (String "sent to ", Op.Plus, Identifier "mail_to");
        ]));
      ])));
      Return (Object [
        ("api_key", Identifier "api_key");
        ("send", Identifier "send");
      ]);
    ])))
  ) => "\
var Client = function Client(api_key) {
  var api_key = api_key;
  var send = function (mail) {
    return console.log(\"sent to \" + mail_to);
  };
  return {api_key: api_key, send: send};
};"

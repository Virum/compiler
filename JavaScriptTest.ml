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
  to_string (Function (None, [], [Term a; Term b; Term c]))
    => "function () {
  a;
  b;
  c;
}"

let () = test "If-else" @@ fun () ->
  to_string (Function (None, [], [IfElse (a, [], [])]))
    => "function () {
  if (a) {} else {};
}";
  to_string (Function (None, [], [IfElse (a, [Term b; Term c], [Term d])]))
    => "function () {
  if (a) {
    b;
    c;
  } else {
    d;
  };
}";
  to_string (Function (None, [], [
    IfElse (a, [Term b], [IfElse (c, [Term d], [Term a])])
  ]))
    => "function () {
  if (a) {
    b;
  } else if (c) {
    d;
  } else {
    a;
  };
}";
  to_string (Function (None, [], [
    IfElse (a, [Term b], [
      IfElse (c, [Term d], [
        IfElse (a, [Term b], [Term c])
      ])
    ])
  ]))
    => "function () {
  if (a) {
    b;
  } else if (c) {
    d;
  } else if (a) {
    b;
  } else {
    c;
  };
}"

let () = test "Return" @@ fun () ->
  to_string (Function (None, [], [Return a]))
    => "function () {
  return a;
}"

let () = test "Variable declaration" @@ fun () ->
  to_string (Function (None, [], [
    Var ("x", a)
  ]))
    => "function () {
  var x = a;
}"

let () = test "Integration" @@ fun () ->
  to_string (
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
  ) => "\
(function (factorial) {
   return console.log(factorial(5.));
 })(function factorial(n) {
      return (function () {
                if (n == 0.) {
                  return 1.;
                } else {
                  return factorial(n - 1.) * n;
                };
              })();
    })"
(*
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

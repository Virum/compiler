open Core_kernel.Std

let test, (=>) = Test.(test, (=>))
open JavaScript
module Op = JavaScript.Operator

let (=>>) left right = print_newline (); print_endline left; left => right

let () = test "Number" @@ fun () ->
  to_string (Term (Number 1.2)) => "1.2;";
  to_string (Term (Number 10.)) => "10;";
  to_string (Term (Number Float.infinity)) => "Infinity;";
  to_string (Term (Number Float.neg_infinity)) => "-Infinity;";
  to_string (Term (Number Float.nan)) => "NaN;";
  to_string (Term (Number (1. /. 3.))) => "0.33333333333333331;";
  to_string (Term (Number 1e23)) => "1e+23;"

let () = test "String" @@ fun () ->
  to_string (Term (String "s")) => {|"s";|};
  to_string (Term (String "hello\"world")) => {|"hello\"world";|};
  to_string (Term (String "hello\nworld")) => {|"hello\nworld";|};
  to_string (Term (String "hello\x00world")) => {|"hello\u0000world";|}

let () = test "Infix" @@ fun () ->
  let (+) left right = Infix (left, Op.Plus, right) in
  let (/) left right = Infix (left, Op.Divide, right) in
  to_string (Term (a + b))
    => "a + b;";
  to_string (Term (a + b / c))
    => "a + b / c;";
  to_string (Term ((a + b) / c))
    => "(a + b) / c;";
  to_string (Term (a / b / c))
    => "a / b / c;";
  to_string (Term (a / (b / c)))
    => "a / (b / c);";
  to_string (Term (a / (b / (c / d))))
    => "a / (b / (c / d));"

let () = test "Call" @@ fun () ->
  to_string (Term (Call (a, [])))
    => "a();";
  to_string (Term (Call (Infix (a, Op.Plus, b), [])))
    => "(a + b)();";
  to_string (Term (Call (a, [b; c; d])))
    => "a(b, c, d);";
  to_string (Term (Call (a, [
    Identifier "parameters_too_long_to_fit_on_a_single_line";
    Identifier "parameters_too_long_to_fit_on_a_single_line";
  ]))) => "\
a(parameters_too_long_to_fit_on_a_single_line,
  parameters_too_long_to_fit_on_a_single_line);"

let () = test "NewCall" @@ fun () ->
  to_string (Term (NewCall (a, [])))
    => "new a();"

let () = test "Prefix" @@ fun () ->
  to_string (Term (Prefix (Operator.Prefix.Not, a)))
    => "!a;";
  to_string (Term (Prefix (Operator.Prefix.Typeof, a)))
    => "typeof a;";
  to_string (Term (Prefix (Operator.Prefix.New, a)))
    => "new a;"

let () = test "Member access" @@ fun () ->
  to_string (Term (Member (a, b)))
    => "a[b];";
  to_string (Term (Member (a, String "b")))
    => "a.b;";
  to_string (Term (Member (a, String "?")))
    => "a[\"?\"];"

let () = test "Array" @@ fun () ->
  to_string (Term (Array []))
    => "[];";
  to_string (Term (Array [a; b; c]))
    => "[a, b, c];";
  to_string (Term (Array [
    Identifier "this_term_too_long_to_fit_in_one_line";
    Identifier "this_term_too_long_to_fit_in_one_line";
  ]))
    => "[
  this_term_too_long_to_fit_in_one_line,
  this_term_too_long_to_fit_in_one_line
];"

let () = test "Muli-line definition" @@ fun () ->
  to_string (Var ("x", (Array [
    Identifier "this_term_too_long_to_fit_in_one_line";
    Identifier "this_term_too_long_to_fit_in_one_line";
    Array [
      Identifier "this_term_too_long_to_fit_in_one_line";
      Identifier "this_term_too_long_to_fit_in_one_line";
    ];
  ])))
    => "\
var x = [
          this_term_too_long_to_fit_in_one_line,
          this_term_too_long_to_fit_in_one_line,
          [
            this_term_too_long_to_fit_in_one_line,
            this_term_too_long_to_fit_in_one_line
          ]
        ];"


let () = test "Function" @@ fun () ->
  to_string (Term (Function (None, [], [])))
    => "function () {};";
  to_string (Term (Function (Some "rec", [], [])))
    => "function rec() {};";
  to_string (Term (Function (None, ["a"; "b"; "c"], [])))
    => "function (a, b, c) {};";
  to_string (Term (Function (None, [], [Term a; Term b; Term c])))
    => "\
function () {
  a;
  b;
  c;
};"

let () = test "Include" @@ fun () ->
  to_string (Term (Function (None, [], [
    Include ["anything"; "goes"];
  ])))
    => "function () {\n  \n  anything\n  goes\n};";
  to_string (Term (Function (None, [], [
    Term a;
    Include ["anything"; "goes"];
    Term b;
  ])))
    => "\
function () {
  a;
  anything
  goes
  b;
};"

let () = test "If-else" @@ fun () ->
  to_string (IfElse (a, [], []))
    => "if (a) {} else {}";
  to_string (IfElse (a, [Term b; Term c], [Term d]))
    => "\
if (a) {
  b;
  c;
} else {
  d;
}";
  to_string (IfElse (a, [Term b], [IfElse (c, [Term d], [Term a])]))
    => "\
if (a) {
  b;
} else if (c) {
  d;
} else {
  a;
}";
  to_string (
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
  to_string (Return a)
    => "return a;"

let () = test "Variable declaration" @@ fun () ->
  to_string (Var ("x", a))
    => "var x = a;"

let () = test "Object literal" @@ fun () ->
  to_string (Term (Object []))
    => "{};";
  to_string (Term (Object [("x", a); ("y", b)]))
    => "{x: a, y: b};";
  to_string (Term (Object [("object_too_long_to_fit_on_one_line", a);
                     ("object_too_long_to_fit_on_one_line", b)]))
    => "{
  object_too_long_to_fit_on_one_line: a,
  object_too_long_to_fit_on_one_line: b
};"

let () = test "Object literal with non-identifier keys" @@ fun () ->
  to_string (Term (Object [("!@#", a); ("y", b)]))
    => "{\"!@#\": a, y: b};"

let () = test "Integration" @@ fun () ->
  to_string (Term (
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
  return console.log(factorial(5));
})(function factorial(n) {
   return (function () {
     if (n == 0) {
       return 1;
     } else {
       return factorial(n - 1) * n;
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
  to_string (
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

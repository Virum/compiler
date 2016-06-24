open Syntax
let (test), (=>) = Test.(test, (=>))

let term = Parser.Term.parse
let items = Parser.Items.parse

let () = test "comment" @@ fun () ->
  term "foo # comment" => Ok (Identifier "foo")

let () = test "identifier" @@ fun () ->
  term "foo" => Ok (Identifier "foo")

let () = test "String literals" @@ fun () ->
  term {| "\x41 \x61 \n \r \t \" \\" |}
    => Ok (String "A a \n \r \t \" \\");
  term {| "\x??" |}
    => Error (`Bad_escape_sequence ((1, 6), "\\x??"));
  term {| "\?" |}
    => Error (`Bad_escape_sequence ((1, 4), "\\?"))

let () = test "Number literals" @@ fun () ->
  term "123" => Ok (Number 123)

(* let () = test "Number literals" @@ fun () -> *)

let id x = Identifier x
let foo, bar, baz = id "foo", id "bar", id "baz"
let a, b, c, d, e, f = id "a", id "b", id "c", id "d", id "e", id "f"

let () = test "Case function" @@ fun () ->
  term "case foo: bar"
    => Ok (CaseFunction [("foo", bar)]);
  term "case foo: bar
        case baz: foo"
    => Ok (CaseFunction [("foo", bar); ("baz", foo)]);
  term "case foo: case a: b
        case baz: foo"
    => Error (`Syntax_error (1, 14)); (* nested cases require parenthesis *)
  term "case foo: (case a: b)
        case baz: foo"
    => Ok (CaseFunction [("foo", (CaseFunction [("a", b)])); ("baz", foo)])

let () = test "Switch case" @@ fun () ->
  term "switch a
          case b: c
          case d: e"
    => Ok (Switch (a, [("b", c); ("d", e)]))

let () = test "If-then-else" @@ fun () ->
  term "if a then b else c"
    => Ok (IfElse (a, b, c));
  term "if a then b else case c: d"
    => Ok (IfElse (a, b, CaseFunction [("c", d)]))

let () = test "LetIn" @@ fun () ->
  term "let a = b in c"
    => Ok (LetIn ("a", b, c))

let () = test "Infix operators" @@ fun () ->
  term "a + b"     => Ok (Infix (a, Plus, b));
  term "a + b * c" => Ok (Infix (a, Plus, Infix (b, Times, c)));
  term "a * b + c" => Ok (Infix (Infix (a, Times, b), Plus, c))

let () = test "Call" @@ fun () ->
  term "a()"
    => Ok (Call (a, []));
  term "a(b)"
    => Ok (Call (a, [b]));
  term "a(b, c)"
    => Ok (Call (a, [b; c]));
  term "a(b, c,)"
    => Ok (Call (a, [b; c]));
  term "a(b)(c)"
    => Ok (Call (Call (a, [b]), [c]))

let () = test "Integration: factorial" @@ fun () ->
  term "
    let factorial = case n:
      if n == 0 then
        1
      else
        factorial(n - 1) * n
    in
      print(factorial(5))
  " => Ok (LetIn ("factorial", (CaseFunction ["n",
    IfElse (
      Infix (Identifier "n", Equal, Number 0),
      Number 1,
      Infix (Call (Identifier "factorial", [
        Infix (Identifier "n", Minus, Number 1)
      ]), Times, Identifier "n")
    )
  ]), Call (Identifier "print", [Call (Identifier "factorial", [Number 5])])))

let () = test "Items" @@ fun () ->
  items "let x = a
         let x = let y = a in b
         do a"
    => Ok [
      Let ("x", None, a);
      Let ("x", None, LetIn ("y", a, b));
      Do a;
    ]

let () = test "Let" @@ fun () ->
  items "let x = a"
    => Ok [Let ("x", None, a)];
  items "let x() = a"
    => Ok [Let ("x", Some [], a)];
  items "let x(a, b, c) = a"
    => Ok [Let ("x", Some ["a"; "b"; "c"], a)]

let () = test "Module" @@ fun () ->
  items "
    module foo {
      let x = a
      let x = let y = a in b
      do a
    }
  " => Ok [
         Module ("foo", [
           Let ("x", None, a);
           Let ("x", None, LetIn ("y", a, b));
           Do a;
         ])
       ]

let () = test "Class" @@ fun () ->
  items "class Foo() {}"
    => Ok [Class ("Foo", [], [])];
  items "class Foo(bar, baz) {}"
    => Ok [Class ("Foo", ["bar"; "baz"], [])];
  items "class Foo() {
    let x = a
    let y = b
  }" => Ok [Class ("Foo", [], [
    Let ("x", None, a);
    Let ("y", None, b);
  ])]

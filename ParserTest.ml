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

let () = test "Tuples" @@ fun () ->
  term "()"
    => Ok (Tuple []);
  term "(a)"
    => Ok a; (* not a tuple *)
  term "(a, b)"
    => Ok (Tuple [a; b]);
  term "(a, b,)"
    => Ok (Tuple [a; b]);
  term "(a, b, c)"
    => Ok (Tuple [a; b; c])

let () = test "Call" @@ fun () ->
  term "a()"
    => Ok (Call (a, Tuple []));
  term "a(b)"
    => Ok (Call (a, b));
  term "a(b, c)"
    => Ok (Call (a, Tuple [b; c]));
  term "a(b, c,)"
    => Ok (Call (a, Tuple [b; c]));
  term "a(b)(c)"
    => Ok (Call (Call (a, b), c))

let () = test "Member" @@ fun () ->
  term "a.b"
    => Ok (Member (a, "b"))

let () = test "Block" @@ fun () ->
  term "{ a }"
    => Ok a

let () = test "Map" @@ fun () ->
  term "{}"
    => Ok (Map []);
  term "{a: b}"
    => Ok (Map [a, b]);
  term "{a: b,}"
    => Ok (Map [a, b]);
  term "{a: b, c: d}"
    => Ok (Map [a, b; c, d]);
  term "{a: b, c: d,}"
    => Ok (Map [a, b; c, d])

let () = test "Array" @@ fun () ->
  term "[]"
    => Ok (Array []);
  term "[a]"
    => Ok (Array [a]);
  term "[a,]"
    => Ok (Array [a]);
  term "[a, b]"
    => Ok (Array [a; b]);
  term "[a, b,]"
    => Ok (Array [a; b])

let () = test "Syntax extension points: Json" @@ fun () ->
  term "Json.123"
    => Ok (Extension (Identifier "Json", Number 123));
  term "Json.{key: [a, b, 1, 2]}"
    => Ok (Extension (Identifier "Json", Map [
         Identifier "key", Array [a; b; Number 1; Number 2];
       ]))

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
      Infix (Call (Identifier "factorial",
        Infix (Identifier "n", Minus, Number 1)
      ), Times, Identifier "n")
    )
  ]), Call (Identifier "print", Call (Identifier "factorial", Number 5))))

let () = test "Items" @@ fun () ->
  items "let x: X = a
         let x: X = let y = a in b
         do a"
    => Ok [
      Let (("x", "X"), None, a);
      Let (("x", "X"), None, LetIn ("y", a, b));
      Do a;
    ]

let () = test "Let" @@ fun () ->
  items "let x: X = a"
    => Ok [Let (("x", "X"), None, a)];
  items "let x(): X = a"
    => Ok [Let (("x", "X"), Some [], a)];
  items "let x(a: A, b: B, c: C): X = a"
    => Ok [Let (("x", "X"), Some [("a", "A"); ("b", "B"); ("c", "C")], a)]

let () = test "Module" @@ fun () ->
  items "
    module foo {
      let x: X = a
      let x: X = let y = a in b
      do a
    }
  " => Ok [
         Module ("foo", [
           Let (("x", "X"), None, a);
           Let (("x", "X"), None, LetIn ("y", a, b));
           Do a;
         ])
       ]

let () = test "Class" @@ fun () ->
  items "class Foo() {}"
    => Ok [Class ("Foo", [], [])];
  items "class Foo(bar: Bar, baz: Baz) {}"
    => Ok [Class ("Foo", [("bar", "Bar"); ("baz", "Baz")], [])];
  items "class Foo() {
    let x: X = a
    let y: Y = b
  }" => Ok [Class ("Foo", [], [
    Let (("x", "X"), None, a);
    Let (("y", "Y"), None, b);
  ])]

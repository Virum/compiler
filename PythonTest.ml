open Core_kernel.Std

let test, (=>) = Test.(test, (=>))
open Python
module Op = Python.Operator

let (=>>) left right = left => right; print_endline left

let () = test "Number" @@ fun () ->
  to_string (Term (Number 1.2)) => "1.2";
  to_string (Term (Number 10.)) => "10.0";
  to_string (Term (Number Float.infinity)) => "float('inf')";
  to_string (Term (Number Float.neg_infinity)) => "float('-inf')";
  to_string (Term (Number Float.nan)) => "float('nan')";
  to_string (Term (Number (1. /. 3.))) => "0.33333333333333331";
  to_string (Term (Number 1e23)) => "1e+23"

let () = test "String" @@ fun () ->
  to_string (Term (String "s")) => {|"s"|}

let () = test "Infix" @@ fun () ->
  to_string (Term (Infix (a, Op.Plus, b)))
    => "a + b"

let () = test "Call" @@ fun () ->
  to_string (Term (Call (a, [])))
    => "a()";
  to_string (Term (Call (Infix (a, Op.Plus, b), [])))
    => "(a + b)()";
  to_string (Term (Call (a, [b; c; d])))
    => "a(b, c, d)";
  to_string (Term (Call (a, [
    Identifier "parameters_too_long_to_fit_on_a_single_line";
    Identifier "parameters_too_long_to_fit_on_a_single_line";
  ]))) => "\
a(parameters_too_long_to_fit_on_a_single_line,
  parameters_too_long_to_fit_on_a_single_line)"

let () = test "Member access" @@ fun () ->
  to_string (Term (Member (a, "b")))
    => "a.b"

let () = test "Dict" @@ fun () ->
  to_string (Term (Dict []))
    => "{}";
  to_string (Term (Dict [(a, b); (c, d)]))
    => "{a: b, c: d}";
  to_string (Term (Dict [(String "object_too_long_to_fit_on_one_line", a);
                         (String "object_too_long_to_fit_on_one_line", b)]))
    => {|{
    "object_too_long_to_fit_on_one_line": a,
    "object_too_long_to_fit_on_one_line": b
}|}

let () = test "Tuple" @@ fun () ->
  to_string (Term (Tuple []))
    => "()";
  to_string (Term (Tuple [a]))
    => "(a,)";
  to_string (Term (Tuple [a; b; c]))
    => "(a, b, c)";
  to_string (Term (Tuple [
    Identifier "this_tuple_is_just_too_long_for_one_line";
    Identifier "this_tuple_is_just_too_long_for_one_line";
  ]))
    => "(
    this_tuple_is_just_too_long_for_one_line,
    this_tuple_is_just_too_long_for_one_line
)"

let () = test "Lambda" @@ fun () ->
  to_string (Term (Lambda ([], a)))
    => "lambda: a";
  to_string (Term (Lambda (["x"], a)))
    => "lambda x: a";
  to_string (Term (Lambda (["x"; "y"; "z"], a)))
    => "lambda x, y, z: a"

let () = test "If-else" @@ fun () ->
  to_string (Term (IfElse {consequence=a; condition=b; alternative=c}))
    => "a if b else c"

let () = test "Return" @@ fun () ->
  to_string (Return a)
    => "return a"

let () = test "Assignment" @@ fun () ->
  to_string (Assignment (a, b))
    => "a = b";
  to_string (Assignment (Member (a, "b"), c))
    => "a.b = c"

let () = test "Def" @@ fun () ->
  to_string (Def (None, "foo", [], []))
    => "\
def foo():
    pass";
  to_string (Def (None, "foo", ["a"; "b"; "c"], []))
    => "\
def foo(a, b, c):
    pass";
  to_string (Def (None, "foo", ["a"; "b"; "c"], [Pass; Pass]))
    => "\
def foo(a, b, c):
    pass
    pass";
  to_string (Def (Some "apply", "foo", ["a"; "b"; "c"], []))
    => "\
@apply
def foo(a, b, c):
    pass"

let () = test "Class" @@ fun () ->
  to_string (Class ("foo", "object", [
    Def (None, "__init__", ["self"; "a"; "b"], [
      Pass
    ]);
    Def (None, "bar", ["self"], [
      Pass
    ]);
  ]))
    => "\
class foo(object):
    def __init__(self, a, b):
        pass

    def bar(self):
        pass"

let () = test "If-else" @@ fun () ->
  to_string (If (a, [], Else []))
    => "\
if a:
    pass
else:
    pass";
  to_string (If (a, [], Elif (b, [], Elif (c, [] ,Else []))))
    => "\
if a:
    pass
elif b:
    pass
elif c:
    pass
else:
    pass"

let () = test "Include" @@ fun () ->
  to_string (Def (None, "f", [], [
    Include ["anything"; "goes"]
  ]))
    => "\
def f():
    anything
    goes"


let () = test "Integration" @@ fun () ->
  let n = Identifier "n" in
  to_string (Def (None, "factorial", ["n"], [
    If (Infix (n, Op.Equal, Number 0.0), [
      Return (Number 1.0);
    ], Else [
      Return (Infix (Call (Identifier "factorial", [
        Infix (n, Op.Minus, Number 1.0)
      ]), Op.Times, n));
    ])
  ])) => "\
def factorial(n):
    if n == 0.0:
        return 1.0
    else:
        return factorial(n - 1.0) * n"

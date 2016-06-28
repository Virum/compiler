let test, (=>) = Test.(test, (=>))

open Type
module V = Syntax
let env = Table.of_alist

let infer tenv_alist env_alist term =
  infer (Table.of_alist_exn tenv_alist) (Table.of_alist_exn env_alist) term


let () = test "Atoms" @@ fun () ->

  infer [] [] V.(Number 42)
    => Ok Number;

  infer [] [] V.(String "s")
    => Ok String


let () = test "Identifier" @@ fun () ->

  infer [] ["x", Number] V.(Identifier "x")
    => Ok Number;

  infer [] [] V.(Identifier "x")
    => Error [`Unbound_identifier "x"]


let () = test "Tuple" @@ fun () ->

  infer [] [] V.(Tuple [])
    => Ok (Tuple []);

  infer [] [] V.(Tuple [Number 1; String "s"])
    => Ok (Tuple [Number; String]);

  infer [] [] V.(Tuple [Number 1; Identifier "i"; Identifier "j"])
    => Error [`Unbound_identifier "i"; `Unbound_identifier "j"]


let () = test "Infix" @@ fun () ->

  infer [] [] V.(Infix (Number 1, Plus, Number 2))
    => Ok Number;

  infer [] [] V.(Infix (Number 1, Plus, String "s"))
    => Error [`Parameter_type_does_not_match_argument];

  infer [] [] V.(Infix (Identifier "x", Plus, Number 2))
    => Error [`Unbound_identifier "x"];

  infer [] [] V.(Infix (Identifier "x", Plus, Identifier "y"))
    => Error [`Unbound_identifier "x"; `Unbound_identifier "y"]


let () = test "Call" @@ fun () ->

  infer [] ["f", Arrow (Tuple [], Tuple [])]
      V.(Call (Identifier "f", Tuple []))
    => Ok (Tuple []);

  infer [] []
      V.(Call (Identifier "f", Tuple []))
    => Error [`Unbound_identifier "f"];

  infer [] ["f", Number]
      V.(Call (Identifier "f", Tuple []))
    => Error [`Not_a_function];

  infer [] ["f", Arrow (Number, Tuple [])]
      V.(Call (Identifier "f", Tuple []))
    => Error [`Parameter_type_does_not_match_argument]

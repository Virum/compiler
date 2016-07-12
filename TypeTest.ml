let test, (=>) = Test.(test, (=>))

module Sexp = Sexplib.Sexp
open Type
module Env = Type.Environment
module V = Syntax

let env = Env.of_alist_exn
let term tenv_alist env_alist term =
  Term.infer {types=(env tenv_alist); values=(env env_alist)} term
let item tenv_alist env_alist term =
  infer {types=(env tenv_alist); values=(env env_alist)} term
(*   infer (env tenv_alist) (env env_alist) term *)


let () = test "Subtyping" @@ fun () ->

  is_subtype Number Number
    => true;

  is_subtype Number Any
    => true;

  is_subtype Any Number
    => false


let () = test "Subtyping: Tuples are covariant" @@ fun () ->

  is_subtype (Tuple []) (Tuple [])
    => true;

  is_subtype (Tuple []) (Tuple [Number; String])
    => false;

  is_subtype (Tuple [Number; String]) (Tuple [])
    => false;

  is_subtype (Tuple [Number; String]) (Tuple [Number; String])
    => true;

  is_subtype (Tuple [Number; String]) (Tuple [Any; Any])
    => true


let () = test "Subtyping: Functions: variance" @@ fun () ->

  is_subtype (Arrow (Number, Number)) (Arrow (Number, Number))
    => true;

  is_subtype (Arrow (Any, Number)) (Arrow (Number, Number))
    => true;

  is_subtype (Arrow (Number, Number)) (Arrow (Any, Number))
    => false;

  is_subtype (Arrow (Number, Any)) (Arrow (Number, Number))
    => false;

  is_subtype (Arrow (Number, Number)) (Arrow (Number, Any))
    => true


let () = test "Atoms" @@ fun () ->

  term [] [] V.(Number 42)
    => Ok Number;

  term [] [] V.(String "s")
    => Ok String


let () = test "Identifier" @@ fun () ->

  term [] ["x", Number] V.(Identifier "x")
    => Ok Number;

  term [] [] V.(Identifier "x")
    => Error [`Unbound_identifier "x"]


let () = test "Tuple" @@ fun () ->

  term [] [] V.(Tuple [])
    => Ok (Tuple []);

  term [] [] V.(Tuple [Number 1; String "s"])
    => Ok (Tuple [Number; String]);

  term [] [] V.(Tuple [Number 1; Identifier "i"; Identifier "j"])
    => Error [`Unbound_identifier "i"; `Unbound_identifier "j"]


let () = test "Infix" @@ fun () ->

  term [] [] V.(Infix (Number 1, Plus, Number 2))
    => Ok Number;

  term [] [] V.(Infix (Number 1, Plus, String "s"))
    => Error [`Parameter_type_does_not_match_argument];

  term [] [] V.(Infix (Identifier "x", Plus, Number 2))
    => Error [`Unbound_identifier "x"];

  term [] [] V.(Infix (Identifier "x", Plus, Identifier "y"))
    => Error [`Unbound_identifier "x"; `Unbound_identifier "y"]


let () = test "Call" @@ fun () ->

  term [] ["f", Arrow (Tuple [], Tuple [])]
      V.(Call (Identifier "f", Tuple []))
    => Ok (Tuple []);

  term [] []
      V.(Call (Identifier "f", Tuple []))
    => Error [`Unbound_identifier "f"];

  term [] ["f", Number]
      V.(Call (Identifier "f", Tuple []))
    => Error [`Not_a_function];

  term [] ["f", Arrow (Number, Tuple [])]
      V.(Call (Identifier "f", Tuple []))
    => Error [`Parameter_type_does_not_match_argument]


let () = test "If-else" @@ fun () ->

  term [] ["a", Boolean; "b", String; "c", String]
      V.(IfElse (a, b, c))
    => Ok String;

  term [] ["a", String; "b", String; "c", String]
      V.(IfElse (a, b, c))
    => Error [`If_condition_not_boolean];

  term [] ["a", Boolean; "b", Number; "c", String]
      V.(IfElse (a, b, c))
    => Error [`If_consequence_and_alternative_types_do_not_match];

  term [] []
      V.(IfElse (a, b, c))
    => Error [
         `Unbound_identifier "a";
         `Unbound_identifier "b";
         `Unbound_identifier "c";
       ];

  term [] ["a", String]
      V.(IfElse (a, b, c))
    => Error [
         `If_condition_not_boolean;
         `Unbound_identifier "b";
         `Unbound_identifier "c";
       ];

  term [] ["b", Number; "c", String]
      V.(IfElse (a, b, c))
    => Error [
         `Unbound_identifier "a";
         `If_consequence_and_alternative_types_do_not_match;
       ]


let () = test "Let-in" @@ fun () ->

  term [] ["a", Boolean; "b", String]
      V.(LetIn ("x", a, b))
    => Ok String;

  term [] ["b", Boolean]
      V.(LetIn ("a", b, a))
    => Ok Boolean;

  term [] ["b", Boolean]
      V.(LetIn ("a", b, Tuple [Number 1; a]))
    => Ok (Tuple [Number; Boolean])


let () = test "Array[T]" @@ fun () ->

  term [] ["b", Boolean; "s", String]
      V.(Array [])
    => Error [`Empty_array_needs_type_annotation];

  term [] ["b", Boolean; "s", String]
      V.(Array [b])
    => Ok (Array Boolean);

  term [] ["b", Boolean; "s", String]
    V.(Array [b; b])
    => Ok (Array Boolean);

  term [] ["b", Boolean; "a", String]
    V.(Array [b; a; Identifier "bogus"])
    => Error [`Unbound_identifier "bogus"];

  term [] ["b", Boolean; "a", String]
    V.(Array [b; a])
    => Error [`Heterogeneous_array ["Boolean"; "String"]]


let () = test "Map[K, V]" @@ fun () ->

  term [] []
      V.(Map [])
    => Error [`Empty_map_needs_type_annotation];

  term [] ["a", Boolean; "b", String]
      V.(Map [a, b])
    => Ok (Map (Boolean, String));

  term [] ["a", Boolean; "b", String]
    V.(Map [a, b; a, b])
    => Ok (Map (Boolean, String));

  term [] ["b", Boolean; "a", String]
    V.(Map [b, a; a, b])
    => Error [`Heterogeneous_map ["Boolean", "String"; "String", "Boolean"]]


let () = test "Member" @@ fun () ->

  term [] []
    V.(Member (a, "member"))
    => Error [`Unbound_identifier "a"];

  term [] ["a", String]
    V.(Member (a, "member"))
    => Error [`Member_does_not_belong ("member", "String")];

  term [] ["a", Module ("M", {types=env []; values=env ["member", Number]})]
    V.(Member (a, "bogus"))
    => Error [`Member_does_not_belong ("bogus", "module M")];

  term [] ["a", Module ("M", {types=env []; values=env ["member", Number]})]
    V.(Member (a, "member"))
    => Ok Number


(* Items *)

let () = test "Do" @@ fun () ->

  item [] []
      V.(Do (Tuple []))
    => Ok (Tuple []);

  item [] ["a", Arrow (Number, Tuple [])]
      V.(Do (Call (a, Number 1)))
    => Ok (Tuple []);

  item [] ["a", Arrow (Number, Number)]
      V.(Do (Call (a, Number 1)))
    => Error [`Do_should_evaluate_to_unit]


let () = test "Let without parameters" @@ fun () ->

  item ["Number", Number] []
      V.(Let (("a", "Number"), None, Number 1))
    => Ok Number;

  item ["Number", Number] []
      V.(Let (("a", "Number"), None, a))
    => Error [`Unbound_identifier "a"];

  item [] []
      V.(Let (("a", "Bogus"), None, Number 1))
    => Error [`Cannot_find_type "Bogus"];

  item ["Number", Number] []
      V.(Let (("a", "Number"), None, String "s"))
    => Error [`Declared_type_does_not_match_real_one "Number"];

  item [] []
      V.(Let (("a", "Bogus"), None, a))
    => Error [
         `Cannot_find_type "Bogus";
         `Unbound_identifier "a";
       ]


let () = test "Let with parameters" @@ fun () ->

  item ["Number", Number] []
      V.(Let (("a", "Number"), Some [], Number 1))
    => Ok (Arrow (Tuple [], Number));

  item ["Number", Number] []
      V.(Let (("a", "Number"), Some ["b", "Number"], Number 1))
    => Ok (Arrow (Number, Number));

  item ["Number", Number] []
     V.(Let (("a", "Number"), Some ["b", "Bogus"], Number 1))
    => Error [`Cannot_find_type "Bogus"];

  item ["Number", Number] []
      V.(Let (("a", "Number"),
              Some ["b", "Number"; "c", "Number"],
              Number 1))
    => Ok (Arrow (Tuple [Number; Number], Number));

  item ["Number", Number] []
      V.(Let (("a", "Number"),
              Some ["b", "Bogus"; "c", "Phony"; "d", "Number"],
              Number 1))
    => Error [
         `Cannot_find_type "Bogus";
         `Cannot_find_type "Phony";
       ];

  item ["Number", Number] []
      V.(Let (("a", "Dummy"),
              Some ["b", "Bogus"; "c", "Phony"; "d", "Number"],
              Number 1))
    => Error [
         `Cannot_find_type "Dummy";
         `Cannot_find_type "Bogus";
         `Cannot_find_type "Phony";
       ]


let () = test "Let with parameters, test inner bindings" @@ fun () ->

  item ["Number", Number] []
      V.(Let (("a", "Number"), Some ["b", "Number"], b))
    => Ok (Arrow (Number, Number));

  item ["Number", Number] []
      V.(Let (("a", "Number"),
              Some ["b", "Number"; "c", "Number"],
              Infix (b, Plus, c)))
    => Ok (Arrow (Tuple [Number; Number], Number));

  item ["Number", Number; "Point", Tuple [Number; Number]] []
      V.(Let (("a", "Point"),
              Some ["b", "Number"; "c", "Number"],
              Tuple [b; c]))
    => Ok (Arrow (Tuple [Number; Number], Tuple [Number; Number]));


  item ["Number", Number; "String", String] []
      V.(Let (("a", "Number"),
              Some ["b", "Number"; "b", "Number"],
              b))
    => Error [`Duplicate_parameter_name "b"]


let (=>) left right =
  match left with
  | Ok type_ -> Test.Assertion.create Type.sexp_of_t type_ right
  | Error _ -> failwith "Left hand side of assertion is an Error"


let (=>!) left right =
  match left with
  | Ok type_ ->
      Printf.printf "\nExpected error, bug got Ok: %s"
        (Sexp.to_string_hum (Type.sexp_of_t type_));
      assert false
  | Error error -> Test.(=>) error right


let () = test "Module" @@ fun () ->

  item ["Number", Number] []
      V.(Module ("Foo", []))
    => Module ("Foo", {types=env []; values=env []});

  item ["Number", Number] []
      V.(Module ("Foo", [
        Do (Tuple []);
      ]))
    => Module ("Foo", {types=env []; values=env []});

  item ["Number", Number] []
      V.(Module ("Foo", [
        Let (("a", "Number"), None, Number 1);
      ]))
    => Module ("Foo", {types=env []; values=env ["a", Number]})


let () = test "Module: first binding is available inside second" @@ fun () ->

  item ["Number", Number] []
      V.(Module ("Foo", [
        Let (("a", "Number"), None, Number 1);
        Let (("b", "Number"), None, a);
      ]))
    => Module ("Foo", {types=env []; values=env [
         "a", Number;
         "b", Number;
       ]})


let () = test "Module: error (currently) short-cirquits" @@ fun () ->

  item ["Number", Number] []
      V.(Module ("Foo", [
        Let (("a", "Number"), None, Identifier "bogus");
        Let (("b", "Number"), None, Identifier "phony");
      ]))
    =>! [`Unbound_identifier "bogus"]


let () = test "Module: defines a new module type" @@ fun () ->

  item ["Number", Number] []
      V.(Module ("Foo", [
        Let (("a", "Number"), None, Number 1);
        Module ("Bar", []);
      ]))
    => Module ("Foo", {types=env [
         "Bar", Module ("Bar", {types=env []; values=env []});
       ]; values=env [
         "a", Number;
         "Bar", Module ("Bar", {types=env []; values=env []});
       ]})

let () = test "Module: creating alias" @@ fun () ->

  item ["Number", Number] []
      V.(Module ("Foo", [
        Let (("a", "Number"), None, Number 1);
        Module ("Bar", []);
        Let (("BarAlias", "Bar"), None, Identifier "Bar");
      ]))
    => Module ("Foo", {types=env [
         "Bar", Module ("Bar", {types=env []; values=env []});
         "BarAlias", Module ("Bar", {types=env []; values=env []});
       ]; values=env [
         "a", Number;
         "Bar", Module ("Bar", {types=env []; values=env []});
         "BarAlias", Module ("Bar", {types=env []; values=env []});
       ]})

let test, (=>) = Test.(test, (=>))
open Syntax


let rec compile_term = function
  | Extension (Identifier "Json", _) -> Ok (Identifier "yay")
  | Extension _ -> Error [`Invalid_syntax_extension]
  | other -> Traversal.Term.compile ~f:compile_term other


let compile = Traversal.compile ~f:compile_term


let () = test "Item traversal" @@ fun () ->
  compile (Module ("Foo", [
    Do (Extension (Identifier "Json", b));
  ]))
    => Ok (Module ("Foo", [
      Do (Identifier "yay");
    ]))


let () = test "Json" @@ fun () ->

  compile_term (Extension (Identifier "Foo", a))
    => Error [`Invalid_syntax_extension];

  compile_term (Extension (Identifier "Json", a))
    => Ok (Identifier "yay");

  compile_term (Map [a, Extension (Identifier "Json", b)])
    => Ok (Map [a, Identifier "yay"])

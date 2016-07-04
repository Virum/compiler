let test, (=>) = Test.(test, (=>))

open Syntax
let compile = Extensions.Json.compile


let () = test "Json" @@ fun () ->

  compile (Extension (Identifier "Foo", a))
    => Error [`Invalid_syntax_extension];

  compile (Extension (Identifier "Json", a))
    => Ok (Identifier "yay");

  compile (Map [a, Extension (Identifier "Json", b)])
    => Ok (Map [a, Identifier "yay"])

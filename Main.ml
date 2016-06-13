




let (>>=) result callback =
  match result with
  | Ok x -> callback x
  | Error e -> Error e



let main () =
  let result = Parser.Term.parse_channel stdin >>= fun ast ->
  Ok (Compiler.compile ast) >>= fun js_ast ->
  Ok (JavaScript.to_string js_ast)

  in
  match result with
  | Ok r -> print_endline r
  | Error r -> print_endline r

let () = main ()

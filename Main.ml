module Result = struct
  module Infix = struct
    let (>>=) = function
      | Ok ok   -> fun f -> f ok
      | Error e -> fun _ -> Error e

    let (>>|) result f =
      result >>= fun x -> Ok (f x)
  end

  let print = function
    | Ok    string -> Printf.printf  "%s\n" string
    | Error string -> Printf.eprintf "%s\n" string
end

open Result.Infix

let () =
  Parser.Term.parse_channel stdin
  >>| Compiler.compile
  >>| JavaScript.to_string
  |> Result.print

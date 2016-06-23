module String = Core_kernel.Std.String

module Result = struct
  module Infix = struct
    let (>>=) = function
      | Ok ok   -> fun f -> f ok
      | Error e -> fun _ -> Error e

    let (>>|) result f =
      result >>= fun x -> Ok (f x)
  end

  let map_error f = function
    | Ok some -> Ok some
    | Error e -> Error (f e)

  let print = function
    | Ok    string -> Printf.printf  "%s\n" string
    | Error string -> Printf.eprintf "%s\n" string

  let exit = function
    | Ok _ -> exit 0
    | Error _ -> exit 1
end

open Result.Infix

let () =
  let file_name = Sys.argv.(1) in
  let module_name = String.chop_suffix_exn file_name ~suffix:".virum" in
  let result =
    Parser.Items.parse_channel (open_in file_name)
      >>| (fun items -> Syntax.Module (module_name, items))
      >>| ToPython.compile
      >>| Python.to_string
  in
  result
    |> Result.map_error Error.to_string
    |> Result.print;
  result
    |> Result.exit;

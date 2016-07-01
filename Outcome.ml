module List = Core_kernel.Std.List

let to_either = function
  | Ok ok -> `Fst ok
  | Error error -> `Snd error

let all items =
  let oks, errors = List.partition_map items ~f:to_either in
  let errors = List.concat errors in
  if errors <> [] then Error errors else Ok oks

let bind = function
  | Ok ok   -> fun f -> f ok
  | Error e -> fun _ -> Error e

let map ~f result =
  bind result (fun x -> Ok (f x))

let both left right = match left, right with
  | Ok left, Ok right -> Ok (left, right)
  | Error left, Error right -> Error (left @ right)
  | Error error, _ | _, Error error -> Error error

let of_option ~error = function
  | None -> Error [error]
  | Some x -> Ok x

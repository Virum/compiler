open Core_kernel.Std

type ('a, 'b) t = 'a * 'b

let map (left, right) ~f = (f left, f right)

let uncurry = Tuple2.uncurry

module List = struct
  let map items ~f = List.map items ~f:(map ~f)
end

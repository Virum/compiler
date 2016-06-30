val (=>): 'a -> 'a -> unit
val test: string -> (unit -> unit) -> unit

module Assertion: sig
  val create: ('a -> Core_kernel.Std.Sexp.t) -> ('a -> 'a -> unit)
end

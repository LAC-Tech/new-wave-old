type t
val create        : unit -> t
val compile_to_ir : t -> bytes -> IR.t list

type vm_error = NestedDefinition | StackUnderflow | UnexpectedInstruction

type exn_data = {
  error: vm_error;
  instruction: IR.t;
  position: int;
} 

val string_of_exn_data : exn_data -> bytes

exception Exn of exn_data

type t
val create : unit -> t
val lookup  : t -> bytes -> IR.t option
val eval    : t -> IR.t list -> bytes
val debug   : t -> bytes

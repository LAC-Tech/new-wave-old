open Core.Std

(* TODO: these should be runtime errors. Unexpected Instructions and Nested
 * defintions should be caught by the type checker.
 *)
type vm_error = NestedDefinition | StackUnderflow | UnexpectedInstruction

let string_of_vm_error = function
  | NestedDefinition      -> "nested definition"
  | StackUnderflow        -> "stack underflow"
  | UnexpectedInstruction -> "unexpected instruction"

exception Internal_exn of vm_error

type exn_data = {
  error: vm_error;
  instruction: IR.t;
  position: int;
}

let string_of_exn_data = function
  | {instruction; position; error} ->
      Format.sprintf "%s thrown by %s at position %s"
        (string_of_vm_error error)
        (IR.to_string instruction)
        (string_of_int position)

exception Exn of exn_data

type state = Executing | Defining
type data = Int32.t

type t = {
  mutable state: state;
  mutable mar: int; (* "memory address register" *)
  new_def: IR.t Dequeue.t;
  memory: (IR.t array) Dequeue.t;
  stack: data Dequeue.t;
}

let create () = {
  state   = Executing; (* can this start in defining??? *)
  mar     = -1;
  new_def = Dequeue.create ();
  memory  = Dequeue.create ~initial_length:128 ();
  stack   = Dequeue.create ~initial_length:32 ()
}

(*
 * Memory
 *)

let def_end vm = 
  let def = (Dequeue.to_array vm.new_def) in
  Dequeue.enqueue_back vm.memory def;
  Dequeue.clear vm.new_def

let compile vm = Dequeue.enqueue_back vm.new_def

(*
 * Stack
 *)

let pop vm = match Dequeue.dequeue_back vm.stack with
  | Some data -> data
  | None      -> raise (Internal_exn StackUnderflow)

let drop vm = ignore(pop vm)

let top vm = match Dequeue.peek_back vm.stack with
  | Some data -> data
  | None      -> raise (Internal_exn StackUnderflow)

let push vm = Dequeue.enqueue_back vm.stack

let dup vm = let top = top vm in push vm top

let replace vm =
  let len = Dequeue.length vm.stack in
  try Dequeue.set_exn vm.stack (len - 1)
  with Failure _ -> raise (Internal_exn StackUnderflow)

let swap vm =
  let a = pop vm in
  let b = pop vm in
  push vm a;
  push vm b

let rot vm =
  let z = pop vm in
  let y = pop vm in
  let x = pop vm in
  push vm y;
  push vm z;
  push vm x

(*
 * Arithmetic operations
 *)

let neg vm = replace vm (top vm |> Int32.neg)

let binary_op vm op =
  let rhs = (pop vm) in
  let lhs = (top vm) in 
  replace vm (op lhs rhs)

let add vm = binary_op vm Int32.( + )
let sub vm = binary_op vm Int32.( - )
let mul vm = binary_op vm Int32.( * )
let div vm = binary_op vm Int32.( / )

(*
 * Conditional operators
 *)

(* cond t f -> t | f *)
let if_ vm = 
  rot vm;
  if (pop vm) = 0l then swap vm; 
  drop vm

let bool_binary_op vm op = binary_op vm (fun l r -> op l r |> Data.bool_to_i32)

let gt vm = bool_binary_op vm ( > )
let lt vm = bool_binary_op vm ( < )
let eq vm = bool_binary_op vm ( = )

(*
 * Interpret
 *)

let string_of_stack vm = 
  vm.stack
  |> Dequeue.to_list
  |> List.map ~f: Int32.to_string
  |> String.concat ~sep:" "

let rec eval_exec vm = function
  (* Memory *)
  | IR.DefBgn(addr) ->
      vm.mar    <- addr;
      vm.state  <- Defining
  | IR.DefEnd     -> raise (Internal_exn UnexpectedInstruction)
  | IR.Call(addr) -> Dequeue.get vm.memory addr |> Array.iter ~f:(eval_exec vm)

  (* Stack *)
  | IR.Push(data) -> push vm data
  | IR.Drop       -> drop vm
  | IR.Dup        -> dup  vm
  | IR.Swap       -> swap vm
  | IR.Rot        -> rot  vm

  (* Arithmetic *)
  | IR.Neg -> neg vm
  | IR.Add -> add vm
  | IR.Sub -> sub vm
  | IR.Mul -> mul vm
  | IR.Div -> div vm

  (* Conditional *)
  | IR.If -> if_ vm
  | IR.Gt -> gt  vm
  | IR.Lt -> lt  vm
  | IR.Eq -> eq  vm

let eval_def vm = function
  | IR.DefBgn(_) -> raise (Internal_exn NestedDefinition)
  | IR.DefEnd ->
      def_end vm;
      vm.state <- Executing
  | ir -> compile vm ir

let eval_ir vm pos ir = 
  try
    match vm.state with
    | Executing -> eval_exec vm ir;
    | Defining  -> eval_def vm ir ;
  with
    | Internal_exn(err) ->
        raise (Exn { position = pos; instruction = ir; error = err })

let eval vm is = 
  List.iteri is ~f:(eval_ir vm);
  vm |> string_of_stack
  
let string_of_state vm = match vm.state with
  | Executing -> "Executing" 
  | Defining -> "Defining"

let string_of_new_def vm = 
  vm.new_def
  |> Dequeue.to_list
  |> List.map ~f:IR.to_string
  |> String.concat ~sep:" "

let debug vm = 
  Format.sprintf "state = %s\nnew_def = %s\nmar = %s\nstack = %s"
  (string_of_state vm) 
  (string_of_new_def vm) 
  (string_of_int vm.mar) 
  (string_of_stack vm)

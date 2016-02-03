open Core.Std

(* 
 * From a low level perspective the "name" of a generic type is completely
 * arbitrary - it just needs to denote a position
 *)

type atom = I32 | DA | Any
type signature = (atom list * atom list) option
exception Mismatch

(* TODO: actually type check and throw the Mismatch exn *)
let compose (l_in, l_out) (r_in, r_out) =
  let diff = (List.length r_in) - (List.length l_out) in

  (* Printf.printf "number of new inputs %d\n" diff; *)

  if diff > 0 then
    let unconsumed_inputs = List.drop r_in (List.length l_out) in
    (l_in @ unconsumed_inputs, r_out)
  else if diff < 0 then
    let remaining_outputs = List.take l_out (-diff) in 
    (l_in, remaining_outputs @ r_out)
  else
    (l_in, r_out)

(*
type t = I32 | Generic of int

let natural_to_string = Int.Map.of_alist_exn
  [0, "a";
   1, "b";
   2, "c";
   3, "d";
   4, "e";
   5, "f";
   6, "g";
   7, "h"]

let string_of_t = function
  | I32 -> "i32"
  | Generic n -> "'" ^ (Map.find_exn natural_to_string n)

type signature = Runtime of runtime | Special
and runtime = {inputs: t list; outputs: t list}

let string_of_t_list ts =
  ts |> List.map ~f:string_of_t |> String.concat ~sep:" "

let string_of_signature = function
  | Special -> "( SPECIAL )"
  | Runtime {inputs = is; outputs = os} -> 
      ["("; string_of_t_list is; "->"; string_of_t_list os; ")"]
      |> String.concat ~sep:" "

let void = {inputs = []; outputs = []}

let push = {
  inputs  = [];
  outputs = [Generic 0]
}

let drop = {
  inputs = [Generic 0]; 
  outputs = []
}

let dup = { 
  inputs  = [Generic 0]; 
  outputs = [Generic 0; Generic 0]
}

let swap = {
  inputs  = [Generic 0; Generic 1]; 
  outputs = [Generic 1; Generic 0]
}

let unary_arithmetic = {
  inputs  = [I32];
  outputs = [I32]
}

let binary_arithmetic = {
  inputs  = [I32; I32]; 
  outputs = [I32]
}

let type_of_word = function
  (* memory *)
  | Primitive (StartDef id) -> Immediate
  | Primitive EndDef        -> Immediate

  (* stack *)
  | Primitive (Push data) -> Runtime push  
  | Primitive Drop        -> Runtime drop
  | Primitive Dup         -> Runtime dup
  | Primitive Swap        -> Runtime swap
 
  (* arithmetic *)
  | Primitive Neg -> Runtime unary_arithmetic
  | Primitive Add -> Runtime binary_arithmetic
  | Primitive Sub -> Runtime binary_arithmetic
  | Primitive Mul -> Runtime binary_arithmetic
  | Primitive Div -> Runtime binary_arithmetic

  (* TODO: shutup compiler, fold_right with the binary compose *)
  | Defined _ -> Immediate 

let input_arity rt = List.length rt.inputs
let output_arity rt = List.length rt.outputs  

let match_types t1 t2 = match t1, t2 with
  (* Generics match whatever type *)
  | Generic _, concrete   -> Some concrete
  | concrete, Generic _   -> Some concrete
  | concrete, conrete     -> Some concrete
  | concrete, _           -> None
   
let runtime_compose rt1 rt2 =
  let diff = (input_arity rt2) - (output_arity rt1) in

  (* Printf.printf "number of new inputs %d\n" diff; *)

  if diff > 0 then
    let unconsumed_inputs = List.drop rt2.inputs (output_arity rt1) in
    { 
      inputs  = rt1.inputs @ unconsumed_inputs; 
      outputs = rt2.outputs
    }
  else if diff < 0 then
    let remaining_outputs = List.take rt1.outputs (-diff) in 
    {
      inputs  = rt1.inputs; 
      outputs = remaining_outputs @ rt2.outputs
    }
  else
    {inputs = rt1.inputs; outputs = rt2.outputs}
*)

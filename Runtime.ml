(*
 * This corresponds to the outer interpreter in forth. It also doubles as the
 * parser, in the traditional sense. 
 *)

open Core.Std

(*
type t {
  symbol_table: (string, int) Hashtbl.t
}

let create () = {
  symbol_table = Hashtbl.create ();
}
*)

let whitespace = Str.regexp "[ \t\r\n]+\\|(\\|)"
let tokenize = Str.split_delim whitespace

let read_i32 s = Option.try_with (fun () -> (IR.Push(Int32.of_string s)))

let read_bool = function
  | "true"  -> Some(IR.Push(1l))
  | "false" -> Some(IR.Push(0l))
  | _       -> None
  
let parse_token lookup token =
  [lookup; read_i32; read_bool]
  |> List.find_map ~f:(fun f -> f token)
  |> Option.value ~default: (IR.DefBgn(token))

let parse lookup str = 
  str 
  |> tokenize 
  |> List.map ~f:(parse_token lookup)



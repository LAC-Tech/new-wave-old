(*
 * This corresponds to the outer interpreter in forth. It also doubles as the
 * parser, in the traditional sense. 
 *)

open Core.Std

type t = {
  mutable free_addr: int;
  symbol_table: (string, int) Hashtbl.t;
}

let create () = {
  free_addr = -1;
  symbol_table = String.Table.create ();
}

let whitespace = Str.regexp "[ \t\r\n]+\\|(\\|)"
let tokenize = Str.split_delim whitespace

let addr_of_symbol rt sym = 
  Hashtbl.find rt.symbol_table sym |> Option.map ~f:(fun addr -> IR.Call(addr))

let read_i32 s = Option.try_with (fun () -> (IR.Push(Int32.of_string s)))

let read_bool = function
  | "true"  -> Some(IR.Push(1l))
  | "false" -> Some(IR.Push(0l))
  | _       -> None

let primitives = String.Map.of_alist_exn
  [";",     IR.DefEnd;
   "drop",  IR.Drop;
   "dup",   IR.Dup;
   "swap",  IR.Swap;
   "rot",   IR.Rot;
   "~",     IR.Neg;
   "+",     IR.Add;
   "-",     IR.Sub;
   "*",     IR.Mul;
   "/",     IR.Div;
   "if",    IR.If;
   ">",     IR.Gt;
   "<",     IR.Lt;
   "=",     IR.Eq]

let lookup rt token =
  [Map.find primitives; read_i32; read_bool; addr_of_symbol rt]
  |> List.find_map ~f:(fun f -> f token)

let bind_sym rt sym =
  rt.free_addr <- rt.free_addr + 1;
  Hashtbl.add_exn rt.symbol_table ~key:sym ~data:rt.free_addr;
  IR.DefBgn(rt.free_addr)

let parse_token rt token = match lookup rt token with
  | Some ir -> ir
  | None -> bind_sym rt token
 
let compile_to_ir rt str = tokenize str |> List.map ~f:(parse_token rt)

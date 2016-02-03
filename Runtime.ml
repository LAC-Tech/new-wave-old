(*
 * This corresponds to the outer interpreter in forth. It also doubles as the
 * parser, in the traditional sense. 
 *)

open Core.Std

type metadata = { addr: int; signature: Type.signature option }

type t = {
  mutable free_addr: int;
  symbol_table: (string, metadata) Hashtbl.t
}

let create () = {
  free_addr     = -1;
  symbol_table  = String.Table.create ();
}

let whitespace = Str.regexp "[ \t\r\n]+\\|(\\|)"

let tokenize = Str.split_delim whitespace

let metadata_of_symbol rt = Hashtbl.find rt.symbol_table

let addr_of_symbol rt sym = 
  metadata_of_symbol rt sym |> Option.map ~f:(fun md -> IR.Call(md.addr))

let read_i32 s = Option.try_with (fun () -> (IR.Push(Int32.of_string s)))

let read_bool = function
  | "true"  -> Some(IR.Push(1l))
  | "false" -> Some(IR.Push(0l))
  | _       -> None

let primitives = String.Map.of_alist_exn
  [";",             IR.DefEnd;
   "drop",          IR.Drop;
   "dup",           IR.Dup;
   "swap",          IR.Swap;
   "rot",           IR.Rot;
   "~",             IR.Neg;
   "+",             IR.Add;
   "-",             IR.Sub;
   "*",             IR.Mul;
   "/",             IR.Div;
   "if",            IR.If;
   ">",             IR.Gt;
   "<",             IR.Lt;
   "=",             IR.Eq;
   "dynarray-new",  IR.DaNew;
   "length",        IR.DaLen;
   "set",           IR.DaSet;
   "ref",           IR.DaRef;
   "push",          IR.DaPush;
   "pop",           IR.DaPop]

let lookup rt token : (IR.t * Type.signature) option =
  [Map.find primitives; read_i32; read_bool; addr_of_symbol rt]
  |> List.find_map ~f:(fun f -> f token)
  |> Option.map ~f:(fun ir -> (ir, IR.to_type ir))

let bind_sym rt (sym, sig_) =
  rt.free_addr <- rt.free_addr + 1;
  Hashtbl.add_exn 
    rt.symbol_table 
    ~key:sym 
    ~data:{ addr = rt.free_addr; signature = sig_ }

let parse_token rt token = match lookup rt token with
  | Some (ir, _) -> ir
  | None -> 
      bind_sym rt (token, None); 
      IR.DefBgn rt.free_addr

let compile_to_ir rt str = tokenize str |> List.map ~f:(parse_token rt)

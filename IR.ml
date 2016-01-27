open Core.Std
open Type

(* I've had many conflicting ideas on how to define the intermediate
 * representation, so I will outline my rationale, possible future plans, and
 * trade offs here.
 *
 * This is intended to be the highest level possible IR I can get. I don't even
 * want to bother with bytecode until I have the interpreter better defined
 * than it currently is.
 *
 * So, at the momement this is not meant to be written to a file, or used as a
 * portable format for New Wave code. It's not even meant to be a small kernel
 * of NewWave I can define to. It's designed to be the simplest, easiest, and
 * clearest way to represent the language, in memory, for easy interpretation.
 * So I am completely abusing union types.
 *
 * When more things are going on, and New Wave is more mature, I can revise
 * this. There are several possibilities:
 *
 * - IR -> Bytecode (easiest)
 * - IR -> LLVM
 * - IR -> JVM
 * - IR -> C
 * - IR -> Assembly
 *)

type t =
  (* Memory *)
  | DefBgn of int
  | DefEnd
  | Call of int
  
  (* Stack *)
  | Push of Int32.t 
  | Drop 
  | Dup 
  | Swap 
  | Rot
  
  (* Arithmetic *) 
  | Neg 
  | Add 
  | Sub 
  | Mul 
  | Div

  (* Conditional *)
  | If
  | Gt
  | Lt
  | Eq

  (* Dynamic Array *)
  (* NOTE: I am not at all convinced these are fundamental enough to be part
   * of the IR. But ATM I need them more than I need "primitive" arrays, so
   * they will stay in for now. They can later be replaced once I have
   * structs/modules/objects or whatever.
   *)
  | DaNew
  | DaLen
  | DaSet
  | DaRef
  | DaPush
  | DaPop

let to_string = function
  (* Memory *)
  | DefBgn(addr)  -> Printf.sprintf "def_bgn(%d)" addr
  | DefEnd        -> ";"
  | Call(addr)    -> Printf.sprintf "call(%d)" addr

  (* Stack *)
  | Push(i32) -> Int32.to_string i32
  | Drop      -> "drop"
  | Dup       -> "dup"
  | Swap      -> "swap"
  | Rot       -> "rot"

  (* Arithmetic *)
  | Neg -> "~"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

  (* Conditional *)
  | If -> "if"
  | Gt -> ">"
  | Lt -> "<"
  | Eq -> "="

  (* Dynamic Arrays *)
  | DaNew   -> "da_new"
  | DaLen   -> "da_len"
  | DaSet   -> "da_set"
  | DaRef   -> "da_ref"
  | DaPush  -> "da_push"
  | DaPop   -> "da_pop"

let to_type = function
  (* Memory *)
  | DefBgn _  -> Special
  | DefEnd    -> Special
  | Call _    -> Special (* TODO: this is not right at all *)
  
  (* Stack *)
  | Push _  -> Sig ([],               [Any])
  | Drop    -> Sig ([Any],            [])
  | Dup     -> Sig ([Any],            [Any; Any])
  | Swap    -> Sig ([Any; Any],       [Any; Any])
  | Rot     -> Sig ([Any; Any; Any],  [Any; Any; Any])
  
  (* Arithmetic *)
  | Neg -> Sig ([I32],      [I32])
  | Add -> Sig ([I32; I32], [I32])
  | Sub -> Sig ([I32; I32], [I32])
  | Mul -> Sig ([I32; I32], [I32])
  | Div -> Sig ([I32; I32], [I32])

  (* Conditional *)
  | If  -> Sig ([I32; Any; Any],  [Any])
  | Gt  -> Sig ([I32; I32],       [I32])
  | Lt  -> Sig ([I32; I32],       [I32])
  | Eq  -> Sig ([I32; I32],       [I32])

  (* Dynamic Array *)
  | DaNew   -> Sig ([I32],          [DA])
  | DaLen   -> Sig ([DA],           [I32])
  | DaSet   -> Sig ([DA; I32; Any], [])
  | DaRef   -> Sig ([DA; I32],      [Any])
  | DaPush  -> Sig ([DA; Any],      [])
  | DaPop   -> Sig ([DA],           [Any])

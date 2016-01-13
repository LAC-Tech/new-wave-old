open Core.Std

type t =
  (* Memory *)
  | DefBgn of string
  | DefEnd
  | Call of t array
  
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

let rec to_string = function
  (* Memory *)
  | DefBgn(id)  -> id
  | DefEnd      -> ";"
  | Call(is)    -> 
      "[ " 
      ^ (is |> Array.map ~f:to_string |> String.concat_array ~sep:" ")
      ^ " ] apply"

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

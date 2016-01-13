open Core.Std

exception Type_error  

let i32_to_bool = function
  | 1l -> true
  | 0l -> false
  | _  -> raise Type_error

let bool_to_i32 b = if b then 1l else 0l

(*
type t = 
  | I32 of Int32.t 
  | Bool of bool 
  | Seq of t Dequeue.t


let get_i32  = function 
  | I32 i -> i 
  | _ -> raise Type_error

let get_bool = function 
  | Bool b -> b 
  | _ -> raise Type_error

let rec to_string = function
  | I32 i -> Int32.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Seq xs ->       
      let elements = 
        xs 
        |> Dequeue.to_list
        |> List.map ~f:to_string 
        |> String.concat ~sep:" "
      in
      "{ " ^ elements ^ "}" 
*)

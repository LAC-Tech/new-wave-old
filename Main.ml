(*
 * The goal of this is not to be performant, or robust, or be written in a 
 * language with a great eco system. The goal is to quickly explore new ideas
 * and get a prototype running quickly.
 *)

let vm = VM.create ()
let rt = Runtime.create ()

let print_exn data =
  Format.printf "EXCEPTION: %s\n%s\n"
    (VM.string_of_exn_data data)
    (VM.debug vm)

let rec repl () =
  print_string "> ";
  begin
  try
    read_line ()
    |> Runtime.compile_to_ir rt
    |> VM.eval vm
    |> print_endline
  with
    | VM.Exn(data)    -> print_exn data
    | Data.Type_error -> print_endline "type error"
  end ;
  repl ()

let main = repl ()

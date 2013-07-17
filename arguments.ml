open Arg

(* let result = Queue.create () *)

let add_cmt result fn =
  Queue.add ( Tt_restore.load_and_restore fn) result
let add_cmti = Tt_restore.add_interface

let fileproc result s =
  let open Filename in
  if check_suffix s ".cmt"
  then add_cmt result s
  else if check_suffix s ".cmti"
  then add_cmti s
  else raise ( Bad (Printf.sprintf "%s is not a bin-annot file" s))

let print_flag = ref false
let print () = !print_flag
let print_usage = "Print the created lambda code"

let comp_flag = ref false
let comp () = !comp_flag
let comp_usage = "Compiles the created lambda code to file lambda_out.cmx"

let args =
  [
    "-p", Set print_flag, print_usage; "-print", Set print_flag, print_usage;
    "-c", Set comp_flag, comp_usage; "-comp", Set comp_flag, comp_usage
  ]

let usage_msg =
  "Please specify .cmt and .cmti files in the order in which they will be used."

let iterate f =
  let result = Queue.create () in
  parse args (fileproc result) usage_msg;
  Array.init
    (Queue.length result)
    (fun _ -> f (Queue.take result))

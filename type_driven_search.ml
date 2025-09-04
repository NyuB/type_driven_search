let usage_and_exit code =
  print_endline (Printf.sprintf "Usage: %s <command>" Sys.argv.(0));
  print_endline "Where command is one of { help, explain }";
  print_endline "help: print this help message";
  print_endline "explain <signature>: explains the C function <signature>";
  exit code
;;

let explain () =
  let sub_args = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
  if Array.length sub_args < 1
  then usage_and_exit 1
  else (
    match Signature.parse sub_args.(0) with
    | Some signature -> print_endline @@ Signature.explain signature
    | None -> print_endline "Invalid C/C++ signature")
;;

let () =
  if Array.length Sys.argv < 2
  then usage_and_exit 1
  else (
    match Sys.argv.(1) with
    | "help" -> usage_and_exit 0
    | "explain" -> explain ()
    | _ -> usage_and_exit 2)
;;

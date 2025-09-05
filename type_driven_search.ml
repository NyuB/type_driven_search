let usage_and_exit code =
  print_endline (Printf.sprintf "Usage: %s <command>" Sys.argv.(0));
  print_endline "Where command is one of { help; explain; index }";
  print_endline "help: print this help message";
  print_endline "explain <signature>: explains the C function <signature>";
  print_endline "index { create; get; store}: store and retrieve functions by signature";
  exit code
;;

let explain args =
  if Array.length args < 1
  then usage_and_exit 1
  else (
    match Signature.parse args.(0) with
    | Some signature -> print_endline @@ Signature.explain signature
    | None -> print_endline "Invalid C/C++ signature")
;;

let index args =
  let command = args.(0) in
  match command with
  | "create" ->
    let _ = Index.FileBased.init Index.{ file = args.(1); mode = Truncate } in
    ()
  | "store" ->
    let index = Index.FileBased.init Index.{ file = args.(1); mode = Keep } in
    Index.FileBased.store
      index
      [ Index.CFunction.
          { name = args.(2); signature = Signature.parse args.(3) |> Option.get }
      ]
  | "get" ->
    let index = Index.FileBased.init Index.{ file = args.(1); mode = Keep } in
    let fs = Index.FileBased.get index (Signature.parse args.(2) |> Option.get) in
    List.iter (fun f -> print_endline @@ Index.CFunction.string_of_t f) fs
  | _ -> failwith "Invalid command"
;;

let () =
  if Array.length Sys.argv < 2
  then usage_and_exit 1
  else (
    match Sys.argv.(1) with
    | "help" -> usage_and_exit 0
    | "explain" -> explain @@ Array.sub Sys.argv 2 (Array.length Sys.argv - 2)
    | "index" -> index @@ Array.sub Sys.argv 2 (Array.length Sys.argv - 2)
    | _ -> usage_and_exit 2)
;;

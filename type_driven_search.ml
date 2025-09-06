let usage_and_exit code =
  print_endline (Printf.sprintf "Usage: %s <command>" Sys.argv.(0));
  print_endline "Where command is one of { help; explain; index }";
  print_endline "help: print this help message";
  print_endline "explain <signature>: explains the C function <signature>";
  print_endline
    "index { create; get; store; serve }: store and retrieve functions by signature";
  print_endline "|-- create <index>: initialize an empty index into the <index> file";
  print_endline
    "|-- store <index> <name> <signature>: stores the function <name> with the given \
     <signature> into <index>";
  print_endline
    "|-- get <index> <query>: list all functions stored within <index> matching <query>";
  print_endline
    "|-- serve <index>: enter an interactive mode waiting for queries on the standard \
     input";
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

let index_get index signature =
  match Signature.parse signature with
  | None -> print_endline "Invalid signature"
  | Some signature ->
    let fs = Index.FileBasedSorted.get index signature in
    List.iter (fun f -> print_endline @@ Index.CFunction.string_of_t f) fs
;;

let index args =
  let command = args.(0) in
  match command with
  | "create" ->
    let _ = Index.FileBasedSorted.init Index.{ file = args.(1); mode = Truncate } in
    ()
  | "store" ->
    let index = Index.FileBasedSorted.init Index.{ file = args.(1); mode = Keep } in
    Index.FileBasedSorted.store
      index
      [ Index.CFunction.
          { name = args.(2); signature = Signature.parse args.(3) |> Option.get }
      ]
  | "get" ->
    let index = Index.FileBasedSorted.init Index.{ file = args.(1); mode = Keep } in
    index_get index args.(2)
  | "serve" ->
    let index = Index.FileBasedSorted.init Index.{ file = args.(1); mode = Keep } in
    while true do
      Out_channel.output_string stdout "? ";
      Out_channel.flush stdout;
      try
        let line = input_line stdin in
        index_get index line
      with
      | End_of_file ->
        print_endline "";
        exit 0
    done
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

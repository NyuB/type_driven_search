let usage_and_exit code =
  print_endline (Printf.sprintf "Usage: %s <command>" Sys.argv.(0));
  print_endline
    {|Where command is one of { help; explain; index }
help: print this help message
explain <signature>: explains the C function <signature>
index [opts] { create; get; store; serve }: store and retrieve functions by signature
|-- create <index>: initialize an empty index into the <index> file
|-- store <index> <name> <signature>: stores the function <name> with the given <signature> into <index>
|-- get <index> <query>: list all functions stored within <index> matching <query>
|-- serve <index>: enter an interactive mode waiting for queries on the standard input
| --index=<index-id>: choose the indexing method where <index-id> is one of { FileBased (default); FileBasedSorted }|};
  exit code
;;

let explain args =
  if Array.length args < 1
  then usage_and_exit 1
  else (
    match Signature.parse args.(0) with
    | Some signature -> print_endline @@ Signature.explain signature
    | None -> print_endline "not a valid C/C++ signature")
;;

module IndexCommand = struct
  type options = { index_id : string }

  let defaults = { index_id = "FileBased" }

  module StringMap = Map.Make (String)

  let index_map : (module Index.S with type config = Index.config_open_file) StringMap.t =
    [ (module Index.FileBased); (module Index.FileBasedSorted) ]
    |> List.map (fun (module I : Index.S with type config = Index.config_open_file) ->
      I.id, (module I : Index.S with type config = Index.config_open_file))
    |> StringMap.of_list
  ;;

  let parse_arg (config, args) arg =
    if String.starts_with ~prefix:"--index=" arg
    then (
      let index_id =
        String.sub
          arg
          (String.length "--index=")
          (String.length arg - String.length "--index=")
      in
      { index_id }, args)
    else config, arg :: args
  ;;

  let parse_index_args args =
    Array.fold_left parse_arg (defaults, []) args
    |> fun (c, args) -> c, List.rev args |> Array.of_list
  ;;

  let get (type i) (module I : Index.S with type t = i) (index : i) signature =
    match Signature.parse signature with
    | None -> print_endline "Invalid signature"
    | Some signature ->
      let fs = I.get index signature in
      List.iter (fun f -> print_endline @@ Index.CFunction.string_of_t f) fs
  ;;

  let or_exit message code = function
    | Some v -> v
    | None ->
      prerr_endline message;
      exit code
  ;;

  let run args =
    let config, args = parse_index_args args in
    let (module I) =
      StringMap.find_opt config.index_id index_map
      |> or_exit (Printf.sprintf "Invalid index type: '%s'" config.index_id) 2
    in
    match args.(0) with
    | "create" ->
      let _ = I.init Index.{ file = args.(1); mode = Truncate } in
      ()
    | "store" ->
      let index = I.init Index.{ file = args.(1); mode = Keep } in
      I.store
        index
        [ Index.CFunction.
            { name = args.(2); signature = Signature.parse args.(3) |> Option.get }
        ]
    | "get" ->
      let index = I.init Index.{ file = args.(1); mode = Keep } in
      get (module I) index args.(2)
    | "serve" ->
      let index = I.init Index.{ file = args.(1); mode = Keep } in
      while true do
        Out_channel.output_string stdout "? ";
        Out_channel.flush stdout;
        try
          let line = input_line stdin in
          get (module I) index line
        with
        | End_of_file ->
          print_endline "";
          exit 0
      done
    | _ -> failwith "Invalid command"
  ;;
end

let () =
  if Array.length Sys.argv < 2
  then usage_and_exit 1
  else (
    match Sys.argv.(1) with
    | "help" -> usage_and_exit 0
    | "explain" -> explain @@ Array.sub Sys.argv 2 (Array.length Sys.argv - 2)
    | "index" -> IndexCommand.run @@ Array.sub Sys.argv 2 (Array.length Sys.argv - 2)
    | _ -> usage_and_exit 2)
;;

let _timed_execution f =
  let start_s = Unix.gettimeofday () in
  let result = f () in
  let current_s = Unix.gettimeofday () in
  let delta = current_s -. start_s in
  result, delta *. 1000.
;;

let pseudo_random_name rand prefix =
  let length = 3 + Random.State.int rand 14 in
  prefix
  ^ String.init length (fun _ -> Random.State.int rand 26 + Char.code 'a' |> Char.chr)
;;

let pseudo_random_type rand among =
  let n = Random.State.int rand among in
  Printf.sprintf "t_%d" n |> Signature.Ctype.atom
;;

let pseudo_random_signature rand : Signature.t =
  let params_count = Random.State.int rand 10
  and return = pseudo_random_type rand 10 in
  let params = List.init params_count (fun _ -> pseudo_random_type rand 10) in
  { return; params }
;;

let pseudo_random_function rand : Index.CFunction.t =
  let name = pseudo_random_name rand "f_"
  and signature = pseudo_random_signature rand in
  { name; signature }
;;

let pseudo_random_functions rand n : Index.CFunction.t list =
  List.init n (fun _ -> pseudo_random_function rand)
;;

let pick_n rand n l =
  let random_access = Array.of_list l in
  List.init n (fun _ ->
    random_access.(Random.State.int rand @@ Array.length random_access))
;;

let bench (module I : Index.S with type config = Index.config_open_file) =
  print_endline (Printf.sprintf "[ %s ]" I.id);
  let rand = Random.State.make [| 1; 2; 3; 5; 8 |] in
  let functions = pseudo_random_functions rand 10_000 in
  let index = I.init { file = "bench.idx"; mode = Create } in
  let _, store_time = _timed_execution @@ fun () -> I.store index functions in
  print_endline @@ Printf.sprintf "| Store: %f(ms)" store_time;
  let _, get_time =
    _timed_execution
    @@ fun () ->
    pick_n rand 1000 functions
    |> List.map Index.CFunction.signature
    |> List.map (I.get index)
  in
  print_endline @@ Printf.sprintf "| Get: %f(ms)" get_time
;;

let () =
  bench (module Index.FileBased);
  bench (module Index.FileBasedSorted)
;;

let _timed_execution f =
  let start_s = Unix.gettimeofday () in
  let result = f () in
  let current_s = Unix.gettimeofday () in
  let delta = current_s -. start_s in
  result, delta *. 1000.
;;

let store_count = 10_000
let get_count = 1000
let query_count = 1000

let bench (module I : Index.S with type config = Index.config_open_file) =
  let rand = Testability.reproducible_random () in
  let functions = Testability.pseudo_random_functions rand store_count 10 10 in
  let index = I.init { file = "bench.idx"; mode = Create } in
  let _, store_time = _timed_execution @@ fun () -> I.store index functions in
  let _, get_time =
    _timed_execution
    @@ fun () ->
    Testability.pick_n rand get_count functions
    |> List.map Signature.CFunction.signature
    |> List.map (I.get index)
  in
  let results, query_time =
    _timed_execution
    @@ fun () ->
    List.init query_count (fun _ -> Testability.pseudo_random_signature rand 10 10)
    |> List.map Index.Query.condense_signature
    |> List.map (I.query index)
  in
  let average_percentage_matching_query =
    results
    |> List.map (Fun.compose float_of_int List.length)
    (* Matched / In store*)
    |> List.map (fun matched -> 100.0 *. matched /. float_of_int store_count)
    (* Sum *)
    |> List.fold_left ( +. ) 0.0
    (* Mean over total of query *)
    |> fun total -> total /. float_of_int (List.length results)
  in
  [| I.id
   ; string_of_float store_time
   ; string_of_float get_time
   ; string_of_float query_time
   ; string_of_float average_percentage_matching_query
  |]
;;

module type Adapter = sig
  type a
  type b

  val map : a -> b
end

module AdaptIndex (A : Adapter) (I : Index.S with type config = A.b) :
  Index.S with type config = A.a and type t = I.t = struct
  include I

  type config = A.a

  let init = Fun.compose init A.map
end

module InMemory =
  AdaptIndex
    (struct
      type a = Index.config_open_file
      type b = unit

      let map _ = ()
    end)
    (Index.InMemory)

let pad_left width str =
  let padding_char = if String.starts_with ~prefix:"---" str then '-' else ' ' in
  let padding = width - String.length str in
  Printf.sprintf "%s%s" (String.make padding padding_char) str
;;

let () =
  let headers = [| "Index Type \\ Op(ms)"; "Store"; "Get"; "Query"; "Match %" |] in
  let headers_suffix = Array.make (Array.length headers) "---" in
  let results = Dynarray.of_array [| headers; headers_suffix |] in
  Dynarray.add_last results @@ bench (module InMemory);
  (* Dynarray.add_last results @@ bench (module Index.FileBased); *)
  Dynarray.add_last results @@ bench (module Index.FileBasedSorted);
  let cell_widths =
    Array.mapi
      (fun i _ ->
         Dynarray.fold_left
           (fun width row -> max width (String.length (Array.get row i)))
           0
           results)
      (Dynarray.get results 0)
  in
  Dynarray.iter
    (fun row ->
       Array.iteri
         (fun i width ->
            print_string "|";
            print_string (pad_left width row.(i)))
         cell_widths;
       print_endline "|")
    results
;;

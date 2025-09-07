class introspector =
  object (self)
    val access_pattern : int Dynarray.t = Dynarray.make 0 0
    method notify index = Dynarray.add_last access_pattern index

    method trace (cmp : int -> int) =
      fun i ->
        self#notify i;
        cmp i

    method get_access_pattern = Dynarray.to_array access_pattern
  end

let print_access_pattern elements string_of_element pattern =
  let pair a b = a, b in
  let instants_to_indices = Array.mapi pair pattern in
  Array.sort (fun (_, ia) (_, ib) -> Int.compare ia ib) instants_to_indices;
  print_endline "Instant  |  Index accessed";
  instants_to_indices
  |> Array.fold_left
       (fun last_index (instant, index) ->
          if last_index != index then print_endline "            [...]";
          print_endline
            (Printf.sprintf
               "%3d  ---->  [%3s]"
               instant
               (string_of_element elements.(index)));
          index)
       0
  |> ignore
;;

let print_find_result string_of_element elements r =
  r
  |> Option.map (fun i -> string_of_element elements.(i))
  |> Option.value ~default:"Not found"
  |> print_endline
;;

let print_insertion_result string_of_element elements insertion_index =
  if Array.length elements = 0
  then (
    print_endline "[ ]";
    print_endline " ^";
    print_endline (Printf.sprintf "%3d" insertion_index))
  else if insertion_index = Array.length elements
  then (
    let left_str =
      Printf.sprintf "[ ... [%s]" (string_of_element elements.(insertion_index - 1))
    in
    let padding_left = String.make (String.length left_str) ' ' in
    print_endline (Printf.sprintf "%s ]" left_str);
    print_endline (Printf.sprintf "%s^" padding_left);
    print_endline (Printf.sprintf "%s%d" padding_left insertion_index))
  else if insertion_index = 0
  then (
    print_endline (Printf.sprintf "[ [%s] ... ]" (string_of_element elements.(0)));
    print_endline (Printf.sprintf " ^");
    print_endline (Printf.sprintf " %d" insertion_index))
  else (
    let left = elements.(insertion_index - 1)
    and right = elements.(insertion_index) in
    let left_str =
      Printf.sprintf
        "[ %s[%s]"
        (if insertion_index > 0 then "... " else "")
        (string_of_element left)
    in
    let right_str = Printf.sprintf "[%s] ... ]" @@ string_of_element right in
    let padding_left = String.make (String.length left_str) ' ' in
    print_endline (Printf.sprintf "%s %s" left_str right_str);
    print_endline (Printf.sprintf "%s^" padding_left);
    print_endline (Printf.sprintf "%s%d" padding_left insertion_index))
;;

let search_and_print needle elements compare string_of_element =
  let debug = new introspector in
  let compare i = compare elements.(i) needle in
  let result = Binary_search.index (Array.length elements) (debug#trace compare) in
  print_endline "Search result:";
  print_find_result string_of_element elements result;
  print_endline "Access pattern:";
  print_access_pattern elements string_of_element debug#get_access_pattern
;;

let%expect_test "Find a name among strings" =
  search_and_print
    "Carlos"
    [| "Alice"; "Bob"; "Carlos"; "Damien"; "Eleonore"; "Fanny"; "Gustave" |]
    String.compare
    Fun.id;
  [%expect
    {|
    Search result:
    Carlos
    Access pattern:
    Instant  |  Index accessed
                [...]
      1  ---->  [Bob]
                [...]
      2  ---->  [Carlos]
                [...]
      0  ---->  [Damien]
    |}]
;;

let%expect_test "Find 1 in [0..50]" =
  search_and_print 1 (Array.init 50 Fun.id) Int.compare string_of_int;
  [%expect
    {|
    Search result:
    1
    Access pattern:
    Instant  |  Index accessed
                [...]
      4  ---->  [  1]
                [...]
      3  ---->  [  3]
                [...]
      2  ---->  [  6]
                [...]
      1  ---->  [ 12]
                [...]
      0  ---->  [ 25]
    |}]
;;

let%expect_test "Find 12 in [0..17]" =
  search_and_print 12 (Array.init 17 Fun.id) Int.compare string_of_int;
  [%expect
    {|
    Search result:
    12
    Access pattern:
    Instant  |  Index accessed
                [...]
      0  ---->  [  8]
                [...]
      2  ---->  [ 11]
                [...]
      3  ---->  [ 12]
                [...]
      1  ---->  [ 13]
    |}]
;;

let%expect_test "Insert a name among strings" =
  let names = [| "Alice"; "Bob"; "Damien"; "Eleonore"; "Fanny"; "Gustave" |] in
  let compare to_insert i = String.compare names.(i) to_insert in
  let result_mid =
    Binary_search.insertion_index (Array.length names) (compare "Carlos")
  in
  print_insertion_result Fun.id names result_mid;
  [%expect
    {|
    [ ... [Bob] [Damien] ... ]
               ^
               2
    |}];
  let result_first =
    Binary_search.insertion_index (Array.length names) (compare "Aaron")
  in
  print_insertion_result Fun.id names result_first;
  [%expect
    {|
    [ [Alice] ... ]
     ^
     0
    |}];
  let result_last = Binary_search.insertion_index (Array.length names) (compare "Zoe") in
  print_insertion_result Fun.id names result_last;
  [%expect
    {|
    [ ... [Gustave] ]
                   ^
                   6
    |}]
;;

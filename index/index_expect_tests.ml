let print_condensed_query signature =
  let signature = Signature.parse signature |> Option.get in
  print_endline (Signature.string_of_t signature);
  let query = Index.Query.condense_signature signature in
  print_endline (Printf.sprintf "> %s" (Index.Query.string_of_t query))
;;

let%expect_test "Query building" =
  List.iter
    print_condensed_query
    [ "void()"; "int(int,char**)"; "int(int,int)"; "bool(plane,point,point)" ];
  [%expect
    {|
    void ()
    > ((returns void) )
    int (int,char**)
    > ((returns int) (1 int) (1 char**))
    int (int,int)
    > ((returns int) (2 int))
    bool (plane,point,point)
    > ((returns bool) (2 point) (1 plane))
    |}]
;;

let temp_index_file () = Filename.temp_file ~temp_dir:"." "temp" ".txt"

let%expect_test "debug" =
  let index = Index.FileBasedSorted.init { mode = Create; file = temp_index_file () } in
  let shared_signature = Signature.parse "int (int,int)" |> Option.get in
  let fadd = Signature.CFunction.{ name = "add"; signature = shared_signature }
  and fmul = Signature.CFunction.{ name = "mul"; signature = shared_signature }
  and fmain =
    Signature.CFunction.
      { name = "main"; signature = Signature.parse "int (int, char**)" |> Option.get }
  in
  Index.FileBasedSorted.store index [ fmain ];
  Index.FileBasedSorted.store index [ fadd ];
  Index.FileBasedSorted.store index [ fmul ];
  let back =
    Index.FileBasedSorted.get index shared_signature
    |> List.sort Signature.CFunction.compare
  in
  print_endline (back |> List.map Signature.CFunction.string_of_t |> String.concat ":");
  [%expect {| int add(int, int):int mul(int, int) |}]
;;

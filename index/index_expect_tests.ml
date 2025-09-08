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

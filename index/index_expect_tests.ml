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

let%expect_test "inspect internal index layout" =
  let index = Index.FileBasedSorted.init { mode = Create; file = temp_index_file () } in
  let shared_signature = Signature.parse "int (int,int)" |> Option.get in
  let fadd = Signature.CFunction.{ name = "add"; signature = shared_signature }
  and fmul = Signature.CFunction.{ name = "mul"; signature = shared_signature } in
  Index.FileBasedSorted.store index [ fadd ];
  Index.FileBasedSorted.store index [ fmul ];
  List.iter print_endline (Index.FileBasedSorted.repr_layout index);
  [%expect
    {|
         ||-----
    [0000]| 21 // int mul(int, int)
    [0001]| 0 // int add(int, int)
         ||-----
         ||[0000](0000)<0017>add:int (int,int)
         ||[0001](0021)<0017>mul:int (int,int)
         ||-----
    [0000]| 0 21 // p:01:int mul:int (int,int)
    [0001]| 0 0 // p:01:int add:int (int,int)
    [0002]| 12 21 // p:02:int mul:int (int,int)
    [0003]| 12 0 // p:02:int add:int (int,int)
    [0004]| 24 0 // r:int add:int (int,int)
    [0005]| 33 21 // r:int mul:int (int,int)
         ||-----
         ||[0000](0000)<0008>p:01:int
         ||[0001](0012)<0008>p:02:int
         ||[0002](0024)<0005>r:int
         ||[0003](0033)<0005>r:int
         ||-----
    |}];
  let f_void =
    Signature.CFunction.
      { name = "write"; signature = Signature.parse "void(int, char**)" |> Option.get }
  in
  Index.FileBasedSorted.store index [ f_void ];
  List.iter print_endline (Index.FileBasedSorted.repr_layout index);
  [%expect
    {|
         ||-----
    [0000]| 21 // int mul(int, int)
    [0001]| 0 // int add(int, int)
    [0002]| 42 // void write(int, char**)
         ||-----
         ||[0000](0000)<0017>add:int (int,int)
         ||[0001](0021)<0017>mul:int (int,int)
         ||[0002](0042)<0023>write:void (int,char**)
         ||-----
    [0000]| 42 42 // p:01:char** write:void (int,char**)
    [0001]| 0 42 // p:01:int write:void (int,char**)
    [0002]| 0 21 // p:01:int mul:int (int,int)
    [0003]| 0 0 // p:01:int add:int (int,int)
    [0004]| 12 21 // p:02:int mul:int (int,int)
    [0005]| 12 0 // p:02:int add:int (int,int)
    [0006]| 24 0 // r:int add:int (int,int)
    [0007]| 33 21 // r:int mul:int (int,int)
    [0008]| 57 42 // r:void write:void (int,char**)
         ||-----
         ||[0000](0000)<0008>p:01:int
         ||[0001](0012)<0008>p:02:int
         ||[0002](0024)<0005>r:int
         ||[0003](0033)<0005>r:int
         ||[0004](0042)<0011>p:01:char**
         ||[0005](0057)<0006>r:void
         ||-----
    |}]
;;

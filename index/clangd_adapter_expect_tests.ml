let%expect_test "Yaml conversion from clangd index" =
  Clangd_adapter.ingest "../test_resources/clangd_index.yml"
  |> List.map Signature.CFunction.string_of_t
  |> List.iter print_endline;
  [%expect
    {|
    int sqlite3_open_1(char const*, sqlite3**)
    int sqlite3_open_2(char const*, sqlite3**)
    |}]
;;

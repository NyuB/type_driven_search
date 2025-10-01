let%expect_test "Yaml conversion from clangd index" =
  let print_symbol s = Yaml.to_string_exn s |> print_endline in
  Clangd_adapter.ingest "../test_resources/clangd_index.yml"
  |> List.map Clangd_adapter.symbol_to_yaml
  |> List.iter print_symbol;
  [%expect
    {|
    ID: 04AE4B6BA5188AA3
    Name: sqlite3_open_1
    Scope: ""
    SymInfo:
      Kind: Function
      Lang: C
    CanonicalDeclaration:
      FileURI: sqlite3.h
      Start:
        Line: 3937
        Column: 15
      End:
        Line: 3937
        Column: 27
    Flags: 25
    Signature: (const char *filename, sqlite3 **ppDb)
    TemplateSpecializationArgs: ""
    CompletionSnippetSuffix: (${1:const char *filename}, ${2:sqlite3 **ppDb})
    Documentation: Awesome function
    ReturnType: int
    Type: c:I
    IncludeHeaders:
    - Header: sqlite3.h
      References: 1

    ID: "123567891234567"
    Name: sqlite3_open_2
    Scope: ""
    SymInfo:
      Kind: Function
      Lang: C
    CanonicalDeclaration:
      FileURI: sqlite3.h
      Start:
        Line: 3937
        Column: 15
      End:
        Line: 3937
        Column: 27
    Flags: 25
    Signature: (const char *filename, sqlite3 **ppDb)
    TemplateSpecializationArgs: ""
    CompletionSnippetSuffix: (${1:const char *filename}, ${2:sqlite3 **ppDb})
    Documentation: Duplicated to test multiple documents
    ReturnType: int
    Type: c:I
    IncludeHeaders:
    - Header: sqlite3.h
      References: 1
    |}]
;;

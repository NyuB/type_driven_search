let signature_testable : Signature.t Alcotest.testable =
  Alcotest.testable
    (fun fmt signature -> Format.pp_print_string fmt @@ Signature.string_of_t signature)
    Signature.equal
;;

let parsing_test
      ~(testable : 'a Alcotest.testable)
      parser_description
      parser
      (input : string)
      (expected : 'a)
  =
  let title = String.concat "" [ parser_description; ": accepts '"; input; "'" ]
  and exec =
    fun () ->
    Alcotest.check
      (Alcotest.option testable)
      "Parsing succeeds"
      (Some expected)
      (Parsers.parse_full parser input)
  in
  title, exec
;;

let parsing_fail_test
      ~(testable : 'a Alcotest.testable)
      parser_description
      parser
      (input : string)
  =
  let title = String.concat "" [ parser_description; ": rejects '"; input; "'" ]
  and exec =
    fun () ->
    Alcotest.check
      (Alcotest.option testable)
      "Parsing succeeds"
      None
      (Parsers.parse_full parser input)
  in
  title, exec
;;

let test_keyword_parsing =
  parsing_test ~testable:Alcotest.string "keyword" (Parsers.keyword "kw") "kw" "kw"
;;

let test_list_parsing =
  let open Parsers in
  let parser =
    list
      ~sep:(keyword ",")
      ~prefix:(keyword "(")
      ~suffix:(keyword ")")
      (first_of [ keyword "kw"; keyword "mot-clef"; keyword "keyword" ])
  in
  parsing_test
    ~testable:(Alcotest.list Alcotest.string)
    "keyword list"
    parser
    "(kw,mot-clef,keyword)"
    [ "kw"; "mot-clef"; "keyword" ]
;;

let test_keyword_fail_parsing =
  parsing_fail_test
    ~testable:Alcotest.string
    "keyword 'kw'"
    (Parsers.keyword "kw")
    "notkw"
;;

let signature_parsing_test input expected =
  let expected_repr = Signature.string_of_t expected in
  let title = Printf.sprintf "%s => %s" input expected_repr
  and exec =
    fun () ->
    Alcotest.check
      (Alcotest.option signature_testable)
      (Printf.sprintf "'%s' is correctly parsed as: %s" input expected_repr)
      (Some expected)
      (Signature.parse input)
  in
  title, exec
;;

let make_signature ret ps =
  Signature.
    { return = Signature.Ctype.parse ret; params = List.map Signature.Ctype.parse ps }
;;

let test_signature_void = signature_parsing_test "void ()" @@ make_signature "void" []
let test_signature_int = signature_parsing_test "int ()" @@ make_signature "int" []

let test_signature_one_param =
  signature_parsing_test "int (char)" @@ make_signature "int" [ "char" ]
;;

let test_signature_two_param =
  signature_parsing_test "int (char, int)" @@ make_signature "int" [ "char"; "int" ]
;;

let test_signature_pointer =
  signature_parsing_test "int (char**, int)" @@ make_signature "int" [ "char**"; "int" ]
;;

let test_condense_pointer =
  signature_parsing_test "int (char * *, int)" @@ make_signature "int" [ "char**"; "int" ]
;;

let test_signature_const =
  signature_parsing_test "void (char const*)" @@ make_signature "void" [ "char const*" ]
;;

let test_signature_unsigned =
  signature_parsing_test "void (unsigned int)" @@ make_signature "void" [ "unsigned int" ]
;;

let cfunction_testable : Index.CFunction.t Alcotest.testable =
  Alcotest.testable
    (fun fmt f -> Format.pp_print_string fmt @@ Index.CFunction.string_of_t f)
    Index.CFunction.equal
;;

let modular_index_test
      (type i)
      (module I : Index.S with type t = i)
      ~(index_description : string)
      create_index
      test
  =
  test (module I : Index.S with type t = i) ~index_description (create_index ())
;;

let modular_index_test_suite
      (type i)
      (module I : Index.S with type t = i)
      ~(index_description : string)
      create_index
      tests
  =
  List.map
    (modular_index_test
       (module I : Index.S with type t = i)
       ~index_description
       create_index)
    tests
;;

let test_index_store_get_single
      (type i)
      (module I : Index.S with type t = i)
      ~(index_description : string)
      (index : i)
  =
  ( Printf.sprintf
      "Storing a function then querying it's signature yields back the stored function \
       (%s)"
      index_description
  , fun () ->
      let cf =
        Index.CFunction.
          { name = "main"; signature = make_signature "int" [ "int"; "char**" ] }
      in
      I.store index [ cf ];
      let back = I.get index cf.signature in
      Alcotest.check
        (Alcotest.list cfunction_testable)
        "Expected to get back the single function stored"
        [ cf ]
        back )
;;

let test_index_empty_get
      (type i)
      (module I : Index.S with type t = i)
      ~(index_description : string)
      (index : i)
  =
  ( Printf.sprintf "Querying an empty index yields no result (%s)" index_description
  , fun () ->
      let back = I.get index @@ make_signature "void" [] in
      Alcotest.check (Alcotest.list cfunction_testable) "Expected no result" [] back )
;;

let test_index_no_match
      (type i)
      (module I : Index.S with type t = i)
      ~(index_description : string)
      (index : i)
  =
  ( Printf.sprintf
      "Storing a function then querying another signature yields no result (%s)"
      index_description
  , fun () ->
      let cf =
        Index.CFunction.
          { name = "main"; signature = make_signature "int" [ "int"; "char**" ] }
      in
      I.store index [ cf ];
      let back = I.get index @@ make_signature "void" [] in
      Alcotest.check (Alcotest.list cfunction_testable) "Expected no result" [] back )
;;

let test_index_sig_order_insignificant
      (type i)
      (module I : Index.S with type t = i)
      ~(index_description : string)
      (index : i)
  =
  ( Printf.sprintf "Search ignores parameters order (%s)" index_description
  , fun () ->
      let f_abc =
        Index.CFunction.
          [ { name = "f1"; signature = make_signature "int" [ "a"; "b"; "c" ] }
          ; { name = "f2"; signature = make_signature "int" [ "a"; "c"; "b" ] }
          ; { name = "f3"; signature = make_signature "int" [ "b"; "a"; "c" ] }
          ; { name = "f4"; signature = make_signature "int" [ "b"; "c"; "a" ] }
          ; { name = "f5"; signature = make_signature "int" [ "c"; "a"; "b" ] }
          ; { name = "f6"; signature = make_signature "int" [ "c"; "b"; "a" ] }
          ]
      and f_no_match =
        Index.CFunction.
          [ { name = "x1"; signature = make_signature "void" [ "a"; "b"; "c" ] }
          ; { name = "x2"; signature = make_signature "int" [ "a"; "b"; "d" ] }
          ; { name = "x3"; signature = make_signature "int" [] }
          ]
      in
      I.store index (f_abc @ f_no_match);
      let result = I.get index @@ make_signature "int" [ "a"; "b"; "c" ] in
      Alcotest.check
        (Alcotest.list cfunction_testable)
        "Expected all functions with queried parameter list regarless of parameters order"
        f_abc
        result )
;;

let tests_index =
  [ test_index_store_get_single
  ; test_index_empty_get
  ; test_index_no_match
  ; test_index_sig_order_insignificant
  ]
;;

let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l

let () =
  Alcotest.run "Signature"
  @@ suites
       [ ( "Parser combinators"
         , [ test_keyword_parsing; test_list_parsing; test_keyword_fail_parsing ] )
       ; ( "C/C++ signature parsing"
         , [ test_signature_void
           ; test_signature_int
           ; test_signature_one_param
           ; test_signature_two_param
           ; test_signature_pointer
           ; test_condense_pointer
           ; test_signature_const
           ; test_signature_unsigned
           ] )
       ; ( "Indexing (in-memory)"
         , modular_index_test_suite
             (module Index.InMemory)
             ~index_description:"In-memory"
             (fun () -> Index.InMemory.init ())
             tests_index )
       ; ( "Indexing (file-based)"
         , modular_index_test_suite
             (module Index.FileBased)
             ~index_description:"File-Based"
             (fun () -> Index.FileBased.init { file = "tmp.txt"; mode = Truncate })
             tests_index )
       ]
;;

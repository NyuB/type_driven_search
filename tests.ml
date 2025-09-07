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

let test_only_east_const =
  ( "~const t~ => t const"
  , fun () ->
      Alcotest.check
        (Alcotest.option signature_testable)
        "West const not recognized"
        None
        (Signature.parse "int(const char*)") )
;;

let test_const_is_not_a_type =
  ( "const is not a valid type"
  , fun () ->
      Alcotest.check
        (Alcotest.option signature_testable)
        "'const' should be rejected as a type"
        None
        (Signature.parse "const()") )
;;

let test_identifier_can_contain_number =
  signature_parsing_test "int32()" @@ make_signature "int32" []
;;

let test_identifier_cannot_start_with_number =
  ( "32int is not a valid type"
  , fun () ->
      Alcotest.check
        (Alcotest.option signature_testable)
        "'32int' should be rejected as a type"
        None
        (Signature.parse "32int()") )
;;

let test_varargs_parameter =
  signature_parsing_test "void(char const*, ...)"
  @@ make_signature "void" [ "char const*"; "..." ]
;;

let cfunction_testable : Index.CFunction.t Alcotest.testable =
  Alcotest.testable
    (fun fmt f -> Format.pp_print_string fmt @@ Index.CFunction.string_of_t f)
    Index.CFunction.equal
;;

let modular_index_test (type i) (module I : Index.S with type t = i) create_index test =
  test (module I : Index.S with type t = i) (create_index ())
;;

let modular_index_test_suite
      (type i)
      (module I : Index.S with type t = i)
      create_index
      tests
  =
  ( Printf.sprintf "Indexing (%s)" I.id
  , List.map (modular_index_test (module I : Index.S with type t = i) create_index) tests
  )
;;

let test_index_store_get_single (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf
      "Storing a function then querying it's signature yields back the stored function \
       (%s)"
      I.id
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

let test_index_store_three_get_two
      (type i)
      (module I : Index.S with type t = i)
      (index : i)
  =
  ( Printf.sprintf
      "Storing three functions, two having the same signature, retrieve both in one \
       query (%s)"
      I.id
  , fun () ->
      let shared_signature = make_signature "int" [ "int"; "int" ] in
      let fadd = Index.CFunction.{ name = "add"; signature = shared_signature }
      and fmul = Index.CFunction.{ name = "mul"; signature = shared_signature }
      and fmain =
        Index.CFunction.
          { name = "main"; signature = make_signature "int" [ "int"; "char**" ] }
      in
      I.store index [ fmain ];
      I.store index [ fadd ];
      I.store index [ fmul ];
      let back = I.get index shared_signature |> List.sort Index.CFunction.compare in
      Alcotest.check
        (Alcotest.list cfunction_testable)
        "Expected to get back the two functions having he queried signature"
        [ fadd; fmul ]
        back )
;;

let test_index_empty_get (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf "Querying an empty index yields no result (%s)" I.id
  , fun () ->
      let back = I.get index @@ make_signature "void" [] in
      Alcotest.check (Alcotest.list cfunction_testable) "Expected no result" [] back )
;;

let test_index_no_match (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf
      "Storing a function then querying another signature yields no result (%s)"
      I.id
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
      (index : i)
  =
  ( Printf.sprintf "Search ignores parameters order (%s)" I.id
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
      let result =
        I.get index @@ make_signature "int" [ "a"; "b"; "c" ]
        |> List.sort Index.CFunction.compare
      in
      Alcotest.check
        (Alcotest.list cfunction_testable)
        "Expected all functions with queried parameter list regardless of parameters \
         order"
        f_abc
        result )
;;

let test_index_interleaved_store (type i) (module I : Index.S with type t = i) (index : i)
  =
  ( Printf.sprintf "Search after interleaved storage (%s)" I.id
  , fun () ->
      let queried_signature = make_signature "int" [] in
      let f1 = Index.CFunction.{ name = "f1"; signature = queried_signature }
      and f2 = Index.CFunction.{ name = "f2"; signature = queried_signature }
      and f3 = Index.CFunction.{ name = "f3"; signature = queried_signature }
      and f4 = Index.CFunction.{ name = "f4"; signature = queried_signature } in
      let interleaved =
        Index.CFunction.
          [ f1
          ; { name = "x1"; signature = make_signature "void" [] }
          ; f2
          ; { name = "x2"; signature = make_signature "int" [ "a"; "b"; "d" ] }
          ; f3
          ; { name = "x3"; signature = make_signature "int" [ "x" ] }
          ; f4
          ]
      in
      I.store index interleaved;
      let result =
        I.get index @@ make_signature "int" [] |> List.sort Index.CFunction.compare
      in
      Alcotest.check
        (Alcotest.list cfunction_testable)
        "Expected all and only functions with queried signature"
        [ f1; f2; f3; f4 ]
        result )
;;

let test_index_respects_oracle (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf "Matches the reference implementation (%s)" I.id
  , fun () ->
      if
        (* Avoid redundant testing since this generates moult queries *)
        String.equal I.id Index.InMemory.id
      then ()
      else (
        let rand = Testability.reproducible_random () in
        let functions = Testability.pseudo_random_functions rand 500 3 3 in
        let mem_index = Index.InMemory.init () in
        I.store index functions;
        Index.InMemory.store mem_index functions;
        let queries =
          (* Pick queries among stored function to ensure there are matches *)
          Testability.pick_n rand 1000 functions |> List.map Index.CFunction.signature
        in
        queries
        |> List.iter (fun query ->
          let mem_results =
            Index.InMemory.get mem_index query |> List.sort Index.CFunction.compare
          in
          Alcotest.check
            (Alcotest.list cfunction_testable)
            "Actual result matches reference implementation result"
            mem_results
            (I.get index query |> List.sort Index.CFunction.compare))) )
;;

let tests_index =
  [ test_index_store_get_single
  ; test_index_empty_get
  ; test_index_store_three_get_two
  ; test_index_no_match
  ; test_index_sig_order_insignificant
  ; test_index_interleaved_store
  ; test_index_respects_oracle
  ]
;;

let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l
let temp_index_file () = Filename.temp_file ~temp_dir:"." "temp" ".txt"

let () =
  Alcotest.run "Type driven search"
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
           ; test_only_east_const
           ; test_const_is_not_a_type
           ; test_identifier_can_contain_number
           ; test_identifier_cannot_start_with_number
           ; test_varargs_parameter
           ] )
       ; modular_index_test_suite
           (module Index.InMemory)
           (fun () -> Index.InMemory.init ())
           tests_index
       ; modular_index_test_suite
           (module Index.FileBased)
           (fun () -> Index.FileBased.init { file = temp_index_file (); mode = Create })
           tests_index
       ; modular_index_test_suite
           (module Index.FileBasedSorted)
           (fun () ->
              Index.FileBasedSorted.init { file = temp_index_file (); mode = Create })
           tests_index
       ]
;;

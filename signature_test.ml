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
      (Signature.Parser.parse_full parser input)
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
      (Signature.Parser.parse_full parser input)
  in
  title, exec
;;

let test_keyword_parsing =
  parsing_test ~testable:Alcotest.unit "keyword" (Signature.Parser.keyword "kw") "kw" ()
;;

let test_list_parsing =
  let open Signature.Parser in
  let parser =
    list ~sep:(keyword ",") ~prefix:(keyword "(") ~suffix:(keyword ")") (keyword "kw")
  in
  parsing_test
    ~testable:(Alcotest.list Alcotest.unit)
    "keyword list"
    parser
    "(kw,kw,kw)"
    [ (); (); () ]
;;

let test_keyword_fail_parsing =
  parsing_fail_test
    ~testable:Alcotest.unit
    "keyword 'kw'"
    (Signature.Parser.keyword "kw")
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

let make_signature return params =
  Signature.
    { return = Signature.Ctype.parse return
    ; params = List.map Signature.Ctype.parse params
    }
;;

let test_signature_void = signature_parsing_test "void ()" @@ make_signature "void" []
let test_signature_int = signature_parsing_test "int ()" @@ make_signature "int" []

let test_signature_one_param =
  signature_parsing_test "int (char)" @@ make_signature "int" [ "char" ]
;;

let test_signature_two_param =
  signature_parsing_test "int (char, int)" @@ make_signature "int" [ "char"; "int" ]
;;

let test_signature_qualifier =
  signature_parsing_test "int (char**, int)" @@ make_signature "int" [ "char**"; "int" ]
;;

let test_condense_qualifier =
  signature_parsing_test "int (char * *, int)" @@ make_signature "int" [ "char**"; "int" ]
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
           ; test_signature_qualifier
           ; test_condense_qualifier
           ] )
       ]
;;

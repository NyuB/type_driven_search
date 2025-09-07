let signature_parsing_test input expected =
  let expected_repr = Signature.string_of_t expected in
  let title = Printf.sprintf "%s => %s" input expected_repr
  and exec =
    fun () ->
    Alcotest.check
      (Alcotest.option Testability.signature_testable)
      "Expected a successfull parsing"
      (Some expected)
      (Signature.parse input)
  in
  title, exec
;;

let test_signature_void =
  signature_parsing_test "void ()" @@ Testability.make_signature "void" []
;;

let test_signature_int =
  signature_parsing_test "int ()" @@ Testability.make_signature "int" []
;;

let test_signature_one_param =
  signature_parsing_test "int (char)" @@ Testability.make_signature "int" [ "char" ]
;;

let test_signature_two_param =
  signature_parsing_test "int (char, int)"
  @@ Testability.make_signature "int" [ "char"; "int" ]
;;

let test_signature_pointer =
  signature_parsing_test "int (char**, int)"
  @@ Testability.make_signature "int" [ "char**"; "int" ]
;;

let test_condense_pointer =
  signature_parsing_test "int (char * *, int)"
  @@ Testability.make_signature "int" [ "char**"; "int" ]
;;

let test_signature_const =
  signature_parsing_test "void (char const*)"
  @@ Testability.make_signature "void" [ "char const*" ]
;;

let test_signature_unsigned =
  signature_parsing_test "void (unsigned int)"
  @@ Testability.make_signature "void" [ "unsigned int" ]
;;

let test_only_east_const =
  ( "~const t~ => t const"
  , fun () ->
      Alcotest.check
        (Alcotest.option Testability.signature_testable)
        "West const not recognized"
        None
        (Signature.parse "int(const char*)") )
;;

let test_const_is_not_a_type =
  ( "const is not a valid type"
  , fun () ->
      Alcotest.check
        (Alcotest.option Testability.signature_testable)
        "'const' should be rejected as a type"
        None
        (Signature.parse "const()") )
;;

let test_identifier_can_contain_number =
  signature_parsing_test "int32()" @@ Testability.make_signature "int32" []
;;

let test_identifier_cannot_start_with_number =
  ( "32int is not a valid type"
  , fun () ->
      Alcotest.check
        (Alcotest.option Testability.signature_testable)
        "'32int' should be rejected as a type"
        None
        (Signature.parse "32int()") )
;;

let test_varargs_parameter =
  signature_parsing_test "void(char const*, ...)"
  @@ Testability.make_signature "void" [ "char const*"; "..." ]
;;

let declaration_parsing_test input expected =
  ( Printf.sprintf "%s is parsed correctly" input
  , fun () ->
      Alcotest.check
        (Alcotest.option Testability.cfunction_testable)
        "Expected a successfull parsing"
        (Some expected)
        (Signature.CFunction.parse input) )
;;

let test_most_famous_declaration =
  declaration_parsing_test
    "int main(int, char**);"
    { name = "main"; signature = Testability.make_signature "int" [ "int"; "char**" ] }
;;

let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l

let () =
  Alcotest.run "C/C++ signature"
  @@ suites
       [ ( "Signature parsing"
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
       ; "Declaration parsing", [ test_most_famous_declaration ]
       ]
;;

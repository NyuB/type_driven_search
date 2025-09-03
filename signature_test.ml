let signature_testable : Signature.t Alcotest.testable =
  Alcotest.testable
    (fun fmt signature -> Format.pp_print_string fmt @@ Signature.string_of_t signature)
    Signature.equal
;;

let parsing_test input expected =
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

let test_parse_void = parsing_test "void ()" Signature.{ return = "void"; params = [] }
let test_parse_int = parsing_test "int ()" Signature.{ return = "int"; params = [] }

let test_parse_one_param =
  parsing_test "int (char)" Signature.{ return = "int"; params = [ "char" ] }
;;

let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l

let () =
  Alcotest.run "Signature"
  @@ suites [ "Parse", [ test_parse_void; test_parse_int; test_parse_one_param ] ]
;;

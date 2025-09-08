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

let test_keyword_ok =
  parsing_test ~testable:Alcotest.string "keyword" (Parsers.keyword "kw") "kw" "kw"
;;

let test_list_ok =
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

let test_whitespaces_ok =
  let open Parsers in
  let parser = keyword "begin" |. whitespaces |* keyword "end" in
  parsing_test
    ~testable:(Alcotest.pair Alcotest.string Alcotest.string)
    "whitespaces"
    parser
    "begin\n \tend"
    ("begin", "end")
;;

let test_keyword_ko =
  parsing_fail_test
    ~testable:Alcotest.string
    "keyword 'kw'"
    (Parsers.keyword "kw")
    "notkw"
;;

let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l

let () =
  Alcotest.run "Parser combinators"
  @@ suites
       [ "Success", [ test_keyword_ok; test_list_ok; test_whitespaces_ok ]
       ; "Failure", [ test_keyword_ko ]
       ]
;;

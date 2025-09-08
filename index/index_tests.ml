let test_index_store_get_single (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf
      "Storing a function then querying it's signature yields back the stored function \
       (%s)"
      I.id
  , fun () ->
      let cf =
        Signature.CFunction.
          { name = "main"
          ; signature = Testability.make_signature "int" [ "int"; "char**" ]
          }
      in
      I.store index [ cf ];
      let back = I.get index cf.signature in
      Alcotest.check
        (Alcotest.list Testability.cfunction_testable)
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
      let shared_signature = Testability.make_signature "int" [ "int"; "int" ] in
      let fadd = Signature.CFunction.{ name = "add"; signature = shared_signature }
      and fmul = Signature.CFunction.{ name = "mul"; signature = shared_signature }
      and fmain =
        Signature.CFunction.
          { name = "main"
          ; signature = Testability.make_signature "int" [ "int"; "char**" ]
          }
      in
      I.store index [ fmain ];
      I.store index [ fadd ];
      I.store index [ fmul ];
      let back = I.get index shared_signature |> List.sort Signature.CFunction.compare in
      Alcotest.check
        (Alcotest.list Testability.cfunction_testable)
        "Expected to get back the two functions having he queried signature"
        [ fadd; fmul ]
        back )
;;

let test_index_empty_get (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf "Querying an empty index yields no result (%s)" I.id
  , fun () ->
      let back = I.get index @@ Testability.make_signature "void" [] in
      Alcotest.check
        (Alcotest.list Testability.cfunction_testable)
        "Expected no result"
        []
        back )
;;

let test_index_no_match (type i) (module I : Index.S with type t = i) (index : i) =
  ( Printf.sprintf
      "Storing a function then querying another signature yields no result (%s)"
      I.id
  , fun () ->
      let cf =
        Signature.CFunction.
          { name = "main"
          ; signature = Testability.make_signature "int" [ "int"; "char**" ]
          }
      in
      I.store index [ cf ];
      let back = I.get index @@ Testability.make_signature "void" [] in
      Alcotest.check
        (Alcotest.list Testability.cfunction_testable)
        "Expected no result"
        []
        back )
;;

let test_index_sig_order_insignificant
      (type i)
      (module I : Index.S with type t = i)
      (index : i)
  =
  ( Printf.sprintf "Search ignores parameters order (%s)" I.id
  , fun () ->
      let f_abc =
        Signature.CFunction.
          [ { name = "f1"
            ; signature = Testability.make_signature "int" [ "a"; "b"; "c" ]
            }
          ; { name = "f2"
            ; signature = Testability.make_signature "int" [ "a"; "c"; "b" ]
            }
          ; { name = "f3"
            ; signature = Testability.make_signature "int" [ "b"; "a"; "c" ]
            }
          ; { name = "f4"
            ; signature = Testability.make_signature "int" [ "b"; "c"; "a" ]
            }
          ; { name = "f5"
            ; signature = Testability.make_signature "int" [ "c"; "a"; "b" ]
            }
          ; { name = "f6"
            ; signature = Testability.make_signature "int" [ "c"; "b"; "a" ]
            }
          ]
      and f_no_match =
        Signature.CFunction.
          [ { name = "x1"
            ; signature = Testability.make_signature "void" [ "a"; "b"; "c" ]
            }
          ; { name = "x2"
            ; signature = Testability.make_signature "int" [ "a"; "b"; "d" ]
            }
          ; { name = "x3"; signature = Testability.make_signature "int" [] }
          ]
      in
      I.store index (f_abc @ f_no_match);
      let result =
        I.get index @@ Testability.make_signature "int" [ "a"; "b"; "c" ]
        |> List.sort Signature.CFunction.compare
      in
      Alcotest.check
        (Alcotest.list Testability.cfunction_testable)
        "Expected all functions with queried parameter list regardless of parameters \
         order"
        f_abc
        result )
;;

let test_index_interleaved_store (type i) (module I : Index.S with type t = i) (index : i)
  =
  ( Printf.sprintf "Search after interleaved storage (%s)" I.id
  , fun () ->
      let queried_signature = Testability.make_signature "int" [] in
      let f1 = Signature.CFunction.{ name = "f1"; signature = queried_signature }
      and f2 = Signature.CFunction.{ name = "f2"; signature = queried_signature }
      and f3 = Signature.CFunction.{ name = "f3"; signature = queried_signature }
      and f4 = Signature.CFunction.{ name = "f4"; signature = queried_signature } in
      let interleaved =
        Signature.CFunction.
          [ f1
          ; { name = "x1"; signature = Testability.make_signature "void" [] }
          ; f2
          ; { name = "x2"
            ; signature = Testability.make_signature "int" [ "a"; "b"; "d" ]
            }
          ; f3
          ; { name = "x3"; signature = Testability.make_signature "int" [ "x" ] }
          ; f4
          ]
      in
      I.store index interleaved;
      let result =
        I.get index @@ Testability.make_signature "int" []
        |> List.sort Signature.CFunction.compare
      in
      Alcotest.check
        (Alcotest.list Testability.cfunction_testable)
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
          Testability.pick_n rand 1000 functions |> List.map Signature.CFunction.signature
        in
        queries
        |> List.iter (fun query ->
          let mem_results =
            Index.InMemory.get mem_index query |> List.sort Signature.CFunction.compare
          in
          Alcotest.check
            (Alcotest.list Testability.cfunction_testable)
            "Actual result matches reference implementation result"
            mem_results
            (I.get index query |> List.sort Signature.CFunction.compare))) )
;;

let test_index_query_void_params (type i) (module I : Index.S with type t = i) (index : i)
  =
  ( Printf.sprintf "Querying 't()' yields all entries with return type 't' (%s)" I.id
  , fun () ->
      if not @@ String.equal I.id "TOOD"
      then ()
      else (
        let fun_returning_t =
          Signature.CFunction.
            [ { name = "fa"; signature = Testability.make_signature "t" [] }
            ; { name = "fb"; signature = Testability.make_signature "t" [ "p1" ] }
            ; { name = "fc"; signature = Testability.make_signature "t" [ "p1"; "p2" ] }
            ]
        and fun_not_returning_t =
          Signature.CFunction.
            [ { name = "xa"; signature = Testability.make_signature "u" [] }
            ; { name = "xb"; signature = Testability.make_signature "u" [ "t" ] }
            ]
        in
        I.store index (fun_returning_t @ fun_not_returning_t);
        let results =
          I.query index (Testability.make_signature "t" [])
          |> List.sort Signature.CFunction.compare
        in
        Alcotest.check
          (Alcotest.list Testability.cfunction_testable)
          "Expected all functions returning 't'"
          fun_returning_t
          results) )
;;

let tests_index =
  [ test_index_store_get_single
  ; test_index_empty_get
  ; test_index_store_three_get_two
  ; test_index_no_match
  ; test_index_sig_order_insignificant
  ; test_index_interleaved_store
  ; test_index_respects_oracle
  ; test_index_query_void_params
  ]
;;

let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l

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

let temp_index_file () = Filename.temp_file ~temp_dir:"." "temp" ".txt"

let () =
  Alcotest.run "C functions indexed stores"
  @@ suites
       [ modular_index_test_suite
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

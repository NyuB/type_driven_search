let test (name, exec) = Alcotest.test_case name `Quick exec
let suite (name, tests) = name, List.map test tests
let suites l = List.map suite l
let seed = [| 28; 01; 1998 |]
let rand = Random.State.make seed

let random_find_test size min max rand =
  let module IntSet = Set.Make (Int) in
  let arr = Array.init size (fun _ -> Random.State.int_in_range rand ~min ~max) in
  let arr =
    Array.fold_left (fun set i -> IntSet.add i set) IntSet.empty arr
    |> IntSet.to_seq
    |> Array.of_seq
  in
  Array.sort Int.compare arr;
  ( Printf.sprintf "Find among %d elements in range [%d..%d[" (Array.length arr) min max
  , fun () ->
      let expected = Array.mapi (fun idx elem -> elem, Some idx) arr in
      let actual =
        Array.map
          (fun elem -> elem, Binary_search.index size (fun i -> Int.compare arr.(i) elem))
          arr
      in
      let testable = Alcotest.pair Alcotest.int (Alcotest.option Alcotest.int) in
      for i = 0 to Array.length arr - 1 do
        Alcotest.check testable "Expected a correct sort" expected.(i) actual.(i)
      done )
;;

let () =
  Alcotest.run "Binary search"
  @@ suites
       [ "Insertion", []
       ; "Find", [ random_find_test 1 (-1) 1 rand; random_find_test 100 0 999 rand ]
       ]
;;

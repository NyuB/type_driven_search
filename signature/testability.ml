(** Helpers to manipulate and test the Signature module *)

let pseudo_random_name rand prefix =
  let length = 3 + Random.State.int rand 14 in
  prefix
  ^ String.init length (fun _ -> Random.State.int rand 26 + Char.code 'a' |> Char.chr)
;;

let pseudo_random_type rand among =
  let n = Random.State.int rand among in
  Printf.sprintf "t_%d" n |> Signature.Ctype.atom
;;

let pseudo_random_signature rand type_count max_params : Signature.t =
  let params_count = Random.State.int rand max_params
  and return = pseudo_random_type rand type_count in
  let params = List.init params_count (fun _ -> pseudo_random_type rand 10) in
  { return; params }
;;

let pseudo_random_function rand type_count max_params : Signature.CFunction.t =
  let name = pseudo_random_name rand "f_"
  and signature = pseudo_random_signature rand type_count max_params in
  { name; signature }
;;

let pseudo_random_functions rand n type_count max_params : Signature.CFunction.t list =
  List.init n (fun _ -> pseudo_random_function rand type_count max_params)
;;

let pick_n rand n l =
  let random_access = Array.of_list l in
  List.init n (fun _ ->
    random_access.(Random.State.int rand @@ Array.length random_access))
;;

let reproducible_random () = Random.State.make [| 1; 2; 3; 5; 8 |]

let make_signature ret ps =
  Signature.
    { return = Signature.Ctype.parse ret; params = List.map Signature.Ctype.parse ps }
;;

(** Alcotest specifics *)
let signature_testable : Signature.t Alcotest.testable =
  Alcotest.testable
    (fun fmt signature -> Format.pp_print_string fmt @@ Signature.string_of_t signature)
    Signature.equal
;;

let cfunction_testable : Signature.CFunction.t Alcotest.testable =
  Alcotest.testable
    (fun fmt f -> Format.pp_print_string fmt @@ Signature.CFunction.string_of_t f)
    Signature.CFunction.equal
;;

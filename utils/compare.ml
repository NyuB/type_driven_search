let compare_by (comparison : 't -> 't -> int) how_to_get_comparable a b =
  comparison (how_to_get_comparable a) (how_to_get_comparable b)
;;

let compare_by_each comparisons (a : 't) (b : 't) =
  List.fold_left
    (fun compared comparison -> if compared != 0 then compared else comparison a b)
    0
    comparisons
;;

let equality comparison a b = comparison a b = 0

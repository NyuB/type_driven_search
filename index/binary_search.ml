(** A generic implementation of dichotomy 'binary search' for sorted 0-based indexed sequences *)

(** @return a suitable index to insert a new element implicitly defined by [compare]
@param compare [compare i] must return 
- [0] if the element at index [i] is equal to the element to insert
-[< 0] if the element at index [i] is {b less than} the element to insert
-[> 0] if the element at index [i] is {b greater} than the element to insert
*)
let insertion_index count compare =
  if count = 0
  then 0
  else (
    let high = count - 1 in
    if compare 0 >= 0
    then 0
    else if compare high <= 0
    then high + 1
    else (
      let rec loop low high =
        assert (high >= low);
        if low == high
        then low
        else if low = high - 1
        then high
        else (
          let middle = low + ((high - low) / 2) in
          let compared = compare middle in
          if compared = 0
          then middle
          else if compared > 0
          then (
            assert (middle != high);
            loop low middle)
          else (
            assert (middle != low);
            loop middle high))
      in
      loop 0 high))
;;

(** @return the index of the element implicitly defined by [compare] or [None] if no such element exists
@param compare [compare i] must return 
- [0] if the element at index [i] is equal to the element to insert
-[< 0] if the element at index [i] is {b less than} the element to insert
-[> 0] if the element at index [i] is {b greater} than the element to insert
*)
let index count compare =
  let rec loop low high =
    if low >= high
    then None
    else (
      let middle = low + ((high - low) / 2) in
      let compared = compare middle in
      if compared = 0
      then Some middle
      else if compared > 0
      then loop low middle
      else loop (middle + 1) high)
  in
  loop 0 count
;;

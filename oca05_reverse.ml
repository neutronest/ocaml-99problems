let rec myreverse l =
  let rec iter acc = function
    | [] -> acc
    | (h::t) -> iter (h::acc) t
  in iter [] l

let () =
  let res = myreverse [1;2;3;4;5;6;7] in
  List.iter (Printf.printf "%d ") res;
  Printf.printf "\n";;


(* Note point: use a more complex but effective method  *)

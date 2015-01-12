let myreplicate l =
  let rec iter acc = function
    | [] -> acc
    | (h::t) -> iter (h::h::acc) t
  in List.rev (iter [] l);;

let () =
  let res = myreplicate  ["a";"b";"c";"c";"d"] in
  List.iter (Printf.printf "%s ") res;;

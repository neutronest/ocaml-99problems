type 'a node =
  | One of 'a
  | Many of 'a node list

let rec nested_flatten l =
  let rec iter acc = function
    | [] -> acc
    | One h :: t -> iter (h::acc) t
    | Many h :: t -> iter (iter acc h) t in
  List.rev (iter [] l);;

let () =
  let res = nested_flatten [ One 1 ; Many [ One 2 ; Many [ One 3 ; One 4 ] ; One 5 ] ] in
  List.iter (Printf.printf "%i ") res;
  Printf.printf "\n"
